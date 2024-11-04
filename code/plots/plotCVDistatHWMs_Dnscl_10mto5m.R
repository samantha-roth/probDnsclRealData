rm(list=ls())

setwd("C:/Users/svr5482")
library(ggplot2)
library(terra)

#load the bilinearly interpolated AND SHIFTED 10m projections at the HWM locations
load("C:/Users/svr5482/probDnsclRealData/data/downscale10mto5mAtHWMs.RData")

downscale10mAtHWMs<- downscale10m; rm(downscale10m)

#load the high water marks
load("probDnsclRealData/data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]
obs<- HWMs.df$height[1:5]

#get 5m preds at HWM locations
run5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/PostMedCh/Extent/Run_1.asc")
preds5matHWMs<- c(as.matrix(extract(run5m,HWMlocs)))

run10m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs10m/Norristown/nCh/PostMedCh/Extent/Run_1.asc")
preds10matHWMs<- c(as.matrix(extract(run10m,HWMlocs)))

#load the variance
load("probDnsclRealData/data/s2CVdownscale10mto5m.RData")

obsMinusDnsclPred10m<- obs- downscale10mAtHWMs

################################################################################
#plot for all locations
#location j

for(j in 1:length(obs)){
  ptsToAddToMean<- seq(-6,6,length.out=600)
  
  normDensBest<- dnorm(obsMinusDnsclPred10m[j]+ptsToAddToMean,
                       mean=obsMinusDnsclPred10m[j],sd= sqrt(s2CV[j]))
  
  normDensBest.df<- data.frame("y"=normDensBest,
                               "x"=downscale10mAtHWMs[j]+ptsToAddToMean)
  
  minx<- downscale10mAtHWMs[j]+ptsToAddToMean[10]
  
  filename<- paste0("probDnsclRealData/plots/CVDensityatHWM",j,"_10mto5m.jpeg")
  jpeg(file = filename,width = 700,height=600)
  print(ggplot(normDensBest.df, aes(x=x, y=y)) + geom_line() +
          geom_vline(xintercept=obs[j], color = "red", linewidth=2) +
          geom_vline(xintercept=preds5matHWMs[j], linetype="dashed", color = "blue", linewidth=2) +
          geom_vline(xintercept=preds10matHWMs[j], linetype="dotted", color = "cyan", linewidth=2) +
          #geom_vline(xintercept=downscale10mAtHWMs[j], linetype="dotted", color = "black") +
          ggtitle(paste0("Probabilistic Downscaling Performance at Observation #",j))+
          xlab("Flood Height (m)")+ ylab("Probability Density")+
          xlim(0,4) +
          geom_text(x=3.5, y=.7, size =8, label="Observation", color= "red")+
          geom_text(x=3.5,y=.8, size =8, label="High Resolution", color= "blue")+
          geom_text(x=3.5,y=.9, size =8, label="Downscaled", color= "black")+
          geom_text(x=3.5,y=1, size =8, label="Low Resolution", color= "cyan")+
          theme_bw() + 
          theme(plot.title = element_text(size=24), 
                axis.title = element_text(size=24),
                axis.text = element_text(size = 20),
                legend.text= element_text(size=24),
                legend.title= element_text(size=24)))
  dev.off()
}

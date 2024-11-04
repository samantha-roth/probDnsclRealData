rm(list=ls())

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

library(ggplot2)
library(terra)

#load the bilinearly interpolated AND SHIFTED 10m projections at the HWM locations
load("data/downscale10mto5mAtHWMs.RData")

downscale10mAtHWMs<- downscale10m; rm(downscale10m)

#load the high water marks
load("data/HWMsdf.RData")
HWMlocs<- as.matrix(HWMs.df[,c("x","y")]); HWMlocs<- HWMlocs[1:5,]
obs<- HWMs.df$height[1:5]

#get 5m preds at HWM locations
run5m<- rast("data/Outputs5m/Run_1.asc")
preds5matHWMs<- c(as.matrix(extract(run5m,HWMlocs)))

run10m<- rast("data/Outputs10m/Run_1.asc")
preds10matHWMs<- c(as.matrix(extract(run10m,HWMlocs)))

#load the variance
load("data/varResHWM10mto5m.RData")

obsMinusDnsclPred10m<- obs- downscale10mAtHWMs

################################################################################
#plot for all locations
#location j

for(j in 1:length(obs)){
  ptsToAddToMean<- seq(-6,6,length.out=600)
  
  normDensBest<- dnorm(obsMinusDnsclPred10m[j]+ptsToAddToMean,
                       mean=obsMinusDnsclPred10m[j],sd= sqrt(varResHWM10m))
  
  normDensBest.df<- data.frame("y"=normDensBest,
                               "x"=downscale10mAtHWMs[j]+ptsToAddToMean)
  
  minx<- downscale10mAtHWMs[j]+ptsToAddToMean[10]
  
  filename<- paste0("plots/DensityatHWM",j,"_10mto5m.jpeg")
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


################################################################################
#now just have text labels on the first one

j=1

ptsToAddToMean<- seq(-6,6,length.out=600)

normDensBest<- dnorm(obsMinusDnsclPred10m[j]+ptsToAddToMean,
                     mean=obsMinusDnsclPred10m[j],sd= sqrt(varResHWM10m))

normDensBest.df<- data.frame("y"=normDensBest,
                             "x"=downscale10mAtHWMs[j]+ptsToAddToMean)

minx<- downscale10mAtHWMs[j]+ptsToAddToMean[10]

filename<- paste0("plots/NL_DensityatHWM",j,"_10mto5m.jpeg")
jpeg(file = filename,width = 700,height=600)
print(ggplot(normDensBest.df, aes(x=x, y=y)) + geom_line(color="blue",linewidth=1.5) +
        #geom_vline(xintercept=obs[j], color = "red", linewidth=2) +
        geom_segment(aes(x = preds5matHWMs[j], y =0, xend = preds5matHWMs[j], yend = 2.6), linetype="dashed", colour="black", lwd = 2)+
        geom_segment(aes(x = preds10matHWMs[j], y = 0, xend = preds10matHWMs[j], yend = 2.6), linetype="dotted", colour="red", lwd = 2)+
        #geom_vline(xintercept=preds5matHWMs[j], linetype="dashed", color = "black", linewidth=2) +
        #geom_vline(xintercept=preds10matHWMs[j], linetype="dotted", color = "red", linewidth=2) +
        #geom_vline(xintercept=downscale10mAtHWMs[j], linetype="dotted", color = "black") +
        #ggtitle(paste0("Probabilistic Downscaling Performance at Observation #",j))+
        xlab("Flood Height (m)")+ ylab("Probability Density")+
        #xlim(0,3) + ylim(0,2.5) +
        scale_x_continuous(limits = c(0, 3), expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, 2.6), expand = c(0, 0)) +
        #geom_text(x=3.4, y=.7, size =9, label="Observation", color= "red")+
        geom_text(x=2.1,y=.75, size =15, label="High Resolution", color= "black")+
        geom_text(x=2.1,y=.95, size =15, label="Downscaled", color= "blue")+
        geom_text(x=2.1,y=1.15, size =15, label="Low Resolution", color= "red")+
        #theme_bw() + 
        theme(plot.title = element_text(size=28), 
              axis.title = element_text(size=35),
              axis.text = element_text(size=35),
              legend.text= element_text(size=28),
              legend.title= element_text(size=28),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black")))
dev.off()


for(j in 2:length(obs)){
  ptsToAddToMean<- seq(-6,6,length.out=600)
  
  normDensBest<- dnorm(obsMinusDnsclPred10m[j]+ptsToAddToMean,
                       mean=obsMinusDnsclPred10m[j],sd= sqrt(varResHWM10m))
  
  normDensBest.df<- data.frame("y"=normDensBest,
                               "x"=downscale10mAtHWMs[j]+ptsToAddToMean)
  
  minx<- downscale10mAtHWMs[j]+ptsToAddToMean[10]
  
  filename<- paste0("plots/NL_DensityatHWM",j,"_10mto5m.jpeg")
  jpeg(file = filename,width = 600,height=600)
  print(ggplot(normDensBest.df, aes(x=x, y=y)) + geom_line(color="blue",linewidth=1.5) +
          #geom_vline(xintercept=obs[j], color = "red", linewidth=2) +
          geom_segment(aes(x = preds5matHWMs[j], y =0, xend = preds5matHWMs[j], yend = 2.6), linetype="dashed", colour="black", lwd = 2)+
          geom_segment(aes(x = preds10matHWMs[j], y = 0, xend = preds10matHWMs[j], yend = 2.6), linetype="dotted", colour="red", lwd = 2)+
          #geom_vline(xintercept=preds5matHWMs[j], linetype="dashed", color = "black", linewidth=2) +
          #geom_vline(xintercept=preds10matHWMs[j], linetype="dotted", color = "red", linewidth=2) +
          #geom_vline(xintercept=downscale10mAtHWMs[j], linetype="dotted", color = "black") +
          #ggtitle(paste0("Probabilistic Downscaling Performance at Observation #",j))+
          xlab("Flood Height (m)")+ ylab("Probability Density")+
          #xlim(0,3) + ylim(0,2.5) +
          scale_x_continuous(limits = c(0, 3), expand = c(0, 0)) +
          scale_y_continuous(limits = c(0, 2.6), expand = c(0, 0)) +
          #theme_bw() + 
          theme(plot.title = element_text(size=28), 
                axis.title.x= element_text(size=35),
                axis.text.x = element_text(size=35),
                legend.text= element_text(size=28),
                legend.title= element_text(size=28),
                axis.text.y = element_blank(),  # Remove y-axis text (numbers)
                axis.ticks.y = element_blank(), # Remove y-axis ticks
                axis.title.y = element_blank(),  # Remove y-axis label
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black")))
  dev.off()
}


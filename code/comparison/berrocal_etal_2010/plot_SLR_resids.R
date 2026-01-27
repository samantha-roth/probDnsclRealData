rm(list=ls())
graphics.off()

setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

#setwd("/Users/f007f8t/Documents/GitHub/probDnsclRealData")

library(terra)
library(ggplot2)

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

#load the residuals from regressing the high water marks on the low resolution flood heights
load("data/BayesianSLR_resids")


preds_obs_locs.df<- data.frame("obs"=obs,
                        "preds10m"=preds10matHWMs,
                        "preds5m"=preds5matHWMs,
                        "downscale10mto5m"=downscale10mAtHWMs,
                        "SLR_resids"= SLR_resids,
                        "x"=HWMlocs[,"x"],
                        "y"=HWMlocs[,"y"])

filename<- paste0("plots/obsInSpace.jpeg")
jpeg(file = filename,width = 700,height=450)
print(ggplot(data=preds_obs_locs.df,aes(x=x,y=y,color=obs))+
  geom_point(size=5) + theme_bw() + labs(color= "flood height (m)") +
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size = 20),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24)))
dev.off()

filename<- paste0("plots/ResidsInSpace.jpeg")
jpeg(file = filename,width = 700,height=450)
print(ggplot(data=preds_obs_locs.df,aes(x=x,y=y,color=SLR_resids))+
        geom_point(size=5) + theme_bw() + labs(color= "residual (m)") +
        theme(plot.title = element_text(size=24), 
              axis.title = element_text(size=24),
              axis.text = element_text(size = 20),
              legend.text= element_text(size=24),
              legend.title= element_text(size=24)))
dev.off()

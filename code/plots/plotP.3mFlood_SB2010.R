#get sensitivity and specificity in wet low resolution cell area

rm(list=ls())

# dir<- commandArgs(trailingOnly=TRUE)
# setwd(dir)

setwd("/Volumes/RothS/probDnsclRealData_Normal")
# setwd("/dartfs/rc/lab/R/RothS/probDnsclRealData_Normal")

library(terra); library(RColorBrewer)

run5m<- rast("data/Outputs5m/Run_1.asc")
run10m<- rast("data/Outputs10m/Run_1.asc")
coords.5m<- xyFromCell(run5m,1:ncell(run5m))

################################################################################
#load flooded locations at the two lower resolutions being downscaled
load("data/floodInds10mat5mAroundHWMs.RData")

#load region of interest coordinates
load("data/boxAroundHWMs.5m.RData")

#load calibrated 5m flood projections in region of interest
load("data/vals5minBdsAroundHWMs.RData")

#downscaled flood projections using SB2010
#load the thinned posterior samples from SLR fit via MCMC
load("data/mcmc.output1_thinned")

wsh.10m<- c(as.matrix(extract(run10m,coordsinBds.5m)))

SLR_mcmc_pred_func<- function(step){
  return(wsh.10m*beta1_thin[step] + beta0_thin[step])
}

#get downscaled values using the Bayesian SLR model
downscale_vals<- matrix(NA, nrow=length(thin_inds), ncol= length(wsh.10m))
for(i in 1:length(thin_inds)){
  downscale_vals[i,]<- SLR_mcmc_pred_func(i)
}

downscale_vals[which(downscale_vals<0)]<- 0

#load cells of interest outside the low res flooded cells
load("data/destInds10mat5m.RData")

length(which(good5minds%in%good5minds[c(destIndsinGoodInds10m,floodInds10mat5m)]))

################################################################################
#get flooded and not flooded locations within the business region of interest
floodInds<- good5minds[floodInds10mat5m]


################################################################################

pFlood<- apply(downscale_vals,2,function(x) length(which(x>0.3))/length(x)) 

################################################################################

library(RColorBrewer)
library(ggmap)
library(osmdata)
library(ggplot2)

#define spatial extent using a bounding box (left, right, bottom, top)
bbox_to_keep <- c(min(coordsinBds.5m[,1])-1, max(coordsinBds.5m[,1])+1, 
                  min(coordsinBds.5m[,2])-1, max(coordsinBds.5m[,2])+1)
extent_to_keep <- ext(bbox_to_keep)

#subset the raster based on the defined extent
run5mDest <- crop(run5m, extent_to_keep)
coordsinBds.5mBox<- xyFromCell(run5mDest,1:ncell(run5mDest))

#all good
min(coordsinBds.5m[,1]); max(coordsinBds.5m[,1])
min(coordsinBds.5mBox[,1]); max(coordsinBds.5mBox[,1])

min(coordsinBds.5m[,2]); max(coordsinBds.5m[,2])
min(coordsinBds.5mBox[,2]); max(coordsinBds.5mBox[,2])

corners<- matrix(c(min(coordsinBds.5m[,1])-1,min(coordsinBds.5m[,2])-1,
                   min(coordsinBds.5m[,1])-1,max(coordsinBds.5m[,2])+1,
                   max(coordsinBds.5m[,1])+1,min(coordsinBds.5m[,2])-1,
                   max(coordsinBds.5m[,1])+1,max(coordsinBds.5m[,2])+1),
                 ncol=2,nrow=4,byrow=TRUE)

#Produce map of Norristown in the business area of interest
v <- vect(corners, crs="+proj=utm +zone=18 +datum=WGS84  +units=m")
y <- terra::project(v, "+proj=longlat +datum=WGS84")

lonlat <- geom(y)[, c("x", "y")]
lonlat

max.lat<- max(lonlat[,2])
min.lat<- min(lonlat[,2])
max.lon<- max(lonlat[,1])
min.lon<- min(lonlat[,1])

################################################################################
#create a map of norristown at the region of interest

Norristown_bb<- matrix(c(min.lon,max.lon,min.lat,max.lat),
                       nrow=2,ncol=2,byrow = TRUE)
colnames(Norristown_bb)<- c("min","max")
rownames(Norristown_bb)<- c("x","y")

Norristown_major <- Norristown_bb %>%
  opq(timeout = 10) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary")) %>%
  osmdata_sf()

# Create the plot object, using the osm_lines element of Philadelphia_major
Norristown_plot <- ggplot() + 
  xlim(min.lon,max.lon) + 
  ylim(min.lat,max.lat) 

################################################################################
#what locations are within the box?
v_all <- vect(coordsinBds.5m, crs="+proj=utm +zone=18 +datum=WGS84  +units=m")
y_all <- terra::project(v_all, "+proj=longlat +datum=WGS84")

lonlat_all <- geom(y_all)[, c("x", "y")]

df<- data.frame("flooding"= pFlood,
                "latitude"= lonlat_all[,2],
                "longitude"=lonlat_all[,1])

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 1))

filename<- "plots/pflood.3m_SB2010_QGIS.jpeg"
jpeg(file = filename,width = 690,height=350)
Norristown_plot +
  geom_point(data = df, alpha = 0.5, shape=15, size= 2,
             mapping = aes(x = longitude, y = latitude, 
                           color=flooding))+ 
  scale_color_gradient(low="yellow", high="blue",limits=c(0,1)) +
  ggtitle("SB2010") +
  theme_bw()+
  theme(plot.title = element_text(size=24), 
        axis.title = element_text(size=24),
        axis.text = element_text(size=24),
        legend.text= element_text(size=24),
        legend.title= element_text(size=24),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        legend.position = "right") + coord_flip()
dev.off()

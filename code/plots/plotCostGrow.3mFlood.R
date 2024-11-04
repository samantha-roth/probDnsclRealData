#plot costgrow for the same region I evaluate my approach for


rm(list=ls())

library(terra)

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

run5m<- rast("data/Outputs5m/Run_1.asc")
coords.5m<- xyFromCell(run5m,1:ncell(run5m))

costgrow_WSE<- rast("data/costgrow10mto5m_methodArea1.tif")

dem5m<- rast("data/norristown_5m.asc")
elev5m<- values(dem5m)

coords.costgrow<- xyFromCell(costgrow_WSE,1:ncell(costgrow_WSE))

coords.dem5m<- xyFromCell(dem5m,1:ncell(dem5m))

cgvals10mto5m<- values(costgrow_WSE)

wshCostGrow10mto5mVals<- cgvals10mto5m-elev5m

wshCostGrow10mto5mVals[which(is.na(wshCostGrow10mto5mVals))]<-0

wshCostGrow10mto5m<- dem5m
values(wshCostGrow10mto5m)<- wshCostGrow10mto5mVals

################################################################################
#crop to area of interest and compare to 5m projections

#load region of interest coordinates
load("data/boxAroundHWMs.5m.RData")

wshCostGrow<- c(as.matrix(extract(wshCostGrow10mto5m,coordsinBds.5m)))

floodExt<- ifelse(wshCostGrow>0,1,0)
floodExt.3m<- ifelse(wshCostGrow>0.3,1,0)

#define spatial extent using a bounding box (left, right, bottom, top)
bbox_to_keep <- c(min(coordsinBds.5m[,1])-1, max(coordsinBds.5m[,1])+1, 
                  min(coordsinBds.5m[,2])-1, max(coordsinBds.5m[,2])+1)
extent_to_keep <- ext(bbox_to_keep)

#Crop the raster to the new extent
flood.3m <- crop(run5m, extent_to_keep)

values(flood.3m)<- floodExt.3m

#filename<- "plots/CostGrowFloodExt.3m_10mto5m.jpeg"
#jpeg(file = filename,width = 300,height=640)
#plot(flood.3m,axes=FALSE)
#dev.off()

flooded<- ifelse(wshCostGrow>0.3,"yes","no")
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

df<- data.frame("flooding"= flooded,
                "latitude"= lonlat_all[,2],
                "longitude"=lonlat_all[,1])

filename<- "plots/flood.3m_CostGrow.jpeg"
jpeg(file = filename,width = 690,height=350)
Norristown_plot +
  geom_point(data = df, alpha = 0.5, shape=15, size= 2,
             mapping = aes(x = longitude, y = latitude, 
                           color=flooding))+ 
  scale_color_manual(values=c("yellow", "blue")) +
  ggtitle("CostGrow")+
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

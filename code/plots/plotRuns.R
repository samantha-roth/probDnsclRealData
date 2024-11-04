#plot calibrated projections at all resolutions

rm(list=ls())

setwd("C:/Users/svr5482")

library(terra); library(RColorBrewer)

run5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/PostMedCh/Extent/Run_1.asc")
coords.5m<- xyFromCell(run5m,1:ncell(run5m))
flood5m<- c(as.matrix(extract(run5m,coords.5m)))

run10m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs10m/Norristown/nCh/PostMedCh/Extent/Run_1.asc")
coords.10m<- xyFromCell(run10m,1:ncell(run10m))
flood10m<- c(as.matrix(extract(run10m,coords.10m)))

load("C:/Users/svr5482/probDnsclRealData/data/maxFH.RData")
################################################################################

library(raster)
library(RColorBrewer)
library(ggmap)
library(terra)
library(rgdal)
library(osmdata)
library(ggplot2)

################################################################################
#5m resolution

#define spatial extent using a bounding box (left, right, bottom, top)
bbox_to_keep <- c(min(coords.5m[,1])-1, max(coords.5m[,1])+1, 
                  min(coords.5m[,2])-1, max(coords.5m[,2])+1)
extent_to_keep <- extent(bbox_to_keep)

#subset the raster based on the defined extent
run5mDest <- crop(run5m, extent_to_keep)
coords.5mBox<- xyFromCell(run5mDest,1:ncell(run5mDest))

#all good
min(coords.5m[,1]); max(coords.5m[,1])
min(coords.5mBox[,1]); max(coords.5mBox[,1])

min(coords.5m[,2]); max(coords.5m[,2])
min(coords.5mBox[,2]); max(coords.5mBox[,2])

corners<- matrix(c(min(coords.5m[,1])-1,min(coords.5m[,2])-1,
                   min(coords.5m[,1])-1,max(coords.5m[,2])+1,
                   max(coords.5m[,1])+1,min(coords.5m[,2])-1,
                   max(coords.5m[,1])+1,max(coords.5m[,2])+1),
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
  opq(timeout = 50) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary")) %>%
  osmdata_sf()

# Create the plot object, using the osm_lines element of Philadelphia_major
Norristown_plot <- ggplot() + 
  #xlim(min.lon-.0001,max.lon+.0001) + 
  #ylim(min.lat-.00005,max.lat+.00005) +
  xlim(min.lon,max.lon) + 
  ylim(min.lat,max.lat)

#what locations are within the box?
v_all <- vect(coords.5m, crs="+proj=utm +zone=18 +datum=WGS84  +units=m")
y_all <- terra::project(v_all, "+proj=longlat +datum=WGS84")

lonlat_all <- geom(y_all)[, c("x", "y")]

df<- data.frame("m"= flood5m,
                "latitude"= lonlat_all[,2],
                "longitude"=lonlat_all[,1])

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, maxFH))

filename<- "probDnsclRealData/plots/run5m.jpeg"
jpeg(file = filename,width = 590,height=500)
Norristown_plot +
  geom_point(data = df, alpha = 0.5, shape=15, size= 1.25,
             mapping = aes(x = longitude, y = latitude, 
                           color=m))+ sc +
  theme_bw()+
  theme(plot.title = element_text(size=30), 
        axis.title = element_text(size=30),
        axis.text = element_text(size = 30),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.text= element_text(size=30),
        legend.title= element_text(size=30),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "right") 
dev.off()

################################################################################
#10m resolution

#define spatial extent using a bounding box (left, right, bottom, top)
bbox_to_keep <- c(min(coords.10m[,1])-1, max(coords.10m[,1])+1, 
                  min(coords.10m[,2])-1, max(coords.10m[,2])+1)
extent_to_keep <- extent(bbox_to_keep)

#subset the raster based on the defined extent
run10mDest <- crop(run10m, extent_to_keep)
coords.10mBox<- xyFromCell(run10mDest,1:ncell(run10mDest))

#all good
min(coords.10m[,1]); max(coords.10m[,1])
min(coords.10mBox[,1]); max(coords.10mBox[,1])

min(coords.10m[,2]); max(coords.10m[,2])
min(coords.10mBox[,2]); max(coords.10mBox[,2])

corners<- matrix(c(min(coords.10m[,1])-1,min(coords.10m[,2])-1,
                   min(coords.10m[,1])-1,max(coords.10m[,2])+1,
                   max(coords.10m[,1])+1,min(coords.10m[,2])-1,
                   max(coords.10m[,1])+1,max(coords.10m[,2])+1),
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
  opq(timeout = 50) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary")) %>%
  osmdata_sf()

# Create the plot object, using the osm_lines element of Philadelphia_major
Norristown_plot <- ggplot() + 
  #xlim(min.lon-.0001,max.lon+.0001) + 
  #ylim(min.lat-.00005,max.lat+.00005) +
  xlim(min.lon,max.lon) + 
  ylim(min.lat,max.lat)

#what locations are within the box?
v_all <- vect(coords.10m, crs="+proj=utm +zone=18 +datum=WGS84  +units=m")
y_all <- terra::project(v_all, "+proj=longlat +datum=WGS84")

lonlat_all <- geom(y_all)[, c("x", "y")]

df<- data.frame("m"= flood10m,
                "latitude"= lonlat_all[,2],
                "longitude"=lonlat_all[,1])

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, maxFH))

filename<- "probDnsclRealData/plots/run10m.jpeg"
jpeg(file = filename,width = 590,height=500)
Norristown_plot +
  geom_point(data = df, alpha = 0.5, shape=15, size= 2.5,
             mapping = aes(x = longitude, y = latitude, 
                           color=m))+ sc +
  theme_bw()+
  theme(plot.title = element_text(size=30), 
        axis.title = element_text(size=30),
        axis.text = element_text(size = 30),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.text= element_text(size=30),
        legend.title= element_text(size=30),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "right") 
dev.off()

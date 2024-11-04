#get sensitivity and specificity in wet low resolution cell area

rm(list=ls())

setwd("C:/Users/svr5482")

library(terra); library(RColorBrewer)

run5m<- rast("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/PostMedCh/Extent/Run_1.asc")
coords.5m<- xyFromCell(run5m,1:ncell(run5m))

################################################################################
#load estimated variance
load("probDnsclRealData/data/varResHWM10mto5m.RData")

#load flooded locations at the two lower resolutions being downscaled
load("probDnsclRealData/data/floodInds10mat5mAroundHWMs.RData")

#load region of interest coordinates
load("probDnsclRealData/data/boxAroundHWMs.5m.RData")

#load calibrated 5m flood projections in region of interest
load("probDnsclRealData/data/vals5minBdsAroundHWMs.RData")

#load downscaled calibrated flood projections in region of interest
load("probDnsclRealData/data/downscale10mto5mAroundHWMs.RData")

#load cells of interest outside the low res flooded cells
load("probDnsclRealData/data/destInds10mat5m.RData")

length(which(good5minds%in%good5minds[c(destIndsinGoodInds10m,floodInds10mat5m)]))

################################################################################
#get flooded and not flooded locations within the business region of interest
floodInds<- good5minds[floodInds10mat5m]

################################################################################
#load flooding probability for dest cells
load("probDnsclRealData/data/p.3mFloodDry_10mto5mElev.RData")

load("probDnsclRealData/data/p.3mflood_10mto5m.RData")
################################################################################

pFlood<- rep(NA, nrow(coordsinBds.5m))

pFlood[floodInds10mat5m]<- pFloodWet
pFlood[-floodInds10mat5m]<- pFloodDry

#define spatial extent using a bounding box (left, right, bottom, top)
bbox_to_keep <- c(min(coordsinBds.5m[,1])-1, max(coordsinBds.5m[,1])+1, 
                  min(coordsinBds.5m[,2])-1, max(coordsinBds.5m[,2])+1)
extent_to_keep <- ext(bbox_to_keep)

#Crop the raster to the new extent
pFlood.3m <- crop(run5m, extent_to_keep)

#coords_pFlood.3m<- xyFromCell(pFlood.3m,1:ncell(pFlood.3m))

values(pFlood.3m)<- pFlood

# Define a custom color palette (e.g., a gradient from blue to red)
colors <- colorRampPalette(c("blue", "yellow", "red"))(100)
legend_args <- list(text=list(cex=1.5))  # Adjust cex (character expansion) for text size

# Plot the raster using the specified colors
filename<- "probDnsclRealData/plots/SpatPFlood.3m_10mto5mElev.jpeg"
jpeg(file = filename,width = 300,height=640)
plot(pFlood.3m, col=colors, axes=FALSE)
#legend("right", legend=seq(0,1, length.out=5), fill=colors, cex=1.5)
dev.off()

flood.3m<- crop(run5m, extent_to_keep)

values(flood.3m)<- ifelse(pFlood>.5, 1,0)

filename<- "probDnsclRealData/plots/SpatFlood.3m_10mto5mElev.jpeg"
jpeg(file = filename,width = 300,height=640)
plot(flood.3m, axes=FALSE)
#legend("right", legend=seq(0,1, length.out=5), fill=colors, cex=1.5)
dev.off()

################################################################################


##This is a mess, I give up

##Produce map of Norristown in the business area of interest
#v <- vect(coordsinBds.5m, crs="+proj=utm +zone=18 +datum=WGS84  +units=m")
#y <- terra::project(v, "+proj=longlat +datum=WGS84")

#lonlat <- geom(y)[, c("x", "y")]
##lonlat

#max.lat<- max(lonlat[,2])
#min.lat<- min(lonlat[,2])
#max.lon<- max(lonlat[,1])
#min.lon<- min(lonlat[,1])

#raster_obj1 <- rast(ext = ext(min.lon, max.lon, min.lat, max.lat),
#                   nrows = length(unique(lonlat[,2])), 
#                   ncols = length(unique(lonlat[,1])),
#                   crs = "+proj=longlat +datum=WGS84")

#coords.raster_obj1<- xyFromCell(raster_obj1,1:ncell(raster_obj1))

#raster_obj <- rasterize(points, raster_obj1, field = pFlood, fun = mean)

## Define the extent of the raster
#r_ext <- ext(min.lon, max.lon, min.lat, max.lat)

## Create a raster with the defined extent and specify rows and columns
#raster_obj <- rast(nrows = length(unique(lonlat[,2])), ncols = length(unique(lonlat[,1])), 
#                   extent = r_ext, crs = "+proj=longlat +datum=WGS84")

## Assign the values to the raster object based on the order of (x, y)
## Here, we might need to reshape the values to match the raster grid
#values(raster_obj) <- matrix(pFlood, nrow = length(unique(lonlat[,2])),byrow=TRUE)
#plot(raster_obj)


#a<- values(raster_obj)
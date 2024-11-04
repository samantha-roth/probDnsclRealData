#get sensitivity and specificity in wet low resolution cell area

rm(list=ls())

setwd("C:/Users/svr5482")

library(terra)

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")

for(f in 1:length(flow)){
  print(flow[f])
  
  run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  coords.5m<- xyFromCell(run5m,1:ncell(run5m))
  
  ################################################################################
  #load estimated variance
  load("probabilisticDownscaling/data/simObs/varResHWM10mto5m.RData")
  
  #load flooded locations at the two lower resolutions being downscaled
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/floodInds10mat5mAroundRS20.RData"))
  
  #load region of interest coordinates
  load("probabilisticDownscaling/data/simObs/boxAroundRS20.5m.RData")
  
  #load calibrated 5m flood projections in region of interest
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/vals5minBdsAroundRS20.RData"))
  
  #load downscaled calibrated flood projections in region of interest
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/downscale10mto5mAroundRS20.RData"))
  
  #load cells of interest outside the low res flooded cells
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds10mat5m.RData"))
  
  length(which(good5minds%in%good5minds[c(destIndsinGoodInds10m,floodInds10mat5m)]))
  
  ################################################################################
  #get flooded and not flooded locations within the business region of interest
  floodInds<- good5minds[floodInds10mat5m]
  
  ################################################################################
  #load correct, incorrect, flooded and not flooded inds for wet cells
  load(paste0("probabilisticDownscaling/data/",flow[f],"/wetCellAccuracy10mto5m.RData"))
  
  #load correct, incorrect, flooded and not flooded inds for dry cells
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/estProbFloodIndsofInterest_10mto5m.RData"))
  
  ################################################################################
  
  #for high resolution locations in low resolution wet cells
  correctFloodWetInds<- floodInds[correctFloodInds]
  correctNoFloodWetInds<- floodInds[correctNoFloodInds]
  incorrectNoFloodWetInds<- floodInds[setdiff(flood5mInds,correctFloodInds)]
  incorrectFloodWetInds<- floodInds[setdiff(noFlood5mInds,correctNoFloodInds)]
  
  #for high resolution locations in low resolution dry cells
  correctFloodDryInds<- destInds10m[indsFloodCorrect]
  correctNoFloodDryInds<- destInds10m[indsNotFloodCorrect]
  incorrectNoFloodDryInds<- destInds10m[setdiff(indsFlood,indsFloodCorrect)]
  incorrectFloodDryInds<- destInds10m[setdiff(indsNotFlood,indsNotFloodCorrect)]
  
  label<- rep(NA, nrow(coords.5m))
  
  #label[floodInds10mat5m]<- "wet"
  #label[-floodInds10mat5m]<- "dry"
  
  label[correctFloodWetInds]<- "True Positive"
  label[correctFloodDryInds]<- "True Positive"
  label[correctNoFloodWetInds]<- "True Negative"
  label[correctNoFloodDryInds]<- "True Negative"
  
  label[incorrectFloodWetInds]<- "False Positive"
  label[incorrectFloodDryInds]<- "False Positive"
  label[incorrectNoFloodWetInds]<- "False Negative"
  label[incorrectNoFloodDryInds]<- "False Negative"
  
  
  notNAinds<- which(!is.na(label))
  label<- label[good5minds]
  
  #df<- data.frame("label"=label,
  #                "x"=coordsinBds.5m[,"x"],
  #                "y"=coordsinBds.5m[,"y"])
  
  ################################################################################
  
  library(raster)
  library(RColorBrewer)
  library(ggmap)
  library(terra)
  library(rgdal)
  library(osmdata)
  library(ggplot2)
  
  #define spatial extent using a bounding box (left, right, bottom, top)
  bbox_to_keep <- c(min(coordsinBds.5m[,1])-1, max(coordsinBds.5m[,1])+1, 
                    min(coordsinBds.5m[,2])-1, max(coordsinBds.5m[,2])+1)
  extent_to_keep <- extent(bbox_to_keep)
  
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
    opq(timeout = 50) %>%
    add_osm_feature(key = "highway", 
                    value = c("motorway", "primary", "secondary")) %>%
    osmdata_sf()
  
  # Create the plot object, using the osm_lines element of Philadelphia_major
  Norristown_plot <- ggplot() + 
    #xlim(min.lon-.0001,max.lon+.0001) + 
    #ylim(min.lat-.00005,max.lat+.00005) +
    xlim(min.lon,max.lon) + 
    ylim(min.lat,max.lat) #+
  #geom_sf(data = Norristown_major$osm_lines,
  #        inherit.aes = FALSE,
  #        color = "black",
  #        size = 0.2)
  # Print the plot
  #Norristown_plot
  
  #Norristown_minor <- Norristown_bb %>%
  #  opq() %>%
  #  add_osm_feature(key = "highway", value = c("tertiary", "residential", "living_street", "road", "secondary")) %>%
  #  osmdata_sf()
  
  #Norristown_plot <- Norristown_plot +
  #  geom_sf(data = Norristown_minor$osm_lines,
  #          inherit.aes = FALSE,
  #          color = "#666666",  # medium gray
  #          size = 0.1) # half the width of the major roads
  # Print the plot
  #Norristown_plot
  
  ################################################################################
  #what locations are within the box?
  v_all <- vect(coordsinBds.5m, crs="+proj=utm +zone=18 +datum=WGS84  +units=m")
  y_all <- terra::project(v_all, "+proj=longlat +datum=WGS84")
  
  lonlat_all <- geom(y_all)[, c("x", "y")]
  
  df<- data.frame("fill"= label,
                  "latitude"= lonlat_all[,2],
                  "longitude"=lonlat_all[,1])
  
  
  filename<- paste0("probabilisticDownscaling/plots/",flow[f],"/BusAreaAccSpatSts_10mto5m.jpeg")
  jpeg(file = filename,width = 950,height=380)
  print(Norristown_plot +
          geom_point(data = df, alpha = 0.5, 
                     mapping = aes(x = longitude, y = latitude, 
                                   shape=fill, color=fill, size= fill))+
          scale_color_manual(values = c("red","yellow","white","blue")) + #if downscaled shifted by elevations
          #scale_color_manual(values = c("red","yellow","green","green","white")) + #if downscaled not shifted by elevations
          scale_shape_manual(values=c(15,15,15,15,15,15))+
          scale_size_manual(values=rep(2.1,6)) +
          theme(plot.title = element_text(size=24), 
                axis.title = element_text(size=24),
                axis.text = element_text(size = 24),
                legend.text= element_text(size=24),
                legend.title= element_text(size=24),
                #axis.title.x=element_blank(),
                #axis.text.x=element_blank(),
                #axis.ticks.x=element_blank(),
                #axis.title.y=element_blank(),
                #axis.text.y=element_blank(),
                #axis.ticks.y=element_blank(),
                legend.position = "bottom"))
  dev.off()
  
}

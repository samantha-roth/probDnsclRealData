rm(list=ls())

#get the 5m source cell within the 50m flooded area that maps to each 5m destination cell

library(terra)
setwd("C:/Users/svr5482")

pt<-proc.time()

#load region of interest coordinates and indices
load("probabilisticDownscaling/data/simObs/boxAroundRS20.5m.RData")

flow<- c("Q2559.8429","Q2503.2092","Q3681.19006")
flow_trunc<- c("Q2559","Q2503","Q3681")

for(f in 1:length(flow)){
  run5m<- rast(paste0("FloodingModelCalibrationProject/04-Spatial_Stats_Samantha/Outputs5m/Norristown/nCh/obs3mMean/",flow[f],"/Extent/Run_",f,".asc"))
  coords.5m<- xyFromCell(run5m,1:ncell(run5m))
  
  backlink50mat5m<- rast(paste0("Documents/ArcGIS/Projects/Storm",flow[f],"/BackLink_10mat5m.tif"))
  backLinkVals50mat5m<- c(as.matrix(extract(backlink50mat5m,coords.5m)))
  
  ################################################################################
  #get the not flooded coordinates that are in the business zone bounds
  
  #load flooded locations at the two lower resolutions within this region
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/floodInds50mat5mAroundRS20.RData"))
  
  load(paste0("probabilisticDownscaling/data/",flow[f],"/simObs/destInds50mat5m.RData"))
  ################################################################################
  
  uX<- unique(coords.5m[,1])
  uY<- unique(coords.5m[,2])
  
  uXord<- order(unique(coords.5m[,1]))
  uYord<- order(unique(coords.5m[,2]))
  
  ords.5m<- matrix(NA,nrow=nrow(coords.5m),ncol=2)
  
  for(i in 1:length(uX)){
    x.inds<- which(coords.5m[,1]==uX[i])
    ords.5m[x.inds,1]<- uXord[i]
  }
  
  for(i in 1:length(uY)){
    y.inds<- which(coords.5m[,2]==uY[i])
    ords.5m[y.inds,2]<- uYord[i]
  }
  
  ordsDestLocs50m<- ords.5m[destInds50m,]
  
  ################################################################################
  #50m 
  
  #function to get the backlink value of a given cell
  backlink_value <- function(cell) {
    ind<- which(ords.5m[,1]==cell[1] & ords.5m[,2]==cell[2])
    backLinkVals50mat5m[ind]
  }
  
  #function to move to the specified neighboring cell
  move_to_cell_in_direction <- function(current_cell, direction) {
    
    current_x <- current_cell[1]
    current_y <- current_cell[2]
    
    #right
    if(direction == 1){
      new_x<- current_x + 1 
      new_y<- current_y} 
    
    #lower right
    if(direction == 2){
      new_x<- current_x + 1
      new_y<- current_y - 1}
    
    #down
    if(direction == 3){
      new_x<- current_x
      new_y<- current_y - 1}
    
    #lower left
    if(direction == 4){
      new_x<- current_x - 1
      new_y<- current_y - 1}
    
    #left
    if(direction == 5){
      new_x<- current_x - 1
      new_y<- current_y}
    
    #upper left
    if(direction == 6){
      new_x<- current_x - 1
      new_y<- current_y + 1}
    
    #up
    if(direction == 7){
      new_x<- current_x
      new_y<- current_y + 1}
    
    #upper right
    if(direction == 8){
      new_x<- current_x + 1
      new_y<- current_y + 1}
    
    return(c(new_x,new_y))
  }
  
  #function to identify flooded source cell for a given destination cell
  #using the backlink raster
  
  identify_source_cell <- function(destination_cell, source_value) {
    current_cell <- destination_cell
    
    while (backlink_value(current_cell) != source_value) {
      direction <- backlink_value(current_cell)
      current_cell <- move_to_cell_in_direction(current_cell, direction)
    }
    
    source_cell <- current_cell
    return(source_cell)
  }
  
  # Example usage
  destination_cell <- ords.5m[1,]
  source_value <- 0  
  source_cell <- identify_source_cell(destination_cell, source_value)
  print(paste("Source Cell:", source_cell))
  
  source_value <- 0  
  sourcesForDests50m<- matrix(NA, nrow= length(destInds50m), ncol= 2)
  
  start<- proc.time()
  for(i in 1:length(destInds50m)){
    #for(i in 1:100){
    source_cell <- identify_source_cell(ordsDestLocs50m[i,], source_value)
    sourcesForDests50m[i,]<- c(source_cell)
  }
  end<- proc.time()
  total<- end[3]-start[3]
  
  save(sourcesForDests50m,total,file=paste0("probabilisticDownscaling/data/",flow[f],"/simObs/sourcesForDests50mto5m.RData"))
  
  ptFinal<-proc.time()-pt
  time_backLink_FloodArea<-ptFinal[3]
  save(time_backLink_FloodArea, file= paste0("C:/Users/svr5482/probabilisticDownscaling/data/",flow[f],"/time_backLink_FloodArea_50mto5m.RData"))
  
}


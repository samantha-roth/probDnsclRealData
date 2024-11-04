rm(list=ls())

#get the 5m source cell within the 10m flooded area that maps to each 5m destination cell

library(terra)

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

#load region of interest coordinates and indices
load("data/boxAroundHWMs.5m.RData")

flood<- c("flood2014","flood2020","floodfuture")

for(f in 1:length(flood)){
  
  pt<-proc.time()
  
  run5m<- rast(paste0("data/Outputs5m/",flood[f],"/Run_1.asc"))
  coords.5m<- xyFromCell(run5m,1:ncell(run5m))
  
  backlink10mat5m<- rast(paste0("data/",flood[f],"/backlink_QGIS.tif"))
  backLinkVals10mat5m<- c(as.matrix(extract(backlink10mat5m,coords.5m)))
  
  ################################################################################
  #get the not flooded coordinates that are in the business zone bounds
  
  #load flooded locations at the two lower resolutions within this region
  load(paste0("data/",flood[f],"/floodInds10mat5mAroundHWMs.RData"))
  
  load(paste0("data/",flood[f],"/destInds10mat5m.RData"))
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
  
  ordsDestLocs10m<- ords.5m[destInds10m,]
  
  ################################################################################
  #10m 
  
  #function to get the backlink value of a given cell
  backlink_value <- function(cell) {
    ind<- which(ords.5m[,1]==cell[1] & ords.5m[,2]==cell[2])
    backLinkVals10mat5m[ind]
  }
  
  #function to move to the specified neighboring cell
  move_to_cell_in_direction <- function(current_cell, direction) {
    
    current_x <- current_cell[1]
    current_y <- current_cell[2]
    
    #right
    if(direction == 2){
      new_x<- current_x + 1 
      new_y<- current_y} 
    
    #lower right
    if(direction == 4){
      new_x<- current_x + 1
      new_y<- current_y - 1}
    
    #down
    if(direction == 8){
      new_x<- current_x
      new_y<- current_y - 1}
    
    #lower left
    if(direction == 16){
      new_x<- current_x - 1
      new_y<- current_y - 1}
    
    #left
    if(direction == 32){
      new_x<- current_x - 1
      new_y<- current_y}
    
    #upper left
    if(direction == 64){
      new_x<- current_x - 1
      new_y<- current_y + 1}
    
    #up
    if(direction == 128){
      new_x<- current_x
      new_y<- current_y + 1}
    
    #upper right
    if(direction == 1){
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
  sourcesForDests10m<- matrix(NA, nrow= length(destInds10m), ncol= 2)
  
  start<- proc.time()
  for(i in 1:length(destInds10m)){
    #for(i in 1:100){
    source_cell <- identify_source_cell(ordsDestLocs10m[i,], source_value)
    sourcesForDests10m[i,]<- c(source_cell)
  }
  end<- proc.time()
  total<- end[3]-start[3]
  
  save(sourcesForDests10m,total,file=paste0("data/",flood[f],"/sourcesForDests10mto5m_QGIS.RData"))
  
  ptFinal<-proc.time()-pt
  time_backLink_FloodArea<-ptFinal[3]
  save(time_backLink_FloodArea, file= paste0("data/",flood[f],"/time_backLink_FloodArea_10mto5m_QGIS.RData"))
  
}


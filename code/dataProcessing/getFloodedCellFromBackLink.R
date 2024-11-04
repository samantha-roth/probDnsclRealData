rm(list=ls())

#get the 5m source cell within the 10m flooded area that maps to each 5m destination cell

library(terra)

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

pt<-proc.time()

run5m<- rast("data/Outputs5m/Run_1.asc")
load("data/coords.5m.RData")

backlinkQGIS<- rast("data/backlink_QGIS.tif")
backlinkValsQGIS<- c(as.matrix(extract(backlinkQGIS,coords.5m)))

################################################################################
#get the not flooded coordinates that are in the business zone bounds

#load region of interest coordinates and indices
load("data/boxAroundHWMs.5m.RData")

#load flooded locations at the two lower resolutions within this region
load("data/floodInds10mat5mAroundHWMs.RData")

load("data/destInds10mat5m.RData")
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

#function to get the backlink value of a given cell
backlink_value <- function(cell) {
  ind<- which(ords.5m[,1]==cell[1] & ords.5m[,2]==cell[2])
  backlinkValsQGIS[ind]
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

# test usage
destination_cell <- ords.5m[1,]
source_value <- 0  
source_cell <- identify_source_cell(destination_cell, source_value)
print(paste("Source Cell:", source_cell))

source_value <- 0  
sourcesForDests10m<- matrix(NA, nrow= length(destInds10m), ncol= 2)

start<- proc.time()
for(i in 1:length(destInds10m)){
  source_cell <- identify_source_cell(ordsDestLocs10m[i,], source_value)
  sourcesForDests10m[i,]<- c(source_cell)
}
end<- proc.time()
total<- end[3]-start[3]

save(sourcesForDests10m,total,file="data/sourcesForDests10mto5m_QGIS.RData")

ptFinal<-proc.time()-pt
time_backLink_FloodArea<-ptFinal[3]
save(time_backLink_FloodArea, file= "data/time_backLink_FloodArea_10mto5m_QGIS.RData")



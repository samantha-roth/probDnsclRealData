#compare backlink rasters from ArcGIS and QGIS

rm(list=ls())

library(terra)
setwd("C:/Users/svr5482")

load("probDnsclRealData/data/coords.5m.RData")

backlinkArcGIS<- rast("Documents/ArcGIS/Projects/ProbDnsclRealData/BackLink_bin10.tif")
backlinkValsArcGIS<- c(as.matrix(extract(backlinkArcGIS,coords.5m)))

backlinkQGIS<- rast("probDnsclRealData/data/backlink_QGIS.tif")
backlinkValsQGIS<- c(as.matrix(extract(backlinkQGIS,coords.5m)))

length(unique(backlinkValsQGIS))
length(unique(backlinkValsArcGIS))

inds8QGIS<- which(backlinkValsQGIS==8)
inds3ArcGIS<- which(backlinkValsArcGIS==3)
mismatchInds8QGIS<- inds8QGIS[which(inds8QGIS%in%inds3ArcGIS==FALSE)]
mismatchInds3ArcGIS<- inds3ArcGIS[which(inds3ArcGIS%in%inds8QGIS==FALSE)]
length(mismatchInds8QGIS)/length(inds3ArcGIS)
length(mismatchInds3ArcGIS)/length(inds8QGIS)

inds16QGIS<- which(backlinkValsQGIS==16)
inds4ArcGIS<- which(backlinkValsArcGIS==4)
mismatchInds16QGIS<- inds16QGIS[which(inds16QGIS%in%inds4ArcGIS==FALSE)]
mismatchInds4ArcGIS<- inds4ArcGIS[which(inds4ArcGIS%in%inds16QGIS==FALSE)]
length(mismatchInds16QGIS)/length(inds4ArcGIS)
length(mismatchInds4ArcGIS)/length(inds16QGIS)

inds4QGIS<- which(backlinkValsQGIS==4)
inds2ArcGIS<- which(backlinkValsArcGIS==2)
mismatchInds4QGIS<- inds4QGIS[which(inds4QGIS%in%inds2ArcGIS==FALSE)]
mismatchInds2ArcGIS<- inds2ArcGIS[which(inds2ArcGIS%in%inds4QGIS==FALSE)]
length(mismatchInds4QGIS)/length(inds2ArcGIS)
length(mismatchInds2ArcGIS)/length(inds4QGIS)


inds2QGIS<- which(backlinkValsQGIS==2)
inds1ArcGIS<- which(backlinkValsArcGIS==1)
mismatchInds2QGIS<- inds2QGIS[which(inds2QGIS%in%inds1ArcGIS==FALSE)]
mismatchInds1ArcGIS<- inds1ArcGIS[which(inds1ArcGIS%in%inds2QGIS==FALSE)]
length(mismatchInds2QGIS)/length(inds1ArcGIS)
length(mismatchInds1ArcGIS)/length(inds2QGIS)


inds0QGIS<- which(backlinkValsQGIS==0)
inds0ArcGIS<- which(backlinkValsArcGIS==0)
mismatchInds0QGIS<- inds0QGIS[which(inds0QGIS%in%inds0ArcGIS==FALSE)]
mismatchInds0ArcGIS<- inds0ArcGIS[which(inds0ArcGIS%in%inds0QGIS==FALSE)]
length(mismatchInds0QGIS)/length(inds0ArcGIS)
length(mismatchInds0ArcGIS)/length(inds0QGIS)

inds32QGIS<- which(backlinkValsQGIS==32)
inds5ArcGIS<- which(backlinkValsArcGIS==5)
mismatchInds32QGIS<- inds32QGIS[which(inds32QGIS%in%inds5ArcGIS==FALSE)]
mismatchInds5ArcGIS<- inds5ArcGIS[which(inds5ArcGIS%in%inds32QGIS==FALSE)]
length(mismatchInds32QGIS)/length(inds5ArcGIS)
length(mismatchInds5ArcGIS)/length(inds32QGIS)

inds128QGIS<- which(backlinkValsQGIS==128)
inds7ArcGIS<- which(backlinkValsArcGIS==7)
mismatchInds128QGIS<- inds128QGIS[which(inds128QGIS%in%inds7ArcGIS==FALSE)]
mismatchInds7ArcGIS<- inds7ArcGIS[which(inds7ArcGIS%in%inds128QGIS==FALSE)]
length(mismatchInds128QGIS)/length(inds7ArcGIS)
length(mismatchInds7ArcGIS)/length(inds128QGIS)

inds64QGIS<- which(backlinkValsQGIS==64)
inds6ArcGIS<- which(backlinkValsArcGIS==6)
mismatchInds64QGIS<- inds64QGIS[which(inds64QGIS%in%inds6ArcGIS==FALSE)]
mismatchInds6ArcGIS<- inds6ArcGIS[which(inds6ArcGIS%in%inds64QGIS==FALSE)]
length(mismatchInds64QGIS)/length(inds6ArcGIS)
length(mismatchInds6ArcGIS)/length(inds64QGIS)

inds1QGIS<- which(backlinkValsQGIS==1)
inds8ArcGIS<- which(backlinkValsArcGIS==8)
mismatchInds1QGIS<- inds1QGIS[which(inds1QGIS%in%inds8ArcGIS==FALSE)]
mismatchInds8ArcGIS<- inds8ArcGIS[which(inds8ArcGIS%in%inds1QGIS==FALSE)]
length(mismatchInds1QGIS)/length(inds8ArcGIS)
length(mismatchInds8ArcGIS)/length(inds1QGIS)

length(c(mismatchInds0ArcGIS,mismatchInds1ArcGIS,mismatchInds2ArcGIS,
         mismatchInds3ArcGIS,mismatchInds4ArcGIS,mismatchInds5ArcGIS,
         mismatchInds6ArcGIS,mismatchInds7ArcGIS,mismatchInds8ArcGIS))/length(backlinkValsArcGIS)

#about 1% of cells are mismatched in labeling between ArcGIS and QGIS
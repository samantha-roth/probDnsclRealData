rm(list=ls())
graphics.off()

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

flood<- c("flood2014","flood2020","floodfuture")

for(f in 1:length(flood)){
  load(paste0("data/",flood[f],"/PI95accuracyMAE.RData"))
  load(paste0("data/",flood[f],"/SensSpecAccuracy_.3mflood.RData"))
  load(paste0("data/",flood[f],"/CostGrowMAEaccuracySensSpec.RData"))
  
  #Table 4
  table4 <- data.frame(
    Approach = c("Mean absolute error", "95% prediction interval coverage", "Percent of flooded and nonflooded cells correctly identified"),
    PDFlood = c(round(MAE,2), round(PI95accuracy,2)*100, round(totAcc,2)*100),
    CostGrow = c(round(MAE.costgrow,2), NA, round(accuracy.costgrow,2)*100)
  )

  print(paste0("Table 4-", flood[f]))
  print(table4)

}

for(f in 1:length(flood)){
  load(paste0("data/",flood[f],"/PI95accuracyMAE.RData"))
  load(paste0("data/",flood[f],"/SensSpecAccuracy_.3mflood.RData"))
  load(paste0("data/",flood[f],"/CostGrowMAEaccuracySensSpec.RData"))
  
  #Table 6
  table6 <- data.frame(
    Approach = c("Percent of flooded cells correctly identified","Percent of nonflooded cells correctly identified"),
    PDFlood = c(round(totalSens,2)*100,round(totalSpec,2)*100),
    CostGrow = c(round(sens.costgrow,2)*100, round(spec.costgrow,2)*100)
  )
  
  print(paste0("Table 6-", flood[f]))
  print(table6)
  
}


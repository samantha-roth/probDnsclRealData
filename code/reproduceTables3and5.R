
rm(list=ls())
graphics.off()

dir<- commandArgs(trailingOnly=TRUE)
setwd(dir)

load("data/PI95accuracyMAE.RData")
load("data/totalSensSpecAccuracy.RData")
load("data/CostGrowMAEaccuracySensSpec.RData")

print(paste0("The reported of MAE for PDFlood is 0.13 m. The value of MAE that you obtained is ",round(MAE,2)," m."))

print(paste0("The reported of 95% predictional interval coverage for PDFlood is 98%. The 95% prediction interval coverage that you obtained is ",round(PI95accuracy,2)*100,"%."))

print(paste0("The reported percent of flooded and non flooded cells correctly identified by PDFlood is 96%. The percent that you obtained is ",round(accuracy,2)*100,"%."))

print(paste0("The reported percent of flooded cells correctly identified by PDFlood is 93%. The percent that you obtained is ",round(totalSens,2)*100,"%."))

print(paste0("The reported percent of nonflooded cells correctly identified by PDFlood is 100%. The percent that you obtained is ",round(totalSpec,2)*100,"%."))

#Table 3
table3 <- data.frame(
  Approach = c("Mean absolute error", "95% prediction interval coverage", "Percent of flooded and nonflooded cells correctly identified"),
  PDFlood = c(round(MAE,2), round(PI95accuracy,2)*100, round(accuracy,2)*100),
  CostGrow = c(round(MAE.costgrow,2), NA, round(accuracy.costgrow,2)*100)
)

print("Table 3")
print(table3)

#Table 5
table5 <- data.frame(
  Approach = c("Percent of flooded cells correctly identified","Percent of nonflooded cells correctly identified"),
  PDFlood = c(round(totalSens,2)*100,round(totalSpec,2)*100),
  CostGrow = c(round(sens.costgrow,2)*100, round(spec.costgrow,2)*100)
)

print("Table 5")
print(table5)


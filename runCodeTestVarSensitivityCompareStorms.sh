#!/bin/bash

DATA_PATH= "/Users/f007f8t/Documents/GitHub/probDnsclRealData"

cd /Users/f007f8t/Documents/GitHub/probDnsclRealData/code/testVarSensitivity/compareStorms/evaluation

Rscript sampleOtherVars.R "$DATA_PATH"
Rscript compareBds.R "$DATA_PATH"

cd /Users/f007f8t/Documents/GitHub/probDnsclRealData/code/testVarSensitivity/compareStorms/models

Rscript probDestCellsWetVSDry.R "$DATA_PATH"
Rscript probDestCellsFlood.3m.R "$DATA_PATH"
Rscript getCDFandPDFatDests.R "$DATA_PATH"

cd /Users/f007f8t/Documents/GitHub/probDnsclRealData/code/testVarSensitivity/compareStorms/evaluation

Rscript sensitivityAndSpecificityWetCells.R "$DATA_PATH"
Rscript sensitivityAndSpecificity.3mWetCells.R "$DATA_PATH"
Rscript comparePredMeanToTruthDest.R "$DATA_PATH"
Rscript getTotalMAE95PIAccuracy.R "$DATA_PATH"
Rscript getTotalSensitivitySpecificity.R "$DATA_PATH"
Rscript getTotalSensitivitySpecificity.3mFlood.R "$DATA_PATH"
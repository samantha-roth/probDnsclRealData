#!/bin/bash

DATA_PATH= "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/dataProcessing
Rscript getFloodedCellFromBackLink.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/dataProcessing
Rscript getSourcesForDests.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/evaluation
Rscript dnsclSource.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/models
Rscript getMeanIfNot0ForDestCells.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/models
Rscript probDestCellsWetVSDry.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/models
Rscript probDestCellsFlood.3m.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/models
Rscript getCDFandPDFatDests.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/evaluation
Rscript sensitivityAndSpecificityWetCells.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/evaluation
Rscript sensitivityAndSpecificity.3mWetCells.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/evaluation
Rscript comparePredMeanToTruthDest.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/evaluation

Rscript getTotalMAE95PIAccuracy.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/evaluation
Rscript getTotalSensitivitySpecificity.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/evaluation
Rscript getTotalSensitivitySpecificity.3mFlood.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/comparison/costgrow
Rscript costgrow10mto5m_MethodArea1.R "$DATA_PATH"

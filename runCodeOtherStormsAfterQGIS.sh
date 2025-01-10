#!/bin/bash

DATA_PATH= "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/dataProcessing
Rscript getFloodedCellFromBackLink.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/dataProcessing
Rscript getSourcesForDests.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/evaluation
Rscript dnsclSource.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/models
Rscript getMeanIfNot0ForDestCells.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/models
Rscript probDestCellsWetVSDry.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/models
Rscript probDestCellsFlood.3m.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/models
Rscript getCDFandPDFatDests.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/evaluation
Rscript sensitivityAndSpecificityWetCells.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/evaluation
Rscript sensitivityAndSpecificity.3mWetCells.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/evaluation
Rscript getTotalMAE95PIAccuracy.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/evaluation
Rscript getTotalSensitivitySpecificity.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/evaluation
Rscript getTotalSensitivitySpecificity.3mFlood.R "$DATA_PATH"

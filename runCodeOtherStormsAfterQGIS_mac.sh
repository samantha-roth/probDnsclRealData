#!/bin/bash

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/dataProcessing
Rscript getFloodedCellFromBackLink.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/dataProcessing
Rscript getSourcesForDests.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/evaluation
Rscript dnsclSource.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/models
Rscript getMeanIfNot0ForDestCells.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/models
Rscript probDestCellsWetVSDry.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/models
Rscript probDestCellsFlood.3m.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/models
Rscript getCDFandPDFatDests.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/evaluation
Rscript sensitivityAndSpecificityWetCells.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/evaluation
Rscript sensitivityAndSpecificity.3mWetCells.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/evaluation
Rscript getTotalMAE95PIAccuracy.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/evaluation
Rscript getTotalSensitivitySpecificity.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/evaluation
Rscript getTotalSensitivitySpecificity.3mFlood.R "/Users/f007f8t/Documents/probDnsclRealData"

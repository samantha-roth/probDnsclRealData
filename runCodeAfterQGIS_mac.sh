#!/bin/bash

cd /Users/f007f8t/Documents/probDnsclRealData/code/dataProcessing
Rscript getFloodedCellFromBackLink.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/dataProcessing
Rscript getSourcesForDests.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/evaluation
Rscript dnsclSource.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/models
Rscript getMeanIfNot0ForDestCells.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/models
Rscript probDestCellsWetVSDry.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/models
Rscript probDestCellsFlood.3m.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/models
Rscript getCDFandPDFatDests.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/evaluation
Rscript sensitivityAndSpecificityWetCells.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/evaluation
Rscript sensitivityAndSpecificity.3mWetCells.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/evaluation
Rscript comparePredMeanToTruthDest.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/evaluation

Rscript getTotalMAE95PIAccuracy.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/evaluation
Rscript getTotalSensitivitySpecificity.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/evaluation
Rscript getTotalSensitivitySpecificity.3mFlood.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/comparison/costgrow
Rscript costgrow10mto5m_MethodArea1.R "/Users/f007f8t/Documents/probDnsclRealData"

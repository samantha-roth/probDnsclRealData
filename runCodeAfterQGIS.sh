#!/bin/bash
#SBATCH --time=48:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=10gb
#SBATCH --partition=open

source activate /storage/work/svr5482/probDnsclRealData

cd /storage/work/svr5482/probDnsclRealData/code/dataProcessing
Rscript getFloodedCellFromBackLink.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/dataProcessing
Rscript getSourcesForDests.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/evaluation
Rscript dnsclSource.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/models
Rscript getMeanIfNot0ForDestCells.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/models
Rscript probDestCellsWetVSDry.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/models
Rscript probDestCellsFlood.3m.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/models
Rscript getCDFandPDFatDests.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/evaluation
Rscript sensitivityAndSpecificityWetCells.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/evaluation
Rscript sensitivityAndSpecificity.3mWetCells.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/evaluation
Rscript comparePredMeanToTruthDest.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/evaluation

Rscript getTotalMAE95PIAccuracy.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/evaluation
Rscript getTotalSensitivitySpecificity.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/evaluation
Rscript getTotalSensitivitySpecificity.3mFlood.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/comparison/costgrow
Rscript costgrow10mto5m_MethodArea1.R "/storage/work/svr5482/probDnsclRealData"

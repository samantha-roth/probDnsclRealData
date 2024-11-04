#!/bin/bash
#SBATCH --time=48:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=10gb
#SBATCH --partition=open

source activate /storage/work/svr5482/probDnsclRealData

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/dataProcessing
Rscript getFloodedCellFromBackLink.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/dataProcessing
Rscript getSourcesForDests.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/evaluation
Rscript dnsclSource.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/models
Rscript getMeanIfNot0ForDestCells.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/models
Rscript probDestCellsWetVSDry.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/models
Rscript probDestCellsFlood.3m.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/models
Rscript getCDFandPDFatDests.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/evaluation
Rscript sensitivityAndSpecificityWetCells.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/evaluation
Rscript sensitivityAndSpecificity.3mWetCells.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/evaluation
Rscript getTotalMAE95PIAccuracy.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/evaluation
Rscript getTotalSensitivitySpecificity.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/evaluation
Rscript getTotalSensitivitySpecificity.3mFlood.R "/storage/work/svr5482/probDnsclRealData"
#!/bin/bash
#SBATCH --time=48:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=10gb
#SBATCH --partition=open

source activate /storage/work/svr5482/probDnsclRealData

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/dataProcessing
Rscript getWetIndsAroundHWMs.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/evaluation
Rscript dnsclAroundHWMs.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/evaluation
Rscript compareBds.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/evaluation
Rscript floodByElev.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/models
Rscript modelProbFloodbyElev.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/dataProcessing
Rscript getProbFloodAtDestLocs.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/compareStorms/dataProcessing
Rscript costDistPrep_FloodArea.R "/storage/work/svr5482/probDnsclRealData"


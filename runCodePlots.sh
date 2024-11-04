#!/bin/bash
#SBATCH --time=48:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=10gb
#SBATCH --partition=open

source activate /storage/work/svr5482/probDnsclRealData

cd /storage/work/svr5482/probDnsclRealData/code/plots
Rscript plotAccuracyWholeRegion.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/plots
Rscript plotP.3mFlood_Dnscl.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/plots
Rscript plot.3mFlood_Dnscl.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/plots
Rscript plotP.3mFlood_Dnscl.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/plots
Rscript plotCostGrow.3mFlood.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/plots
Rscript plotHR.3mFlood.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/plots
Rscript plotDistatHWMs_Dnscl.R "/storage/work/svr5482/probDnsclRealData"
#!/bin/bash

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

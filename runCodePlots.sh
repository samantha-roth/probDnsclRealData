#!/bin/bash

DATA_PATH= "/Users/f007f8t/Documents/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/plots
Rscript plotObsVSPreds.R "$DATA_PATH"

cd /storage/work/svr5482/probDnsclRealData/code/plots
Rscript plotP.3mFlood_Dnscl.R "$DATA_PATH"

cd /storage/work/svr5482/probDnsclRealData/code/plots
Rscript plot.3mFlood_Dnscl.R "$DATA_PATH"

cd /storage/work/svr5482/probDnsclRealData/code/plots
Rscript plotCostGrow.3mFlood.R "$DATA_PATH"

cd /storage/work/svr5482/probDnsclRealData/code/plots
Rscript plotHR.3mFlood.R "$DATA_PATH"

cd /storage/work/svr5482/probDnsclRealData/code/plots
Rscript plotDistatHWMs_Dnscl.R "$DATA_PATH"

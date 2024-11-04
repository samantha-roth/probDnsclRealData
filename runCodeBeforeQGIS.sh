#!/bin/bash

cd /storage/work/svr5482/probDnsclRealData/code/dataProcessing

Rscript getCoordsFromRuns.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/dataProcessing

Rscript get5mCoordsAroundHWMs.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/dataProcessing

Rscript getWetIndsAroundHWMs.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/dataProcessing

Rscript getAdjPredsAtAndAroundHWMs.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/evaluation

Rscript dnsclAroundHWMs.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/evaluation

Rscript dnsclAtHWMs.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/evaluation

Rscript compareBds.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/evaluation

Rscript floodByElev.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/models

Rscript modelProbFloodbyElev.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/dataProcessing

Rscript getProbFloodatDestLocs.R "/storage/work/svr5482/probDnsclRealData"

cd /storage/work/svr5482/probDnsclRealData/code/dataProcessing

Rscript costDistPrep_FloodArea.R "/storage/work/svr5482/probDnsclRealData"


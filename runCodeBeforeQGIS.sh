#!/bin/bash

DATA_PATH= "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/dataProcessing

Rscript getCoordsFromRuns.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/dataProcessing

Rscript get5mCoordsAroundHWMs.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/dataProcessing

Rscript getWetIndsAroundHWMs.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/dataProcessing

Rscript getAdjPredsAtAndAroundHWMs.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/evaluation

Rscript dnsclAroundHWMs.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/evaluation

Rscript dnsclAtHWMs.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/evaluation

Rscript compareBds.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/evaluation

Rscript floodByElev.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/models

Rscript modelProbFloodbyElev.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/dataProcessing

Rscript getProbFloodatDestLocs.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/dataProcessing

Rscript costDistPrep_FloodArea.R "$DATA_PATH"

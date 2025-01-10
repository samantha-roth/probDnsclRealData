#!/bin/bash

DATA_PATH= "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/dataProcessing
Rscript getWetIndsAroundHWMs.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/evaluation
Rscript dnsclAroundHWMs.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/evaluation
Rscript compareBds.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/evaluation
Rscript floodByElev.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/models
Rscript modelProbFloodbyElev.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/dataProcessing
Rscript getProbFloodAtDestLocs.R "$DATA_PATH"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/dataProcessing
Rscript costDistPrep_FloodArea.R "$DATA_PATH"


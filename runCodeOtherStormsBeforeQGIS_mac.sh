#!/bin/bash

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/dataProcessing
Rscript getWetIndsAroundHWMs.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/evaluation
Rscript dnsclAroundHWMs.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/evaluation
Rscript compareBds.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/evaluation
Rscript floodByElev.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/models
Rscript modelProbFloodbyElev.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/dataProcessing
Rscript getProbFloodAtDestLocs.R "/Users/f007f8t/Documents/probDnsclRealData"

cd /Users/f007f8t/Documents/probDnsclRealData/code/compareStorms/dataProcessing
Rscript costDistPrep_FloodArea.R "/Users/f007f8t/Documents/probDnsclRealData"


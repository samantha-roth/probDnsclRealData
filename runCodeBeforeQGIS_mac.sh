#!/bin/bash

cd /Users/f007f8t/Documents/GitHub/probDnsclRealData/code/dataProcessing

Rscript getCoordsFromRuns.R "/Users/f007f8t/Documents/GitHub/probDnsclRealData"

cd /Users/f007f8t/Documents/GitHub/probDnsclRealData/code/dataProcessing

Rscript get5mCoordsAroundHWMs.R "/Users/f007f8t/Documents/GitHub/probDnsclRealData"

cd /Users/f007f8t/Documents/GitHub/probDnsclRealData/code/dataProcessing

Rscript getWetIndsAroundHWMs.R "/Users/f007f8t/Documents/GitHub/probDnsclRealData"

cd /Users/f007f8t/Documents/GitHub/probDnsclRealData/code/dataProcessing

Rscript getAdjPredsAtAndAroundHWMs.R "/Users/f007f8t/Documents/GitHub/probDnsclRealData"

cd /Users/f007f8t/Documents/GitHub/probDnsclRealData/code/evaluation

Rscript dnsclAroundHWMs.R "/Users/f007f8t/Documents/GitHub/probDnsclRealData"

cd /Users/f007f8t/Documents/GitHub/probDnsclRealData/code/evaluation

Rscript dnsclAtHWMs.R "/Users/f007f8t/Documents/GitHub/probDnsclRealData"

cd /Users/f007f8t/Documents/GitHub/probDnsclRealData/code/evaluation

Rscript compareBds.R "/Users/f007f8t/Documents/GitHub/probDnsclRealData"

cd /Users/f007f8t/Documents/GitHub/probDnsclRealData/code/evaluation

Rscript floodByElev.R "/Users/f007f8t/Documents/GitHub/probDnsclRealData"

cd /Users/f007f8t/Documents/GitHub/probDnsclRealData/code/models

Rscript modelProbFloodbyElev.R "/Users/f007f8t/Documents/GitHub/probDnsclRealData"

cd /Users/f007f8t/Documents/GitHub/probDnsclRealData/code/dataProcessing

Rscript getProbFloodatDestLocs.R "/Users/f007f8t/Documents/GitHub/probDnsclRealData"

cd /Users/f007f8t/Documents/GitHub/probDnsclRealData/code/dataProcessing

Rscript costDistPrep_FloodArea.R "/Users/f007f8t/Documents/GitHub/probDnsclRealData"

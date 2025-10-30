## Probabilistic Downscaling for Flood Hazard Models

## Overview
This repository stores the entire workflow for the paper "Probabilistic Downscaling for Flood Hazard Models." This includes all data and code used in the analysis and all generated figures. For supplemental information related to the methods and results not provided in the manuscript, please see PDFlood_supplement.pdf.


## Setup

This workflow has been tested using R version 4.2.1 (Funny-Looking Kid) on a laptop operating under Windows 10 with a 11th Gen Intel(R) Core(TM) i7-1165G7 @ 2.80GHz processor and a 64-bit operating system. This workflow has also been tested using R version 4.4.2 (Pile of Leaves) on laptops operating with (1) macOS Sequoia 15.2 and an Apple M3 chip and (2) macOS Sonoma 15.2 with an Apple M1 Max chip. To perform cost distance analysis, we use the Whitebox Workflows for QGIS plugin for QGIS version 3.34.10. To install the Whitebox Workflows plugin, follow installation instructions at https://www.whiteboxgeo.com/manual/wbt_book/qgis_plugin.html.

The R packages, in the format "package (version)," required for this analysis are `terra` (1.7-3), `akima` (0.6-3.4), `DescTools` (0.99.54), `MASS` (7.3-58.2), `Matrix` (1.6-1.1), `fields` (14.1), `mvtnorm` (1.1-3), and `spam` (2.9-1). 
For the visualizations, the required R packages are `RColorBrewer` (1.1-3), `ggmap` (3.0.1), `osmdata` (0.2.5), `ggplot2` (3.4.4). Before installing the necessary R packages, we recommend updating R to version 4.4.2 and deleting any dependences for any of these R packages that were installed on an earlier version of R.

## Reproduction
To complete the analysis more quickly in chunks, perform the following steps:
1. In all bash files, replace `/Users/f007f8t/Documents` with the location of your `probDnsclRealData` folder. Edit the bash files as necessary to schedule R scripts to be run on your local machine or the cluster of your choice. Examples are provided for a Mac laptop.
2. Install the necessary R packages by running `installPackages.R` in the `code` folder.
3. Call the bash file `runCodeBeforeQGIS.sh`. This should take less than a minute to execute. 
4. Open QGIS.
5. Go to the processing toolbox and search for CostDistance, which will show up under Whitebox Tools.
6. From the folder `~/probDnsclRealData/data`, select the raster `bin10mat5m.tif` as the 'Input Source File' and the raster `norristown_5m.tif` as the 'Input Cost (Friction) File.' Under 'Output Backlink File,' specify the name `backlink_QGIS.tif` and the folder `~/probDnsclRealData/data`. It does not matter whether the 'Open output file after running algorithm' boxes are checked. The output cost accumulation file does not need to be saved.
7. Hit 'Run.'
8. Call the bash file `runCodeAfterQGIS.sh`. This should not take more than a couple minutes to execute.
9. Call the bash file `runCodePlots.sh`. This should only take seconds to execute.
10. Call the bash file `runCodeOtherStormsBeforeQGIS.sh`. This should not take more than a minute to execute.
11. Repeat steps 4-7, in step 6 replacing `~/probDnsclRealData/data` with `~/probDnsclRealData/data/flood2014` when selecting `bin10mat5m.tif` as the 'Input Source File'. Also, replace `~/probDnsclRealData/data` with `~/probDnsclRealData/data/flood2014` when selecting the folder to save `backlink_QGIS.tif` to.
12. Repeat steps 4-7, in step 6 replacing `~/probDnsclRealData/data` with `~/probDnsclRealData/data/flood2020` when selecting `bin10mat5m.tif` as the 'Input Source File'. Also, replace `~/probDnsclRealData/data` with `~/probDnsclRealData/data/flood2020` when selecting the folder to save `backlink_QGIS.tif` to.
13. Repeat steps 4-7, in step 6 replacing `~/probDnsclRealData/data` with `~/probDnsclRealData/data/floodfuture` when selecting `bin10mat5m.tif` as the 'Input Source File'. Also, replace `~/probDnsclRealData/data` with `~/probDnsclRealData/data/floodfuture` when selecting the folder to save `backlink_QGIS.tif` to.
14. Call the bash file `runCodeOtherStormsAfterQGIS.sh`. This should not take more than a few minutes to execute.
15. Compare generated figures in the `plots` folder to the figure with the same name in the `compareResults` folder.
16. Run `reproduceTables3and5.R` and `reproduceTables4and6.R` in the `code` folder. Compare the output to the corresponding tables in the `compareResults` folder.

- If you wish to downscale flood projections using CostGrow for comparison, please see the corresponding section at the bottom of the file.

**To run each R script individually not using bash files, follow the steps below. All subsequent folders are located in the `code` folder.**

**Setup**
1. Install packages needed for the main analysis and visualization: `installPackages.R`

**Preliminary data processing**
1. In the `dataProcessing` folder, get the coordinates of the grid cells for the model runs at each model resolution: `getCoordsFromRuns.R`
2. In the `dataProcessing` folder, get the high resolution cells within the bounds of the business area of interest: `get5mCoordsAroundHWMs.R`
3. In the `dataProcessing` folder, get the 5m locations that are flooded at the 10m resolution near the HWMs: `getWetIndsAroundHWMs.R`
4. In the `dataProcessing` folder, get the adjusted projections based on 5m being the high resolution at and around the high water marks: `getAdjPredsAtAndAroundHWMs.R`

**Decide on a model structure for our model for the observations**
1. In the `evaluation` folder, get downscaled Xm flood heights to plug into model for high resolution cells contained by low resolution wet cells near the high water marks used for calibration: `dnsclAroundHWMs.R`
2. In the `evaluation` folder, get downscaled Xm flood heights to plug into model for high resolution cells at the same locations as the high water marks used for calibration: `dnsclAtHWMs.R`
3. In the `evaluation` folder, use downscaled Xm flood heights to obtain confidencence interval coverage and width: `compareBds.R`

**Use elevations to predict whether or not a cell is flooded**
1. In the `evaluation` folder, compute percent of cells flooded in different elevation groups: `floodbyElev.R`
2. In the `models` folder, fit a gaussian process to model the probability of flooding by elevation: `modelProbFloodbyElev.R`
3. In the `dataProcessing` folder, use these GPs to predict the probability of coming from the distribution of flood heights at each high resolution cell within a low resolution dry cell: `getProbFloodatDestLocs.R`

**Find and get the predictive distribution of flooded (at low res) sources cells for dry (at low res) destination cells**
1. In the `dataProcessing` folder, create a raster at each resolution where every location inside the low res flooded area =1 and every location outside the low res flooded area = NA: `costDistPrep_FloodArea.R`
2. Look under the section 'GIS Analysis- Distance Tools' under the Whitebox Tools section of the processing toolbar in QGIS. Select 'Cost Distance.'
3. Select the raster `bin10m.tif` as the 'Input Source File' and the raster `norristown_5m.tif` as the 'Input Cost (Friction) File.' Under 'Output Backlink File,' specify the name `backlink_QGIS.tif` and the folder `~/probDnsclRealData/data`.
4. Hit 'Run.'
5. In the `dataProcessing` folder, use the backlink file to find downscaled cell within the low res flooded area that maps to the destination outside the low res flooded area: `getFloodedCellFromBackLink.R`, then `getSourcesForDests.R`
6. In the `evaluation` folder, get downscaled Xm flood projections at source cells: `dnsclSource.R`

**Use the predictive distribution of flooded (at low res) sources cells to get the predictive distributions at dry (at low res) destination cells.**
1. In the `models` folder, get the mean of the distribution of flood heights from the corresponding source cell assuming that the distribution of flood heights at the destination cell does not come from the point mass at zero: `getMeanIfNot0ForDestCells.R`
2. In the `models` folder, compute the total probability of flooding at each high resolution cell within a low resolution dry cell: `probDestCellsWetVSDry.R`
3. In the `models` folder, compute the total probability of flooding (defined as flood height > 0.3m) at each high resolution cell within a low resolution dry cell: `probDestCellsFlood.3m.R`
4. In the `models` folder, compute the approximate CDF and PDF of the distribution of high resolution flood heights in each low resolution dry cell: `getCDFandPDFatDests.R`

**Evaluate Performance**
1. In the `evaluation` folder, get the percent of flooded and non-flooded cells identified for high resolution cells within low resolution dry cells: `sensitivityAndSpecificityWetCells.R`
2. In the `evaluation` folder, get the percent of flooded and non-flooded (where flooded is defined as having a flood height >0.3m) cells identified for high resolution cells within low resolution dry cells: `sensitivityAndSpecificity.3mWetCells.R`
3. In the `evaluation` folder, compare the mean of the distribution of flood heights for high resolution cells within low resolution dry cells to the high resolution flood heights: `comparePredMeanToTruthDest.R`
4. In the `evaluation` folder, for all high resolution cells, compute the mean absolute errors and 95% prediction interval coverage: `getTotalMAE95PIAccuracy.R`
5. In the `evaluation` folder, for all high resolution cells, compute the percent of flooded and non-flooded cells identified: `getTotalSensitivitySpecificity.R`
6. In the `evaluation` folder, for all high resolution cells, compute the percent of flooded and non-flooded (where flooded is defined as having a flood height >0.3m) cells identified: `getTotalSensitivitySpecificity.3mFlood.R`

**Consider other flood events: follow same steps as above but using adjusted scripts located in the `compareStorms` folder.**

*Preliminary data processing*
1. In the `dataProcessing` folder, identify the low resolution wet cells: `getWetIndsAroundHWMs.R`

*Decide on a model structure for our model for the observations*
1. In the `evaluation` folder, decide on a model structure for our model for the observations: `dnsclAroundHWMs.R`
2. In the `evaluation` folder, use downscaled Xm flood heights to obtain confidencence interval coverage and width: `compareBds.R`

*Use elevations to predict whether or not a cell is flooded*
1. In the `evaluation` folder, compute percent of cells flooded in different elevation groups: `floodbyElev.R`
2. In the `models` foler, fit a gaussian process to model the probability of flooding by elevation: `modelProbFloodbyElev.R`
3. In the `dataProcessing` folder, use these GPs to predict the probability of coming from the distribution of flood heights at each high resolution cell within a low resolution dry cell: `getProbFloodatDestLocs.R`

*Find and get the predictive distribution of flooded (at low res) sources cells for dry (at low res) destination cells*
1. In the `dataProcessing` folder, create a raster at each resolution where every location inside the low res flooded area =1 and every location outside the low res flooded area = NA: `costDistPrep_FloodArea.R`
2. Look under the section 'GIS Analysis- Distance Tools' under the Whitebox Tools section of the processing toolbar in QGIS. Select 'Cost Distance.'
3. Select the raster `bin10m.tif` as the 'Input Source File' and the raster `norristown_5m.tif` as the 'Input Cost (Friction) File.' Under 'Output Backlink File,' specify the name `backlink_QGIS.tif` and the folder `~/probDnsclRealData/data`.
4. Hit 'Run.'
5. In the `dataProcessing` folder, use the backlink file to find downscaled cell within the low res flooded area that maps to the destination outside the low res flooded area: `getFloodedCellFromBackLink.R`, then `getSourcesForDests.R`
6. In the `evaluation` folder, get downscaled Xm flood projections at source cells: `dnsclSource.R`

*Use the predictive distribution of flooded (at low res) sources cells to get the predictive distributions at dry (at low res) destination cells.*
1. In the `models` folder, get the mean of the distribution of flood heights from the corresponding source cell assuming that the distribution of flood heights at the destination cell does not come from the point mass at zero: `getMeanIfNot0ForDestCells.R`
2. In the `models` folder, compute the total probability of flooding at each high resolution cell within a low resolution dry cell: `probDestCellsWetVSDry.R`
3. In the `models` folder, compute the total probability of flooding (defined as flood height > 0.3m) at each high resolution cell within a low resolution dry cell: `probDestCellsFlood.3m.R`
4. In the `models` folder, compute the approximate CDF and PDF of the distribution of high resolution flood heights in each low resolution dry cell: `getCDFandPDFatDests.R`

*Evaluate Performance*
1. In the `evaluation` folder, get the percent of wet and dry cells identified for high resolution cells within low resolution dry cells: `sensitivityAndSpecificityWetCells.R`
2. In the `evaluation` folder, get the percent of flooded and non-flooded (where flooded is defined as having a flood height >0.3m) cells identified for high resolution cells within low resolution dry cells: `sensitivityAndSpecificity.3mWetCells.R`
3. In the `evaluation` folder, for all high resolution cells, compute the mean absolute errors and 95% prediction interval coverage: `getTotalMAE95PIAccuracy.R`
4. In the `evaluation` folder, for all high resolution cells, compute the percent of wet and dry cells identified: `getTotalSensitivitySpecificity.R`
5. In the `evaluation` folder, for all high resolution cells, compute the percent of flooded and non-flooded cells identified when flooding is defined by a flood height >0.3m: `getTotalSensitivitySpecificity.3mFlood.R`

**Plot results**
1. In the `plots` folder, plot the Hurricane Ida observations against the high resolution flood projection values: `plotObsVSPreds.R`
2. In the `plots` folder, plot probability of flooding over the entire region: `plotP.3mFlood_Dnscl.R` and whether or not flooding occurred based on whether the probability of flooding was >0.5: `plot.3mFlood_Dnscl`.
3. In the `plots` folder, plot the predictive distribution of flood heights, the low resolution flood height, and the high resolution flood height at the high water mark locations: `plotDistatHWMs_Dnscl.R`
4. In the `plots` folder within the `comparison` folder, plot the high resolution binary results: `plotHR.3mFlood.R`
5. Compare generated figures in the `plots` folder to the figure with the same name in the `compareResults` folder.
6. Run `reproduceTables3and5.R` and `reproduceTables4and6.R` in the `code` folder. Compare the output to the corresponding tables in the `compareResults` folder.

**Comparison to CostGrow**
*To downscale projections using CostGrow within QGIS, please follow the steps available at https://github.com/cefect/FloodRescaler*
1. In the `costgrow` folder within the `comparison` folder, compute accuracy metrics of costgrow approach: `costgrow10mto5m_MethodArea1.R`
2. In the `costgrow` folder within the `comparison` folder, plot results of the costgrow approach: `plotCostGrow_Spatial.R`
3. In the `plots` folder, plot binary results of the costgrow approach: `plotCostGrow.3mFlood.R`
4. In the `costgrow` folder within the `comparison` folder within the `compareStorms` folder, compute WSE from the WSH output by CostGrow: `computeWSHfromWSE.R`
5. In the `costgrow` folder within the `comparison` folder within the `compareStorms` folder, compute accuracy metrics of costgrow approach: `getPerformance.R`


## Contact
Please contact Samantha Roth at samantha.m.roth@dartmouth.edu with any reproduction issues.

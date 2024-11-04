#install necessary R packages

rm(list=ls())

install.packages("remotes")

library(remotes)

#for main analysis
install_version("MASS","7.3-58.2")
install_version("Matrix","1.6-1.1")

#problem packages
install_version("terra","1.7-3")
install_version("akima","0.6-3.4")
install_version("DescTools","0.99.54")
install_version("fields","14.1")
install_version("mvtnorm","1.1-3")
install_version("spam","2.9-1")

#for visualizations

install_version("RColorBrewer","1.1-3")
install_version("ggplot2","3.4.4")

install_version("ggmap","3.0.1")
install_version("rgdal","1.6-7")
install_version("osmdata","0.2.5")

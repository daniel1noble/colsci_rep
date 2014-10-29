#-------------------------------------------------------------------------------------------------#
# Title: Quality of colour reporting 
# Authors: Daniel Noble, Tom White, Kate Umbers, James O'Hanlon, Rhiannon Dalrymple, Daniel Zurek
# Description: Analysis of reporting throughout a series of "colour" journals. Mainly summary 
#			   stats and graphs of the data. 
#-------------------------------------------------------------------------------------------------#

# Packages
packages <- c("car", "ggplot2")
install.packages(packages)
lapply(packages, function(x) library(x, character.only = TRUE, verbose = FALSE))

# Set working dir
setwd(path.expand("~/Dropbox/Share\ Folder/colsci_rep/"))

# Load data 
setwd(paste(getwd(), "/data", sep = ""))
coldat <- read.csv("col_data.csv", header = TRUE)[c(1:60),c(1:42)]
str(coldat)


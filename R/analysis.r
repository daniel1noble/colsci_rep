#-------------------------------------------------------------------------------------------------#
# Title: Quality of colour reporting 
# Authors: Daniel Noble, Tom White, Kate Umbers, James O'Hanlon, Rhiannon Dalrymple, Daniel Zurek
# Description: Analysis of reporting throughout a series of "colour" journals. Mainly summary 
#			   stats and graphs of the data. 
#-------------------------------------------------------------------------------------------------#

# Clear workspace
rm(list=ls())

## Packages. Coded to simplify multiple package installation and loading.
packages <- c("car", "ggplot2")
install.packages(packages)
sapply(packages, function(x) library(x, character.only = TRUE, verbose = FALSE))

## Set working dir. path.expand gives a bit of flexibility
setwd(path.expand("~/Dropbox/Share\ Folder/colsci_rep/"))

## Load data 
coldat <- read.csv("data/col_data.csv", header = TRUE)
str(coldat)

## Process data. Remove irrelevant variables for analaysis. 
# Remove justification columns, which are just simply for our use and complicate analysis
col_just   <- colnames(coldat)[c(grep("_just", colnames(coldat)))]
coldat     <- coldat[,! colnames(coldat) %in% col_just][-c(2:13, 35:36)] # Additioally remove details on analysis and details on papers, but keep paper number.

## Things we need to check. The studies categorized as "multiple" Need to make clear what the values (0 or 1) correspond to camera or specs. 
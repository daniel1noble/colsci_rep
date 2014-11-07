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
## Remove justification columns, which are just simply for our use and complicate analysis
col_just   <- colnames(coldat)[c(grep("_just", colnames(coldat)))]
coldat     <- coldat[,! colnames(coldat) %in% col_just][-c(2:13, 35:36)] # Additioally remove details on analysis and details on papers, but keep paper number.

## Things we need to check. The studies categorized as "multiple" Need to make clear what the values (0 or 1) correspond to camera or specs. 

#--------------------------------------------- ANALYSIS -----------------------------------------#
## How many studies utilized specs (1), cameras (2), both (3)?
studytype <- table(coldat$stud_type)

specs <- as.numeric(studytype[1])
cam   <- as.numeric(studytype[2])
both  <- as.numeric(studytype[3])

## Of the studies using a spec, what proportion report the make and model?
specModel <- (sum(coldat$spec_rep, na.rm = TRUE)/(specs+both))*100

## Of the studies using a spec, what proportion report the make and model?
camModel  <- (sum(coldat$cam_model, na.rm = TRUE)/(cam+both))*100

## Of the studies using a spec, what proportion report the light source. Note that column needs to be extracted because it combines cameras and specs

DatSpec   <- subset(coldat, stud_type == 1 |stud_type == 3)

# Note that we need to change the rows where spec and cemera differ
specLight <- (sum(DatSpec$light_source)/length(DatSpec$light_source))*100

## Of the studies using a cam, what proportion report the light source. Note that column needs to be extracted because it combines cameras and specs

DatCam    <- subset(coldat, stud_type == 2 |stud_type == 3)
CamLight  <- (sum(DatCam$light_source, na.rm = TRUE)/length(DatCam$light_source))*100

## Of studies spec'ing what proportion report a dark standard
drk_std       <- (sum(DatSpec$dark_std)/(length(DatSpec$dark_std)))*100
white_stdspec <- (sum(DatSpec$white_stdspec)/length(DatSpec$white_stdspec))*100

## What proportion of studies specing report the num of specs averaged

specAVG <- (sum(DatSpec$specpix_avg)/length(DatSpec$specpix_avg))*100

## What proportion of studies using a camera report the num of pixs averaged

camAVG  <- (sum(DatCam$specpix_avg)/length(DatCam$specpix_avg))*100

##--------------------------------------
## How do studies analyse colour?
analysisType <- table(coldat$analysis_type)

## How about just spec studies because they can do visual modelling, or colorimetric
analysisTypeSpec <- table(DatSpec$analysis_type)

## What proportion of spec studies reported 1) the integration time, spec angle and distance?

intTime <- (sum(DatSpec$int_time)/nrow(DatSpec))*100
angle   <- (sum(DatSpec$spec_angle)/nrow(DatSpec))*100
dist    <- (sum(DatSpec$spec_dist)/nrow(DatSpec))*100

## Of colormetric studies how many defined their colormetrics?

col_def <- sum(coldat$colmetric_def, na.rm = TRUE)/(length(coldat$colmetric_def[!is.na(coldat$colmetric_def)]))

## How did studies fair in their reporting of visual models?
prop_irrad <- (sum(DatSpec$irrad_type[! is.na(DatSpec$irrad_type)])/length(DatSpec$irrad_type[! is.na(DatSpec$irrad_type)]))*100 # Note that there are a few studies under "multiple" that need to be checked because irrad_type is NA and yet the visual model they report is not given. We should check these.

prop_vismod <- (sum(DatSpec$vis_mod[! is.na(DatSpec$vis_mod)])/length(DatSpec$vis_mod[! is.na(DatSpec$vis_mod)]))*100

prop_spvismod <- (sum(DatSpec$vis_mod_sp[! is.na(DatSpec$vis_mod_sp)])/length(DatSpec$vis_mod_sp[! is.na(DatSpec$vis_mod_sp)]))*100

propvis_mod_param <- (sum(DatSpec$vis_mod_param[! is.na(DatSpec$vis_mod_param)])/length(DatSpec$vis_mod_param[! is.na(DatSpec$vis_mod_param)]))*100


##------------------------------------ Figure 1--------------------------------------##
# Figure of proportions on the hardware/software used

props   <- c(specModel, camModel, specLight, CamLight, drk_std, white_stdspec, specAVG, camAVG)
N_props <- c((specs+both), (cam+both), length(DatSpec$light_source), length(DatCam$light_source), )

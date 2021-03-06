#-------------------------------------------------------------------------------------------------#
# Title: Reproducible research in the study of biological coloration 
# Authors: Daniel Noble, Tom White, Kate Umbers, James O'Hanlon, Rhiannon Dalrymple, Daniel Zurek
# Description: Analysis of reporting throughout a series of "colour" journals. Mainly summary 
#			   stats and graphs of the data. 
#-------------------------------------------------------------------------------------------------#

# Clear workspace
	rm(list=ls())

## Packages. Coded to simplify multiple package installation and loading.
	packages <- c("car", "ggplot2", "plyr", "knitr", "markdown")
	install.packages(packages)
	sapply(packages, function(x) library(x, character.only = TRUE, verbose = FALSE))

## Load data 
	coldat <- read.csv("data/col_data.csv", header = TRUE)
	str(coldat)

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

#Compute proportions of the various criteria. Extract only the rows we are interested in using
	criteriaSpec <- c("light_source","specpix_avg","dark_std", "white_stdspec", "int_time", "spec_angle", "spec_dist", "irrad_type", "vis_mod", "vis_mod_sp", "vis_mod_param", "vis_mod_adapt", "vis_mod_qcatch", "vis_mod_bkg", "vis_mod_noise_type", "refl_fig")

	propSpec_fin   <- apply(DatSpec[sapply(DatSpec, is.numeric)], 2, function(x) sum(x[! is.na(x)])/length(x[! is.na(x)])*100)[criteriaSpec]

	lenthSpec_fin  <- apply(DatSpec[sapply(DatSpec, is.numeric)], 2, function(x) length(x[! is.na(x)]))[criteriaSpec]

	names(propSpec_fin)  <- paste("prop", names(propSpec_fin), sep = "_")
	names(lenthSpec_fin) <- paste("prop", names(lenthSpec_fin), sep = "_")

## Of the studies using a cam, what proportion report the light source. Note that column needs to be extracted because it combines cameras and specs
	DatCam    <- subset(coldat, stud_type == 2 |stud_type == 3)

#Compute various proportions
	criteriaCam <- c("light_source", "specpix_avg")

	propCam_fin  <- apply(DatCam[sapply(DatCam, is.numeric)], 2, function(x) sum(x[! is.na(x)])/length(x[! is.na(x)])*100)[criteriaCam]

	lengthCam_fin <- apply(DatCam[sapply(DatCam, is.numeric)], 2, function(x) length(x[! is.na(x)]))[criteriaCam]

	names(propCam_fin) <- paste("prop_", names(propCam_fin), sep ="")

##--------------------------------------
## How do studies analyse colour?
	analysisType <- table(coldat$analysis_type)

## How many colourmetric studies
	colorMet <- length(coldat$colmetric_def[!is.na(coldat$colmetric_def)])

## Of colormetric studies how many defined their colormetrics?
	col_def <- (sum(coldat$colmetric_def, na.rm = TRUE)/(length(coldat$colmetric_def[!is.na(coldat$colmetric_def)])))*100

## How about just spec studies because they can do visual modelling, or colorimetric
	analysisTypeSpec <- table(DatSpec$analysis_type)

## How did studies fair in their reporting of visual models?
	vismod_type <- table(DatSpec$vis_mod_type)

## Number of studies referencing other work.
	ref_stud <- (sum(coldat$prev_pub)/length(coldat$prev_pub))*100

##Code and data deposition
	data <- table(coldat$data)

##------------------------------------ Figure 1--------------------------------------##
# Figure of proportions on the hardware/software reported
	setwd(paste(getwd(), "/output/figures", sep = ""))

	pdf(file = "figure1.pdf", height = 7, width = 15)
		par(mfrow=c(1,2))
		props   <- c(SpecModel = specModel, CamModel = camModel, propSpec_fin[c("prop_light_source", "prop_specpix_avg", "prop_dark_std", "prop_white_stdspec", "prop_int_time",  "prop_spec_angle", "prop_spec_dist")], propCam_fin)
		N_props <- c(SpecMod= (specs+both), CamMod = (cam+both), lenthSpec_fin[c("prop_light_source", "prop_specpix_avg", "prop_dark_std", "prop_white_stdspec", "prop_int_time",  "prop_spec_angle", "prop_spec_dist")], lengthCam_fin)

		dat     <- t(arrange(data.frame(props, N_props), props, decreasing = TRUE))

		names   <- c("Spec", "Cam", "SpecL", "PixAvg"," CamL"," WtSd", "SpecAvg",  "Angle", "IntT", "Dist", "DrkSd")
		colnames(dat) <- names

		barplot(dat[1,], ylim = c(0,120), ylab = "Percentage of studies reporting criteria", xlab = "", col = "gray", space = 0.30, cex.names = 0.72, mgp = c(2.5,0.5,0), cex.axis = 1.2, cex.lab = 1.5, las = 2) -> bp.out
			abline(h = 0, lwd = 2)
			text(dat[2,], x = bp.out, y = 100)
			text(paste(round(dat[1,], digits = 0), "%", sep = ""), x = bp.out, y = round(dat[1,], digits = 0)*0.5)
			text(x = bp.out[length(bp.out)], y = 114, "(a)", cex = 2)

		props_analy   <- propSpec_fin[c("prop_irrad_type","prop_vis_mod_sp", "prop_vis_mod_adapt", "prop_vis_mod_qcatch", "prop_vis_mod_bkg", "prop_vis_mod_noise_type")]

		N_props_analy <- lenthSpec_fin[c("prop_irrad_type","prop_vis_mod_sp", "prop_vis_mod_adapt", "prop_vis_mod_qcatch", "prop_vis_mod_bkg", "prop_vis_mod_noise_type")]

		dat_analy     <- t(arrange(data.frame(props_analy, N_props_analy), props_analy, decreasing = TRUE))

		names_analy   <- c("SpRep", "Bkg", "Irrad", "Qcatch","Noise"," ChrAdapt")
		colnames(dat_analy) <- names_analy

		barplot(dat_analy[1,], ylim = c(0,120), ylab = "", xlab = "", col = "gray", xlim = c(0,10), cex.names = 0.72, mgp = c(2.5,0.5,0), cex.axis = 1.2, cex.lab = 1.5, las = 2) -> bp.out
			arrows(x0 = -1, y0 = 0, y1 = 0, x1 = bp.out[length(bp.out)] + 1, angle = 90, length = 0, lwd = 2)
			text(dat_analy[2,], x = bp.out, y = 100)
			text(paste(round(dat_analy[1,], digits = 0), "%", sep = ""), x = bp.out, y = round(dat_analy[1,], digits = 0)*0.5)
			text(x = bp.out[length(bp.out)], y = 114, "(b)", cex = 2)
			mtext("Criteria", side = 1, padj = 3, adj = -0.32, cex = 2)
			
	dev.off()

	setwd("../..")
library(pavo)
setwd("ColTest/")

#####FIX GET SPEC####

getspec <- function(where=getwd(), ext='txt', lim=c(300,700), decimal=".", 
           subdir=FALSE, subdir.names=FALSE)
{

extension <- paste('.', ext, sep='')

file_names <- list.files(where, pattern=extension, recursive=subdir, include.dirs=subdir)
files <- paste(where,'/',file_names,sep='')

cat(length(files),' files found; importing spectra\n')

if(subdir.names){
	file_names <- gsub(extension,'',file_names)}else{
	file_names <- gsub(extension,'',basename(file_names))
	 }

if(length(file_names)==0){
	stop('No files found. Try a different ext')
	} 

range <- lim[1]:lim[2]

final <- data.frame(matrix(nrow=length(range), ncol=length(file_names)+1))
final[,1] <- range

# Setting a progress bar
progbar <- txtProgressBar(min=0, max=length(files), style=2)

for(i in 1:length(files))
{

raw <- scan(file=files[i], what='', quiet=T, dec=decimal, sep='\n')
#ToDo we can actually use this raw string to import metadata if we want

# find last line with text
# correct for spectrasuite files, which have a "End Processed Spectral Data" at the end

start <- grep('[A-Da-dF-Zf-z]',raw)
isendline <- length(grep('End.*Spectral Data', raw)) > 0

if(isendline)
  start <- start[-length(start)]


start <- max(start)

end <- length(raw) - start

if( isendline > 0 )
  end <- end - 1

# Avantes has an extra skipped line between header and data. Bad Avantes.
newavaheader <- length(grep("Wave.*;Sample.*;Dark.*;Reference;Reflectance", raw)) > 0

if(newavaheader)
  start <- start + 1

# find if columns are separated by semicolon or tab
issem <- length(grep(';',raw)) > 0
istab <- length(grep('\t',raw)) > 0

if(issem & istab)
  stop('inconsistent column delimitation in source files.')
  
separ <- ifelse(issem,';','\t')

# extract data from file

tempframe <- read.table(files[i], dec=decimal, sep=separ, skip=start, nrows=end, row.names=NULL)		

# remove columns where all values are NAs (due to poor tabulation)
tempframe <- tempframe[ ,colSums(is.na(tempframe))<nrow(tempframe)]

# Jaz and Avasoft8 have 5 columns, correct
tempframe <- tempframe[ ,c(1,dim(tempframe)[2])]


interp<-data.frame(approx(tempframe[,1], tempframe[,2], xout=range))
names(interp) <- c("wavelength", strsplit(file_names[i], extension) )

final[,i+1] <- interp[,2]

setTxtProgressBar(progbar, i)

}

names(final) <- c('wl',gsub(extension,'',file_names))
class(final) <- c('rspec','data.frame')
	
final
}


## Compare effects on visual model
library(pavo)
data <- as.rspec(read.csv("VMdat.csv", header = TRUE))

visMod <- vismodel(data, qcatch = "fi", visual = "avg.uv", illum = "D65", vonkries = TRUE, bkg = "ideal", relative = FALSE)

ColDist <- coldist(visMod, vis = "tetra", noise = "neural", n1 = 1, n2 = 4, n3 = 4, n4 = 8, v = 0.1)


###########

data<-getspec(lim=c(300,700))
data <- procspec(data, fixneg = "zero")

plot(data$wl, data$a01distnorm10, type="l")
lines(data$wl, data$a05boxnorm, lty=2) ### normal one???
lines(data$wl, data$a12degninety1, lty=3)
lines(data$wl, data$a16blacknorm, lty=4) ### or normal???
lines(data$wl, data$a21whitesample1, lty=5)

#### set up par
quartz("comps", height=6, width=9)
par(mfcol=c(2,3), mar=c(4,4,1,1), mgp = c(2.5,1,0), cex.lab = 1.2)

###plot whites
plot(data$wl, data$a05boxnorm, type="l", lwd=3, ylim=c(0,100), yaxs="i", ylab="% Reflectance", xlab="Wavelength (nm)") ####this?
lines(data$wl, data$a22whitepaper) ####this?
lines(data$wl, data$a23whitetape, col="slateblue", lwd=2) ####this?
text(320,90, "a)", cex=2)

lines(c(420,470), c(90,90), lwd=3)
text(475,90, "Sample 1", pos=4)
lines(c(420,470), c(80,80), lwd=2, col="slateblue")
text(475, 80, "Sample 2", pos=4)
lines(c(420,470), c(70,70))
text(475, 70, "Paper", pos=4)


###plot distances
plot(data$wl, data$a05boxnorm, type="l", lwd=3, ylim=c(0,100), yaxs="i", ylab="% Reflectance", xlab="Wavelength (nm)") ####this?
#### lines(data$wl, data$a02dist07) #### not this
lines(data$wl, data$a03dist15) ####this
lines(data$wl, data$a04dist18, col="slateblue", lwd=2) ####this?
text(320,90, "d)", cex=2)

lines(c(420,470), c(90,90), lwd=3)
text(475,90, "10mm", pos=4)
lines(c(420,470), c(80,80))
text(475, 80, "15mm", pos=4)
lines(c(420,470), c(70,70), lwd=2, col="slateblue")
text(475, 70, "18mm", pos=4)


###plot blacks
plot(data$wl, data$a05boxnorm, type="l", lwd=3, ylim=c(0,100), yaxs="i", ylab="% Reflectance", xlab="Wavelength (nm)") ####this?
lines(data$wl, data$a18blackfabric2) ####this?
lines(data$wl, data$a19blackplastic, col="slateblue", lwd=2) ####this?
#### lines(data$wl, data$a20blackcover) #### maybe not this
text(320,90, "b)", cex=2)

lines(c(420,470), c(90,90), lwd=3)
text(475,90, "No light", pos=4)
lines(c(420,470), c(80,80), lwd=2, col="slateblue")
text(475, 80, "Black fabric", pos=4)
lines(c(420,470), c(70,70))
text(475, 70, "Black plastic", pos=4)

###plot angles
plot(data$wl, data$a05boxnorm, type="l", lwd=3, ylim=c(0,100), yaxs="i", ylab="% Reflectance", xlab="Wavelength (nm)") ####this?
lines(data$wl, data$a13degfourtyfive1) ####this?
text(320,90, "e)", cex=2)

lines(c(420,470), c(90,90), lwd=3)
text(475,90, "90 degrees", pos=4)
lines(c(420,470), c(80,80))
text(475,80, "45 degrees", pos=4)


###plot boxcars
plot(data$wl, data$a05boxnorm, type="l", lwd=3, ylim=c(0,100), yaxs="i", ylab="% Reflectance", xlab="Wavelength (nm)") ####this?
### lines(data$wl, data$a06box20, lwd=2, col="slateblue") ####this? bit complicated to view
### lines(data$wl, data$a08box05) #### odd not plot
### lines(data$wl, data$a07box00) #### odd not plot
lines(data$wl, data$a10box00, col="slateblue") ####this?
### lines(data$wl, data$a11box05) ### still complicated
text(320,90, "c)", cex=2)

lines(c(420,470), c(90,90), lwd=3)
text(475,90, "Boxcar 10", pos=4)
lines(c(420,470), c(80,80), col="slateblue")
text(475,80, "Boxcar 0", pos=4)

## plot the chromatic contrast effects from visual modelling
# data
dat <- read.csv("vismod.csv")
dat2 <- matrix(t(dat[1:4,4:5]),nrow=2, ncol = 4)
rownames(dat2) <- c("dS", "dL")

barplot(dat2[1,], ylim = c(0, 30), ylab = expression("Just Noticable Differences" ~Delta~"S"), xlab = "Parameter combination") -> bp.out 
v    <- dat[1:4,2]
dens <- as.character(dat[1:4,3])
text(paste0("v = ", v), x = bp.out[,1], y = 2)
text(dens, x = bp.out[,1], y = 0.75)
text("f)", x = 0.5, y = 27, cex = 2)
box()
quartz.save(file = "Fig1.pdf", type = "pdf")
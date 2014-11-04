
packages <- c("knitr", "markdown")
install.packages(packages)
sapply(packages, function(x) library(x, character.only = TRUE))

#Read necessary functions and files. There are two functions knit2pdf and knit2html which will produce documents directly, but there are problems with pdf and also problems rendering the markdown file when complete. Best to just use knit first then take md file and convert in ternimal. 

rm(list=ls())
setwd("./docs")
knit("ms.Rmd")
setwd("..")

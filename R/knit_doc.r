#Read necessary functions and files. There are two functions knit2pdf and knit2html which will produce documents directly, but there are problems with pdf and also problems rendering the markdown file when complete. Best to just use knit first then take md file and convert in ternimal. 

# First source analysis file 
source("R/analysis.R")

# knit the .RmD file with R code

setwd("./docs/")
knit("ms.Rmd")
setwd("..")

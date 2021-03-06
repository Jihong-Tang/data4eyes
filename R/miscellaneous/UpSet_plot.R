#!/usr/bin/env Rscript

# Usage: read in multiple given number of files and draw the Upset plot
# Parameter: 1st ==> the number of the files, followings are the name
# and the path of the files, and the last one is the output path

# 1- clean up the environment
rm(list = ls())
if(!(require(UpSetR))){install.packages("UpSetR")}

# 2- load the required packages
suppressPackageStartupMessages({
  library(UpSetR)
})

# 3- get info from the parameters
Args <- commandArgs(T)
filenum <- as.numeric(Args[1])
listname <- c()
listcontent <- list()
for (i in seq(1, 2 * filenum - 1, 2)) {
  listname <- c(listname, Args[i + 1])
  listcontent[[length(listcontent) + 1]] <- read.delim2(Args[i + 2])[, 2]
}
outputpath <- Args[2 * (filenum + 1)]
names(listcontent) <- listname

# 4- plot the UpSet plots
pdf.options(reset = TRUE, onefile = FALSE)
pdf(paste0(outputpath, ".pdf"))
upset(fromList(listcontent), order.by = "freq",
      mainbar.y.label = "Intersection Size", sets.x.label = "Set Size",
      number.angles = 0, line.size = 1, point.size = 3)
dev.off()
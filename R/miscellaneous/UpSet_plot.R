#!/usr/bin/env Rscript

# 1- clean up the environment
rm(list = ls())
if(!(require(UpSetR))){install.packages("UpSetR")}

# 2- load the required packages
suppressPackageStartupMessages({
  library(UpSetR)
})

# 3- get info from the parameters
Args <- commandArgs(T)
filenum <- Args[1]
for (i in 1:filenum){
    print(Args[i+1])
}
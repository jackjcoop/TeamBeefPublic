library(tidyverse)
library(lubridate)
#rm(list=ls())


source("loadData.R")


test <- loadData("accl", "accl/post")

directoryPath <- paste("raw/", "accl/post", "/", sep="")
fileList <- list.files(directoryPath)
filePath <- paste(directoryPath, fileList[19], sep="") 
copyFrame <- read.csv(filePath, header = F, stringsAsFactors = FALSE)


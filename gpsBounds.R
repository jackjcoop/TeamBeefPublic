#Library Loading ----

library(tidyverse)
#library(anytime)
#library(plyr)
#library(dplyr)
#library(readr)
library(lubridate)
#library(geosphere)
library(rgdal)
library(raster)
library(sp)
#to remove all variables when needed
#rm(list=ls())
library(rgeos)

source("loadData.R")

#Testing Code ---- 
testFile <- loadData("gps", "gps/pre")
testFile <- testFile[[1]]

testGPS <- testFile[c('Longitude', "Latitude")]


directoryPath <- paste("raw/", "bounds/shapefiles/pen1", "/", sep="")
fileList <- list.files(directoryPath)
filePath <- paste(directoryPath, fileList[6], sep="")

pen1 <- shapefile(filePath)

directoryPath <- paste("raw/", "bounds/shapefiles/pen2", "/", sep="")
fileList <- list.files(directoryPath)
filePath <- paste(directoryPath, fileList[6], sep="")

pen2 <- shapefile(filePath)

irectoryPath <- paste("raw/", "bounds/shapefiles/pen3", "/", sep="")
fileList <- list.files(directoryPath)
filePath <- paste(directoryPath, fileList[6], sep="")

pen3 <- shapefile(filePath)


t <- over(pen1, testGPS)

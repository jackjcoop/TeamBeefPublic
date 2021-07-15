#Library Loading ----

require(tidyverse)
require(lubridate)
require(rgdal)
require(raster)
require(sp)
require(rgeos)

#to remove all variables when needed
#rm(list=ls())

source("loadData.R")

#Testing Loading Code ---- 
shapeList <- loadShapeFiles("raw/bounds")

dataIn <- loadData("gps", "gps/pre")
sharedCRS <- proj4string(shapeList[[1]])

gpsCoords <- loadGPSCoords(dataIn, sharedCRS)

movementCalendar <- loadMovement()

test <- extractZeroGPS(dataList)

#Testing Code ----
rawData <- dataIn
coords <- gpsCoords
shapes <- shapeList

test1 <- extractOutBounds(dataIn)

filterOutBounds <- function(dataIn)
{
  shapes <- loadShapeFiles("raw/bounds")
  sharedCRS <- proj4string(shapeList[[1]])
  
  coords <- loadGPSCoords(dataIn, sharedCRS)
  
  dataList <- list()
  
  for(j in 1:nrow(movementCalendar))
  {
    copyCalendar <- movementCalendar[j, ]
    
    CollarID <- copyCalendar$Collar
    BullID <- copyCalendar$Bull
    pen <- copyCalendar$arcGISShapeFileName
    
    prefix <- "ID"
    varname <- paste(prefix, CollarID, sep="_")
    
    copyFrame <- dataIn[[varname]]
    copyCoords <- coords[[varname]]
    copyShape <- shapes[[pen]]
    
    if(is.null(copyFrame))
    {}
    else if(nrow(copyFrame) > 0)
    {
      #Will have to add when made into a full function
      #copyFrame <- copyFrame[copyFrame$BullID == BullID, ] 
      
      copyFrame <- copyFrame[copyCalendar$StartDate <= as.Date(copyFrame$DateTime) & copyCalendar$EndDate >= as.Date(copyFrame$DateTime), ]

      if(is.null(copyFrame))
      {}
      else if(nrow(copyFrame) > 0)
      {
        analyzedCoordinates <- over(copyCoords, copyShape)
        
        inBounds <- analyzedCoordinates[!is.na(analyzedCoordinates$Shape_Leng) | !is.na(analyzedCoordinates$Shape_Area),]
        
        index <- rownames(inBounds)
        
        outputFrame <- copyFrame[rownames(copyFrame) %in% index,]
        
        logNames <- names(dataList) == varname
        listSum <- sum(logNames)
        
        if(listSum > 0)
        {
          dataList[[varname]] <- rbind(dataList[[varname]], outputFrame)
        }
        else
        {
          dataList[[varname]] <- outputFrame
        } 
      
      }
    }
  }
  
  return(dataList)
}


extractOutBounds <- function(dataIn)
{
  shapes <- loadShapeFiles("raw/bounds")
  sharedCRS <- proj4string(shapeList[[1]])
  
  coords <- loadGPSCoords(dataIn, sharedCRS)
  
  dataList <- list()
  
  for(j in 1:nrow(movementCalendar))
  {
    copyCalendar <- movementCalendar[j, ]
    
    CollarID <- copyCalendar$Collar
    BullID <- copyCalendar$Bull
    pen <- copyCalendar$arcGISShapeFileName
    
    prefix <- "ID"
    varname <- paste(prefix, CollarID, sep="_")
    
    copyFrame <- dataIn[[varname]]
    copyCoords <- coords[[varname]]
    copyShape <- shapes[[pen]]
    
    if(is.null(copyFrame))
    {}
    else if(nrow(copyFrame) > 0)
    {
      #Will have to add when made into a full function
      #copyFrame <- copyFrame[copyFrame$BullID == BullID, ] 
      
      copyFrame <- copyFrame[copyCalendar$StartDate <= as.Date(copyFrame$DateTime) & copyCalendar$EndDate >= as.Date(copyFrame$DateTime), ]
      
      if(is.null(copyFrame))
      {}
      else if(nrow(copyFrame) > 0)
      {
        analyzedCoordinates <- over(copyCoords, copyShape)
        
        inBounds <- analyzedCoordinates[is.na(analyzedCoordinates$Shape_Leng) | is.na(analyzedCoordinates$Shape_Area),]
        
        index <- rownames(inBounds)
        
        outputFrame <- copyFrame[rownames(copyFrame) %in% index,]
        
        logNames <- names(dataList) == varname
        listSum <- sum(logNames)
        
        if(listSum > 0)
        {
          dataList[[varname]] <- rbind(dataList[[varname]], outputFrame)
        }
        else
        {
          dataList[[varname]] <- outputFrame
        } 
        
      }
    }
  }
  
  return(dataList)
}

#Testing Working Code ---- 

#selection for points that are exsiting do equals NA if wanting number of out of bound points
t <- over(testGPS, shape)
test <- t[!is.na(t$Shape_Leng),]
index <- rownames(test)

testFrame <- dataIn[[1]]

testCoords <- testFrame[rownames(testFrame) %in% index,]


#Loading Movement Callendar ----
loadMovement <- function()
{
  directoryPath <- paste("raw/", "events", "/", sep="")
  fileList <- list.files(directoryPath)
  filePath <- paste(directoryPath, fileList[2], sep="") 
  
  pens <- read.csv(filePath, stringsAsFactors = FALSE)
  pens$EndDate[pens$EndDate == "END"] <- '20-Sep-20'
  
  pens$EndDate <- as.Date(pens$EndDate, format = '%d-%b-%y')
  pens$StartDate <- as.Date(pens$StartDate, format = '%d-%b-%y')
  
  pens <- pens[pens$Collar != "N/A",] 
  
  return(pens)
}


#Loading Shape Functions ---- 
loadShapeFiles <- function(path)
{
  directoryPath <- path
  fileList <- list.files(directoryPath, pattern = "\\.shp$")
  fileNames <- strsplit(fileList, "[.]")
  
  shapeList <- list()
  
  for(i in 1:length(fileNames))
  {
    name <- fileNames[[i]][1]
    shapeList[[name]] <- readOGR(dsn = directoryPath, layer = name)
  }
  
  return(shapeList)
}


#GPS Coordinate Change ----
loadGPSCoords <- function(dataIn, sharedCRS)
{
  dataNames <- names(dataIn)
  gpsCoordiantes <- list()
  
  for(i in 1:length(dataIn))
  {
    name <- dataNames[i]
    copyFrame <- dataIn[[name]]
    
    copyFrame <- copyFrame[c('Longitude', "Latitude")]
    coordinates(copyFrame) <- cbind(copyFrame$Longitude, copyFrame$Latitude)
    
    proj4string(copyFrame) <- sharedCRS
    
    gpsCoordiantes[[name]] <- copyFrame
  }
  
  return(gpsCoordiantes)
}


#GPS Zero Extraction & Filtering
extractZeroGPS <- function(dataIn) 
{
  dataList <- list()
  
  for(i in 1:length(dataIn))
  {
    copyFrame <- dataIn[[i]]
    
    fileID <- levels(copyFrame$CollarID[1])
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    copyFrame <- copyFrame[copyFrame$Latitude == 0 | copyFrame$Longitude == 0 | copyFrame$Altitude == 0,]
    
    logNames <- names(dataList) == varname
    listSum <- sum(logNames)
    
    if(listSum > 0)
    {
      dataList[[varname]] <- rbind(dataList[[varname]], copyFrame)
    }
    else
    {
      dataList[[varname]] <- copyFrame
    }   
  }
  return(dataList)
}

filterZeroGPS <- function(dataIn) 
{
  dataList <- list()
  
  for(i in 1:length(dataIn))
  {
    copyFrame <- dataIn[[i]]
    
    fileID <- levels(copyFrame$CollarID[1])
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    copyFrame <- copyFrame[copyFrame$Latitude != 0 | copyFrame$Longitude != 0 | copyFrame$Altitude != 0,]
    
    logNames <- names(dataList) == varname
    listSum <- sum(logNames)
    
    if(listSum > 0)
    {
      dataList[[varname]] <- rbind(dataList[[varname]], copyFrame)
    }
    else
    {
      dataList[[varname]] <- copyFrame
    }   
  }
  return(dataList)
}

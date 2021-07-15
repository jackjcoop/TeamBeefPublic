#Loading Libraries ----
library(tidyverse)
library(lubridate)
library(geosphere)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)

require(tidyverse)
require(lubridate)
require(rgdal)
require(raster)
require(sp)
require(rgeos)

source("loadData.R")
#rm(list=ls())

#Testing Code ---- 



#Calendar Loading Functions ----
loadCalendar <- function()
{
  directoryPath <- paste("raw/", "events", "/", sep="")
  fileList <- list.files(directoryPath)
  filePath <- paste(directoryPath, fileList[1], sep="") 
  
  events <- read.csv(filePath, stringsAsFactors = FALSE)
  events$Collar_On <- as.POSIXct(events$Collar_On)
  events$Collar_Off <- as.POSIXct(events$Collar_Off)
  
  return(events)
}



#Calendar Assignment Functions ----
filterCalendar <- function(dataIn)
{
  eventCalendar <- loadCalendar()
  eventList <- list()
  
  
  for(i in 1:nrow(eventCalendar))
  {
    onTime <- eventCalendar$Collar_On[i]
    offTime <- eventCalendar$Collar_Off[i]
    fileID <- as.factor(eventCalendar$Collar_ID[i])
    BullID <- as.factor(eventCalendar$Bull_ID[i])
    
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    copyFrame <- dataIn[[varname]]
    copyFrame <- copyFrame[onTime < copyFrame$DateTime & offTime > copyFrame$DateTime, ]
    
    if(is.null(copyFrame))
    {}
    else if(nrow(copyFrame) > 0)
    {
      copyFrame <- transform(copyFrame, BullID = BullID)
      
      logNames <- names(eventList) == varname
      listSum <- sum(logNames)
      
      if(listSum > 0)
      {
        eventList[[varname]] <- rbind(eventList[[varname]], copyFrame)
      }
      else
      {
        eventList[[varname]] <- copyFrame
      }
    }
    
  }
  
  return(eventList)
}


#DateTime Difference Function ----
speedAndTimeDifference <- function(dataIn)
{
  dataList <- list()
  
  for(i in 1:length(dataIn))
  {
    copyFrame <- dataIn[[i]]
    
    if(is.null(copyFrame))
    {}
    else if(nrow(copyFrame) > 0)
    {
      fileID <- levels(copyFrame$CollarID[1])
      prefix <- "ID"
      varname <- paste(prefix, fileID, sep="_")
      
      timeFirst <-  copyFrame[1:(nrow(copyFrame) - 1), ]
      timeSecond <- copyFrame[2:nrow(copyFrame), ]
      
      p1 <-  copyFrame[1:(nrow(copyFrame) - 1), ]
      p2 <- copyFrame[2:nrow(copyFrame), ]
      
      #Time Difference 
      timeFirst <- as.numeric(timeFirst$DateTime)
      timeSecond <- as.numeric(timeSecond$DateTime)
      
      time <- timeSecond - timeFirst
      copyFrame <- copyFrame[2:nrow(copyFrame), ]
      
      copyFrame <- cbind(copyFrame, TimeDifference = time)
      
      #Speed
      # p1GPS <- p1 %>% select(Latitude, Longitude)
      # p2GPS <- p2 %>% select(Latitude, Longitude)
      
      p1GPS <- p1[c('Longitude', "Latitude")]
      p2GPS <- p2[c('Longitude', "Latitude")]
      
      distance <- distGeo(p1GPS, p2GPS)
      speed <- abs(distance / time)
      
      copyFrame <- cbind(copyFrame, Speed = speed)
      copyFrame$DateTime <- date(copyFrame$DateTime)
      
      #Loading In 
      
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
    
    else if(nrow(copyFrame) == 0)
    {
      varname <- names(dataIn[i])
      
      copyFrame <- cbind(copyFrame, TimeDifference = numeric(0))
      copyFrame <- cbind(copyFrame, Speed = numeric(0))
      
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
  }
  
  return(dataList)
}

timeDifference <- function(dataIn)
{
  dataList <- list()
  
  for(i in 1:length(dataIn))
  {
    copyFrame <- dataIn[[i]]
    
    fileID <- levels(copyFrame$CollarID[1])
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    timeFirst <-  copyFrame[1:(nrow(copyFrame) - 1), ]
    timeSecond <- copyFrame[2:nrow(copyFrame), ]
    
    
    #Time Difference 
    timeFirst <- as.numeric(timeFirst$DateTime)
    timeSecond <- as.numeric(timeSecond$DateTime)
    
    time <- timeSecond - timeFirst
    copyFrame <- copyFrame[2:nrow(copyFrame), ]
    
    copyFrame <- cbind(copyFrame, TimeDifference = time)
    copyFrame$DateTime <- date(copyFrame$DateTime)
    
    #Loading In 
    
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


#GPS Zero Filter Functions ---- 
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

#Errenous Dates Selection Function ---- 

#this kind of works but there is a lot that it may be missing. 
#should look into a little more
extractErrenousDates <- function(dataIn) 
{
  dataList <- list()
  
  for(i in 1:length(dataIn))
  {
    copyFrame <- dataIn[[i]]
    
    fileID <- levels(copyFrame$CollarID[1])
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    if(nrow(copyFrame) > 0)
      copyFrame <- copyFrame[copyFrame$TimeDifference < 0 | copyFrame$TimeDifference >= 3600,]
    
    
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

filterErrenousDates <- function(dataIn) 
{
  dataList <- list()
  
  for(i in 1:length(dataIn))
  {
    copyFrame <- dataIn[[i]]
    
    fileID <- levels(copyFrame$CollarID[1])
    prefix <- "ID"
    varname <- paste(prefix, fileID, sep="_")
    
    if(nrow(copyFrame) > 0)
      copyFrame <- copyFrame[copyFrame$TimeDifference > 0 & copyFrame$TimeDifference < 3600,]
    
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

#Early, Late and Missing Fixes ----
gpsFixCheck <- function(dataIn)
{
  eventCalendar <- loadCalendar()
  dataList <- list()
  
  for(j in 1:nrow(eventCalendar))
  {
    
    outputFrame <- data.frame(Dates = character(0), Expected_Fixes = numeric(0), On_Time_Fix = numeric(0), Early_Fix = numeric(0), Late_Fix = numeric(0), Missing_Fix = numeric(0), CollarID = numeric(0), BullID = numeric(0))
    
    start <- eventCalendar$Collar_On[j] 
    end <- eventCalendar$Collar_Off[j]
    
    expectedFixes <- data.frame(dates = seq(start, end, by = 300))
    dateSequence <- data.frame(dates = seq(as.Date(start), as.Date(end), by="days"))
    
    CollarID <- as.factor(eventCalendar$Collar_ID[j])
    BullID <- as.factor(eventCalendar$Bull_ID[j])
    
    prefix <- "ID"
    varname <- paste(prefix, CollarID, sep="_")
    
    copyFrame <- dataIn[[varname]]
    
    
    for(i in 1:nrow(dateSequence))
    {
      dateFrame <- copyFrame[copyFrame$DateTime == dateSequence$dates[i], ]
      expFix <- length(expectedFixes[as.Date(expectedFixes$dates) == dateSequence$dates[i], ] )
      
      date <- dateSequence$dates[i]
      
      if(is.null(copyFrame))
      {
        fix <- 0 
        early <- 0 
        late <- 0 
        missing <- expFix
      }
      else if(nrow(copyFrame) == 0)
      {
        fix <- 0 
        early <- 0 
        late <- 0 
        missing <- expFix
      }
      else if(nrow(copyFrame) > 0)
      {
        fix <- nrow(dateFrame[dateFrame$TimeDifference >= 270 & dateFrame$TimeDifference <= 330, ])
        early <- nrow(dateFrame[dateFrame$TimeDifference < 270, ])
        late <- nrow(dateFrame[dateFrame$TimeDifference > 330, ])
        
        missing <- expFix - (fix + early + late)
      }
      
      holdFrame <- data.frame(date, expFix, fix, early, late, missing, CollarID, BullID)
      outputFrame <- rbind(outputFrame, holdFrame)
    }
    
    #change date when dealing with pre-breeding season 2020-06-18, 07-13 and 09-20
    outputFrame <- outputFrame[as.Date(outputFrame$date) >= "2020-07-13" & as.Date(outputFrame$date) <= "2020-09-20", ]
    outputFrame <- arrange(outputFrame, date)
    
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
  return(dataList)
}

#GPS Histogram Plotting ---- 
gpsHistograms <- function(dataIn)
{
  for(j in 1:length(dataIn))
  {
    #set to i in the loop 
    copyFrame <- dataIn[[j]]
    
    #Binwidth by Freedman-Diaconis Rule 
    #change type depending on graph
    for(i in 1:length(levels(copyFrame$BullID)))
    {
      CollarID <- levels(copyFrame$CollarID)
      CollarID <- CollarID[1]
      
      BullID <- levels(copyFrame$BullID)
      BullID <- BullID[i]
      copyFrame <- copyFrame[copyFrame$BullID == BullID, ]
      
      titleFix <- paste("Collar", CollarID, "Bull", BullID, "Histogram of Fix Rate", sep=" ")
      titleNoFix <- paste("Collar", CollarID, "Bull", BullID, "Histogram of No Fix Rate", sep=" ")
      titleMissing <- paste("Collar", CollarID, "Bull", BullID, "Histogram of Missing Fix Rate", sep=" ")
      
      #The Freedman-Diaconis RUle for Bin Width Calculation 
      # x <- copyFrame$missing
      # bw <- 2 * IQR(x) / length(x)^(1/3)
      
      print(qplot(copyFrame$fix,
                  geom="histogram",
                  binwidth=20,  
                  main=titleFix, 
                  xlab="Daily Number of On Time Fixes", 
                  fill=I("blue"), 
                  col=I("black")))
      
      print(qplot(copyFrame$noFix,
                  geom="histogram",
                  binwidth=10,  
                  main=titleNoFix, 
                  xlab="Daily Number of No Fixes", 
                  fill=I("cyan"), 
                  col=I("black")))
      
      print(qplot(copyFrame$missing,
                  geom="histogram",
                  binwidth=20,  
                  main=titleMissing, 
                  xlab="Daily Number of Missing Fixes", 
                  fill=I("green"), 
                  col=I("black")))
    }
  }
}

#GPS Pie Chart ----
gpsPie <- function(dataIn)
{
  for(j in 1:length(dataIn))
  {
    copyFrame <- dataIn[[j]]
    
    #add condition to break if a level is NA
    
    for(i in 1:length(levels(copyFrame$BullID)))
    {
      CollarID <- levels(copyFrame$CollarID)
      CollarID <- CollarID[1]
      
      BullID <- levels(copyFrame$BullID)
      BullID <- BullID[i]
      copyFrame <- copyFrame[copyFrame$BullID == BullID, ]
      
      fix <- mean(copyFrame$fix) / mean(copyFrame$expFix) * 100
      nofix <- mean(copyFrame$noFix) / mean(copyFrame$expFix) * 100
      earlyfix <- mean(copyFrame$early) / mean(copyFrame$expFix) * 100
      latefix <- mean(copyFrame$late) / mean(copyFrame$expFix) * 100
      missingfix <- mean(copyFrame$missing) / mean(copyFrame$expFix) * 100
      outfix <- mean(copyFrame$outBounds) / mean(copyFrame$expFix) * 100
      
      data <- data.frame(Type = c("Fix", "noFix", "earlyFix", "lateFix", "missingFix", "outOfBounds"), 
                         values = c(fix, nofix, earlyfix, latefix, missingfix, outfix))
      
      data <- cbind(data, labels = signif(data$values, digits = 4))
      data$label <- paste0(data$label, "%")
      
      
      title <- paste("Collar", CollarID, "Bull", BullID, "Pie Chart Average % Summary Post-Breeding", sep=" ")
      
      p <- ggplot(data,aes(x=1,y=values,fill=Type)) + geom_bar(stat="identity", color = "black")
      
      p <- p + coord_polar(theta='y')+ theme(axis.ticks=element_blank(),
                                             axis.text.y=element_blank(),
                                             axis.text.x=element_text(colour='black'),
                                             axis.title=element_blank())
      
      # p <- p + scale_y_continuous(breaks=cumsum(data$values) - data$values / 2, labels= data$label)
      
      p <- p + ggtitle(title)
      
      print(p)
      
    }
  }
}
#Acclerometer Summarization ---- 
acclAnalysis <- function(dataIn)
{
  eventCalendar <- loadCalendar()
  dataList <- list()
  
  for(j in 1:nrow(eventCalendar))
  {
    outputFrame <- data.frame(Dates = character(0), Daily_Frequency = numeric(0), Unexpected_Freqeuncy = numeric(0), Percent_Missing = numeric(0), avgX = numeric(0), avgY = numeric(0), avgZ = numeric(0))
    
    start <- eventCalendar$Collar_On[j] 
    end <- eventCalendar$Collar_Off[j]
    
    expectedFixes <- data.frame(dates = seq(start, end, by = 2))
    dateSequence <- data.frame(dates = seq(as.Date(start), as.Date(end), by="days"))
    
    CollarID <- as.factor(eventCalendar$Collar_ID[j])
    BullID <- as.factor(eventCalendar$Bull_ID[j])
    
    prefix <- "ID"
    varname <- paste(prefix, CollarID, sep="_")
    
    copyFrame <- dataIn[[varname]]
    
    for(i in 1:nrow(dateSequence))
    {
      dateFrame <- copyFrame[copyFrame$DateTime == dateSequence$dates[i], ]
      expFix <- length(expectedFixes[as.Date(expectedFixes$dates) == dateSequence$dates[i], ] )
      
      date <- dateSequence$dates[i]
      
      if(nrow(dateFrame) == 0 | is.null(dateFrame))
      {
        avgFreqeuncy <- 0
        unexpectedFrequency <- 0 
        perMissing <- 100 
        avgX <- 0 
        avgY <- 0 
        avgZ <- 0 
      }
      else
      {
        avgFreqeuncy <- mean(dateFrame$TimeDifference)
        unexpectedFrequency <- nrow(dateFrame[dateFrame$TimeDifference != 2, ]) 
        perMissing <- ((expFix - nrow(dateFrame)) / expFix) * 100 
        
        avgX <- mean(dateFrame$X)
        avgY <- mean(dateFrame$Y)
        avgZ <- mean(dateFrame$Z)
        
      }
      
      holdFrame <- data.frame(date, avgFreqeuncy, unexpectedFrequency, perMissing, avgX, avgY, avgZ, CollarID, BullID)
      outputFrame <- rbind(outputFrame, holdFrame)
    }
    
    
    
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
  return(dataList)
}

#Summarization ----
gpsSummary <- function(results, badDates)
{
  outputFrame <- data.frame(ID = character(0), expFix = numeric(0), Fix = numeric(0), noFix = numeric(0), early = numeric(0), late = numeric(0), missing = numeric(0), outBounds = numeric(0), errenousDates = numeric(0))
  
  
  for(j in 1:length(results))
  {
    copyFrameResults <- results[[j]]
    
    CollarID <- as.factor(copyFrameResults$CollarID[1])
    prefix <- "ID"
    varname <- paste(prefix, CollarID, sep="_")
    
    copyFrameDates <- badDates[[varname]]
    
    for(i in 1:length(levels(copyFrameResults$BullID)))
    {
      
      BullID <- levels(copyFrameResults$BullID)
      BullID <- BullID[i]
      holdFrameResults <- copyFrameResults[copyFrameResults$BullID == BullID, ]
      holdFrameDates <- copyFrameDates[copyFrameDates$BullID == BullID, ]
      
      if(is.null(holdFrameResults) | is.null(holdFrameDates))
      {}
      else if(nrow(holdFrameResults) == 0)
      {
        expFix <- 0
        fix <- 0 
        noFix <- 0 
        early <- 0 
        late <- 0
        missing <- 0
        outBounds <- 0 
        errenousDates <- nrow(holdFrameDates)
      }
      else if(nrow(holdFrameResults) > 0)
      {
        expFix <- sum(holdFrameResults$expFix)
        fix <- sum(holdFrameResults$fix)
        noFix <- sum(holdFrameResults$noFix) 
        early <- sum(holdFrameResults$early) 
        late <- sum(holdFrameResults$late)
        missing <- sum(holdFrameResults$missing)
        outBounds <- sum(holdFrameResults$outBounds)
        errenousDates <- nrow(holdFrameDates)
      }
      
      ID <- paste("CollarID", CollarID, "BullID", BullID, sep=" ")
      
      holdFrame <- data.frame(ID, expFix, fix, noFix, early, late, missing, outBounds, errenousDates)
      outputFrame <- rbind(outputFrame, holdFrame)
    }
  }
  
  sumFix <- (outputFrame$fix + outputFrame$noFix +  outputFrame$early + outputFrame$late + abs(outputFrame$missing) + outputFrame$outBounds + outputFrame$errenousDates)
  
  outputFrame$fix <- outputFrame$fix / sumFix * 100
  outputFrame$noFix <- outputFrame$noFix / sumFix * 100
  outputFrame$early <- outputFrame$early / sumFix * 100
  outputFrame$late <- outputFrame$late / sumFix * 100
  outputFrame$missing <- outputFrame$missing / sumFix * 100
  outputFrame$outBounds <- outputFrame$outBounds / sumFix * 100
  outputFrame$errenousDates <- outputFrame$errenousDates / sumFix * 100
  
  return(outputFrame)
}

#GPS Summary Raw ---- 
gpsSummaryRaw <- function(results, badDates, zeros, out)
{
  outputFrame <- data.frame(ID = character(0), expFix = numeric(0), Fix = numeric(0), early = numeric(0), late = numeric(0), missing = numeric(0), errenousDates = numeric(0), zeros = numeric(0), outBounds = numeric(0))
  
  
  for(j in 1:length(results))
  {
    copyFrame <- results[[j]]
    
    for(i in 1:length(levels(copyFrame$BullID)))
    {
      CollarID <- levels(copyFrame$CollarID)
      CollarID <- CollarID[1]
      prefix <- "ID"
      varname <- paste(prefix, CollarID, sep="_")
      
      BullID <- levels(copyFrame$BullID)
      BullID <- BullID[i]
      
      copyFrameDates <- badDates[[varname]]
      copyFrameDates <- copyFrameDates[copyFrameDates$BullID == BullID, ]
      
      
      copyFrameZeros <- zeros[[varname]]
      copyFrameZeros <- copyFrameZeros[copyFrameZeros$BullID == BullID, ]
      
      copyFrameOut <- out[[varname]]
      copyFrameOut <- copyFrameOut[copyFrameOut$BullID == BullID, ]
      
      copyFrameResults <- copyFrame[copyFrame$BullID == BullID, ]
      
      if(is.null(copyFrameResults) | nrow(copyFrameResults) == 0) 
      {
        expFix <- 0
        fix <- 0
        early <- 0
        late <- 0
        missing <- 0
      }
      else 
      {
        expFix <- sum(copyFrameResults$expFix)
        fix <- sum(copyFrameResults$fix)
        early <- sum(copyFrameResults$early) 
        late <- sum(copyFrameResults$late)
        # missing <- sum(copyFrameResults$missing)
      }
      
      if(is.null(copyFrameDates)) 
      {
        errenousDates <- 0 
      }
      else 
      {
        errenousDates <- nrow(copyFrameDates)
      }
      
      if(is.null(copyFrameZeros)) 
      {
        noFix <- 0 
      }
      else 
      {
        noFix <- nrow(copyFrameZeros)
      }
      
      if(is.null(copyFrameOut))
      {
        outBounds <- 0
      }
      else
      {
        outBounds <- nrow(copyFrameOut)
      }
      
      missing <- expFix - (fix + early + late + errenousDates + noFix + outBounds)
      
      ID <- paste("CollarID", CollarID, "BullID", BullID, sep=" ")
      
      holdFrame <- data.frame(ID, expFix, fix, early, late, missing, errenousDates, noFix, outBounds)
      outputFrame <- rbind(outputFrame, holdFrame)
      
    }
    
  }
  # 
  # sumFix <- (outputFrame$fix +  outputFrame$early + outputFrame$late + abs(outputFrame$missing) + outputFrame$errenousDates + outputFrame$noFix + outputFrame$outBounds)
  # 
  # outputFrame$fix <- outputFrame$fix / sumFix * 100
  # outputFrame$early <- outputFrame$early / sumFix * 100
  # outputFrame$late <- outputFrame$late / sumFix * 100
  # outputFrame$missing <- outputFrame$missing / sumFix * 100
  # outputFrame$errenousDates <- outputFrame$errenousDates / sumFix * 100
  # outputFrame$noFix <- outputFrame$noFix / sumFix * 100
  # outputFrame$outBounds <- outputFrame$outBounds / sumFix * 100
  
  return(outputFrame)
}

gpsSummaryCombine <- function(data1, data2)
{
  ID <- data1$ID
  expFix <- data1$expFix + data2$expFix
  fix <- data1$fix + data2$fix
  early <- data1$early + data2$early
  late <- data1$late + data2$late
  missing <- data1$missing + data2$missing
  errenousDates <- data1$errenousDates + data2$errenousDates
  noFix <- data1$noFix + data2$noFix
  outBounds <- data1$outBounds + data2$outBounds
  # 
  outputFrame <- data.frame(ID, expFix, fix, early, late, missing, errenousDates, noFix, outBounds)
  # 
  sumFix <- (outputFrame$fix +  outputFrame$early + outputFrame$late + abs(outputFrame$missing) + outputFrame$errenousDates + outputFrame$noFix + outputFrame$outBounds)
  #
  outputFrame$fix <- outputFrame$fix / sumFix * 100
  outputFrame$early <- outputFrame$early / sumFix * 100
  outputFrame$late <- outputFrame$late / sumFix * 100
  outputFrame$missing <- outputFrame$missing / sumFix * 100
  outputFrame$errenousDates <- outputFrame$errenousDates / sumFix * 100
  outputFrame$noFix <- outputFrame$noFix / sumFix * 100
  outputFrame$outBounds <- outputFrame$outBounds / sumFix * 100
  # 
  # 
  return(outputFrame)
  # 
}

test <- gpsSummaryCombine(gpsSummaryPre, gpsSummaryPre)

#GPS Out of Bounds ----
filterOutBounds <- function(dataIn)
{
  dataList <- list()
  movementCalendar <- loadMovement()
  
  shapes <- loadShapeFiles("raw/bounds")
  sharedCRS <- proj4string(shapes[[1]])
  
  gpsCoords <- loadGPSCoordsList(dataIn, sharedCRS)
  
  for(j in 1:nrow(movementCalendar))
  {
    copyCalendar <- movementCalendar[j, ]
    
    CollarID <- copyCalendar$Collar
    BullID <- copyCalendar$Bull
    
    pen <- copyCalendar$arcGISShapeFileName
    penList <- strsplit(pen, ", ")
    
    prefix <- "ID"
    varname <- paste(prefix, CollarID, sep="_")
    
    copyFrame <- dataIn[[varname]]
    copyCoords <- gpsCoords[[varname]]
    
    if(is.null(copyFrame))
    {}
    else if(nrow(copyFrame) > 0)
    {
      copyFrame <- copyFrame[copyFrame$BullID == BullID, ] 
      copyFrame <- copyFrame[copyCalendar$StartDate <= as.Date(copyFrame$DateTime) & copyCalendar$EndDate >= as.Date(copyFrame$DateTime), ]
      
      for(i in 1:length(penList[[1]]))
      {
        penName <- penList[[1]][i]
        copyShape <- shapes[[penName]]
        
        if(i == 1)
        {
          outputFrame <- over(copyCoords, copyShape)
        }
        else if(i > 1)
        {
          if("CN_SE" != penName)
          {
            holdFrame <- over(copyCoords, copyShape)
            outputFrame <- rbind(outputFrame, holdFrame)
          }
        }
      }
      
      outBounds <- outputFrame[!is.na(outputFrame$Shape_Leng) | !is.na(outputFrame$Shape_Area), ]
      
      index <- rownames(outBounds)
      
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
  return(dataList)
}


extractOutBounds <- function(dataIn)
{
  dataList <- list()
  movementCalendar <- loadMovement()
  
  shapes <- loadShapeFiles("raw/bounds")
  sharedCRS <- proj4string(shapes[[1]])
  
  gpsCoords <- loadGPSCoordsList(dataIn, sharedCRS)
  
  for(j in 1:nrow(movementCalendar))
  {
    copyCalendar <- movementCalendar[j, ]
    
    CollarID <- copyCalendar$Collar
    BullID <- copyCalendar$Bull
    
    pen <- copyCalendar$arcGISShapeFileName
    penList <- strsplit(pen, ", ")
    
    prefix <- "ID"
    varname <- paste(prefix, CollarID, sep="_")
    
    copyFrame <- dataIn[[varname]]
    copyCoords <- gpsCoords[[varname]]
    
    if(is.null(copyFrame))
    {}
    else if(nrow(copyFrame) > 0)
    {
      copyFrame <- copyFrame[copyFrame$BullID == BullID, ] 
      copyFrame <- copyFrame[copyCalendar$StartDate <= as.Date(copyFrame$DateTime) & copyCalendar$EndDate >= as.Date(copyFrame$DateTime), ]
      
      for(i in 1:length(penList[[1]]))
      {
        penName <- penList[[1]][i]
        copyShape <- shapes[[penName]]
        
        if(i == 1)
        {
          outputFrame <- over(copyCoords, copyShape)
        }
        else if(i > 1)
        {
          if("CN_SE" != penName)
          {
            holdFrame <- over(copyCoords, copyShape)
            outputFrame <- rbind(outputFrame, holdFrame)
          }
        }
      }
      
      outBounds <- outputFrame[is.na(outputFrame$Shape_Leng) | is.na(outputFrame$Shape_Area),]
      
      index <- rownames(outBounds)
      
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
  return(dataList)
}

OutBounds <- function(dataIn)
{
  dataList <- list()
  movementCalendar <- loadMovement()
  
  shapes <- loadShapeFiles("raw/bounds")
  sharedCRS <- proj4string(shapes[[1]])
  
  gpsCoords <- loadGPSCoords(dataIn, sharedCRS)
  
  for(j in 1:nrow(movementCalendar))
  {
    copyCalendar <- movementCalendar[j, ]
    
    CollarID <- copyCalendar$Collar
    BullID <- copyCalendar$Bull
    
    pen <- copyCalendar$arcGISShapeFileName
    penList <- strsplit(pen, ", ")
    
    prefix <- "ID"
    varname <- paste(prefix, CollarID, sep="_")
    
    copyFrame <- dataIn[[varname]]
    copyCoords <- gpsCoords[[varname]]
    
    if(is.null(copyFrame))
    {}
    else if(nrow(copyFrame) > 0)
    {
      copyFrame <- copyFrame[copyFrame$BullID == BullID, ] 
      copyFrame <- copyFrame[copyCalendar$StartDate <= as.Date(copyFrame$DateTime) & copyCalendar$EndDate >= as.Date(copyFrame$DateTime), ]
      
      for(i in 1:length(penList[[1]]))
      {
        penName <- penList[[1]][i]
        copyShape <- shapes[[penName]]
        
        if(i == 1)
        {
          outputFrame <- over(copyCoords, copyShape)
        }
        else if(i > 1)
        {
          if("CN_SE" != penName)
          {
            holdFrame <- over(copyCoords, copyShape)
            outputFrame <- rbind(outputFrame, holdFrame)
          }
        }
      }
      
      outBounds <- outputFrame[is.na(outputFrame$Shape_Leng) | is.na(outputFrame$Shape_Area),]
      
      index <- rownames(outBounds)
      
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
  return(dataList)
}




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
loadGPSCoordsList <- function(dataIn, sharedCRS)
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

loadGPSCoordsFrame <- function(dataIn, sharedCRS)
{
  copyFrame <- dataIn
  
  copyFrame <- copyFrame[c('Longitude', "Latitude")]
  coordinates(copyFrame) <- cbind(copyFrame$Longitude, copyFrame$Latitude)
  
  proj4string(copyFrame) <- sharedCRS
  
  return(copyFrame)
}

#GPS Zero Extraction & Filtering ----
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

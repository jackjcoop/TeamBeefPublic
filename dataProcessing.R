#Library Loading ----

library(tidyverse)
#library(anytime)
#library(plyr)
#library(dplyr)
#library(readr)
library(lubridate)
library(geosphere)

#to remove all variables when needed
#rm(list=ls())


#Data Loading ----
# PGdf1 <- read.csv(file = 'raw/gps/post/PinPoint 80378 2020-11-09 10-57-44.csv', header = TRUE)
# PGdf2 <- read.csv(file = 'raw/gps/post/PinPoint 80379 2020-11-09 13-05-18.csv', header = TRUE)
# PGdf3 <- read.csv(file = 'raw/gps/post/PinPoint 80380 2020-11-09 13-18-22.csv', header = TRUE)
# PGdf4 <- read.csv(file = 'raw/gps/post/PinPoint 80381 2020-11-09 13-48-25.csv', header = TRUE)
# PGdf5 <- read.csv(file = 'raw/gps/post/PinPoint 80382 2020-11-09 12-11-25.csv', header = TRUE)
# PGdf6 <- read.csv(file = 'raw/gps/post/PinPoint 80383 2020-11-09 11-31-51.csv', header = TRUE)
# PGdf7 <- read.csv(file = 'raw/gps/post/PinPoint 80384 2020-11-05 15-35-36.csv', header = TRUE)
# PGdf8 <- read.csv(file = 'raw/gps/post/PinPoint 80385 2020-11-05 13-28-27.csv', header = TRUE)
# PGdf9 <- read.csv(file = 'raw/gps/post/PinPoint 80386 2020-11-09 15-35-23.csv', header = TRUE)
# PGdf10 <- read.csv(file = 'raw/gps/post/PinPoint 80387 2020-11-09 11-45-02.csv', header = TRUE)
# PGdf11 <- read.csv(file = 'raw/gps/post/PinPoint 80388 2020-11-09 14-28-51.csv', header = TRUE)
# PGdf12 <- read.csv(file = 'raw/gps/post/PinPoint 80389 2020-11-09 10-05-18.csv', header = TRUE)
# PGdf13 <- read.csv(file = 'raw/gps/post/PinPoint 80390 2020-11-05 14-37-10.csv', header = TRUE)
# PGdf14 <- read.csv(file = 'raw/gps/post/PinPoint 80391 2020-11-09 12-39-21.csv', header = TRUE)

PGdf1 <- read.csv(file = 'raw/gps/pre/PreGPS1.csv', header = TRUE)
PGdf2 <- read.csv(file = 'raw/gps/pre/PreGPS2.csv', header = TRUE)
PGdf3 <- read.csv(file = 'raw/gps/pre/PreGPS3.csv', header = TRUE)
PGdf4 <- read.csv(file = 'raw/gps/pre/PreGPS4.csv', header = TRUE)
PGdf5 <- read.csv(file = 'raw/gps/pre/PreGPS5.csv', header = TRUE)
PGdf6 <- read.csv(file = 'raw/gps/pre/PreGPS6.csv', header = TRUE)
PGdf7 <- read.csv(file = 'raw/gps/pre/PreGPS7.csv', header = TRUE)
PGdf8 <- read.csv(file = 'raw/gps/pre/PreGPS8.csv', header = TRUE)
PGdf9 <- read.csv(file = 'raw/gps/pre/PreGPS9.csv', header = TRUE)
PGdf10 <- read.csv(file = 'raw/gps/pre/PreGPS10.csv', header = TRUE)
PGdf11 <- read.csv(file = 'raw/gps/pre/PreGPS11.csv', header = TRUE)
PGdf12 <- read.csv(file = 'raw/gps/pre/PreGPS12.csv', header = TRUE)
# PGdf13 <- read.csv(file = 'raw/gps/post/PinPoint 80390 2020-11-05 14-37-10.csv', header = TRUE)
# PGdf14 <- read.csv(file = 'raw/gps/post/PinPoint 80391 2020-11-09 12-39-21.csv', header = TRUE)


#Calling MST Conversion Functions ----
PGdf1 <- mstConversion(PGdf1)
PGdf2 <- mstConversion(PGdf2)
PGdf3 <- mstConversion(PGdf3)
PGdf4 <- mstConversion(PGdf4)
PGdf5 <- mstConversion(PGdf5)
PGdf6 <- mstConversion(PGdf6)
PGdf7 <- mstConversion(PGdf7)
PGdf8 <- mstConversion(PGdf8)
PGdf9 <- mstConversion(PGdf9)
PGdf10 <- mstConversion(PGdf10)
PGdf11 <- mstConversion(PGdf11)
PGdf12 <- mstConversion(PGdf12)
# PGdf13 <- mstConversion(PGdf13)
# PGdf14 <- mstConversion(PGdf14)

#Calling Analyze Fix Functions ----
RPGdf1 <- analyzeFixes(PGdf1)
RPGdf2 <- analyzeFixes(PGdf2)
RPGdf3 <- analyzeFixes(PGdf3)
RPGdf4 <- analyzeFixes(PGdf4)
RPGdf5 <- analyzeFixes(PGdf5)
RPGdf6 <- analyzeFixes(PGdf6)
RPGdf7 <- analyzeFixes(PGdf7)
RPGdf8 <- analyzeFixes(PGdf8)
RPGdf9 <- analyzeFixes(PGdf9)
RPGdf10 <- analyzeFixes(PGdf10)
RPGdf11 <- analyzeFixes(PGdf11)
RPGdf12 <- analyzeFixes(PGdf12)
# RPGdf13 <- analyzeFixes(PGdf13)
# RPGdf14 <- analyzeFixes(PGdf14)

#Data Output ----
# write.csv(RPGdf1, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost1GPS.csv", row.names = FALSE)
# write.csv(RPGdf2, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost2GPS.csv", row.names = FALSE)
# write.csv(RPGdf3, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost3GPS.csv", row.names = FALSE)
# write.csv(RPGdf4, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost4GPS.csv", row.names = FALSE)
# write.csv(RPGdf5, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost5GPS.csv", row.names = FALSE)
# write.csv(RPGdf6, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost6GPS.csv", row.names = FALSE)
# write.csv(RPGdf7, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost7GPS.csv", row.names = FALSE)
# write.csv(RPGdf8, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost8GPS.csv", row.names = FALSE)
# write.csv(RPGdf9, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost9GPS.csv", row.names = FALSE)
# write.csv(RPGdf10, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost10GPS.csv", row.names = FALSE)
# write.csv(RPGdf11, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost11GPS.csv", row.names = FALSE)
# write.csv(RPGdf12, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost12GPS.csv", row.names = FALSE)
# write.csv(RPGdf13, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost13GPS.csv", row.names = FALSE)
# write.csv(RPGdf14, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost14GPS.csv", row.names = FALSE)

write.csv(RPGdf1, "F:/Development/Projects/Research/TeamBeef/workingProject/output/pre/APre1GPS.csv", row.names = FALSE)
write.csv(RPGdf2, "F:/Development/Projects/Research/TeamBeef/workingProject/output/pre/APre2GPS.csv", row.names = FALSE)
write.csv(RPGdf3, "F:/Development/Projects/Research/TeamBeef/workingProject/output/pre/APre3GPS.csv", row.names = FALSE)
write.csv(RPGdf4, "F:/Development/Projects/Research/TeamBeef/workingProject/output/pre/APre4GPS.csv", row.names = FALSE)
write.csv(RPGdf5, "F:/Development/Projects/Research/TeamBeef/workingProject/output/pre/APre5GPS.csv", row.names = FALSE)
write.csv(RPGdf6, "F:/Development/Projects/Research/TeamBeef/workingProject/output/pre/APre6GPS.csv", row.names = FALSE)
write.csv(RPGdf7, "F:/Development/Projects/Research/TeamBeef/workingProject/output/pre/APre7GPS.csv", row.names = FALSE)
write.csv(RPGdf8, "F:/Development/Projects/Research/TeamBeef/workingProject/output/pre/APre8GPS.csv", row.names = FALSE)
write.csv(RPGdf9, "F:/Development/Projects/Research/TeamBeef/workingProject/output/pre/APre9GPS.csv", row.names = FALSE)
write.csv(RPGdf10, "F:/Development/Projects/Research/TeamBeef/workingProject/output/pre/APre10GPS.csv", row.names = FALSE)
write.csv(RPGdf11, "F:/Development/Projects/Research/TeamBeef/workingProject/output/pre/APre11GPS.csv", row.names = FALSE)
write.csv(RPGdf12, "F:/Development/Projects/Research/TeamBeef/workingProject/output/pre/APre12GPS.csv", row.names = FALSE)
# write.csv(RPGdf13, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost13GPS.csv", row.names = FALSE)
# write.csv(RPGdf14, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost14GPS.csv", row.names = FALSE)
#Loading in Post ----

df1 <- read.csv(file = "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost1GPS.csv", header = TRUE)
#df2 <- read.csv(file = "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost2GPS.csv", header = TRUE)
df3 <- read.csv(file = "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost3GPS.csv", header = TRUE)
df4 <- read.csv(file = "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost4GPS.csv", header = TRUE)
df5 <- read.csv(file = "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost5GPS.csv", header = TRUE)
df6 <- read.csv(file = "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost6GPS.csv", header = TRUE)
#df7 <- read.csv(file = "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost7GPS.csv", header = TRUE)
df8 <- read.csv(file = "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost8GPS.csv", header = TRUE)
df9 <- read.csv(file = "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost9GPS.csv", header = TRUE)
df10 <- read.csv(file = "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost10GPS.csv", header = TRUE)
df11 <- read.csv(file = "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost11GPS.csv", header = TRUE)
df12 <- read.csv(file = "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost12GPS.csv", header = TRUE)
df13 <- read.csv(file = "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost13GPS.csv", header = TRUE)
df14 <- read.csv(file = "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost14GPS.csv", header = TRUE)

#Post Avg ---- 
preAvg1 <- removePercent(df1)
preAvg1 <- collarAvg(preAvg1, 30378)
#preAvg2 <- removePercent(df2)
#preAvg2 <- collarAvg(preAvg2, 30380)
preAvg3 <- removePercent(df3)
preAvg3 <- collarAvg(preAvg3, 30380)
preAvg4 <- removePercent(df4)
preAvg4 <- collarAvg(preAvg4, 30381)
preAvg5 <- removePercent(df5)
preAvg5 <- collarAvg(preAvg5, 30382)
preAvg6 <- removePercent(df6)
preAvg6 <- collarAvg(preAvg6, 30383)
#preAvg7 <- removePercent(df7)
#preAvg7 <- collarAvg(preAvg7, 30384)
preAvg8 <- removePercent(df8)
preAvg8 <- collarAvg(preAvg8, 30385)
preAvg9 <- removePercent(df9)
preAvg9 <- collarAvg(preAvg9, 30386)
preAvg10 <- removePercent(df10)
preAvg10 <- collarAvg(preAvg10, 30387)
preAvg11 <- removePercent(df11)
preAvg11 <- collarAvg(preAvg11, 30388)
preAvg12 <- removePercent(df12)
preAvg12 <- collarAvg(preAvg12, 30389)
preAvg13 <- removePercent(df13)
preAvg13 <- collarAvg(preAvg13, 30390)
preAvg14 <- removePercent(df14)
preAvg14 <- collarAvg(preAvg14, 30391)

avgPostGPS <- rbind(preAvg1,preAvg3,preAvg4,preAvg5,preAvg6,preAvg8,preAvg9,preAvg10,preAvg11,preAvg12,preAvg13,preAvg14)
avgPostGPSn6 <- rbind(preAvg1,preAvg3,preAvg4,preAvg5,preAvg8,preAvg9,preAvg10,preAvg11,preAvg12,preAvg13,preAvg14)

#Avg of GPS Pre----
preAvg1 <- removePercent(RPGdf1)
preAvg1 <- collarAvg(preAvg1, 30379)
preAvg2 <- removePercent(RPGdf2)
preAvg2 <- collarAvg(preAvg2, 30380)
preAvg3 <- removePercent(RPGdf3)
preAvg3 <- collarAvg(preAvg3, 30381)
preAvg4 <- removePercent(RPGdf4)
preAvg4 <- collarAvg(preAvg4, 30382)
preAvg5 <- removePercent(RPGdf5)
preAvg5 <- collarAvg(preAvg5, 30384)
preAvg6 <- removePercent(RPGdf6)
preAvg6 <- collarAvg(preAvg6, 30385)
preAvg7 <- removePercent(RPGdf7)
preAvg7 <- collarAvg(preAvg7, 30386)
preAvg8 <- removePercent(RPGdf8)
preAvg8 <- collarAvg(preAvg8, 30387)
preAvg9 <- removePercent(RPGdf9)
preAvg9 <- collarAvg(preAvg9, 30388)
preAvg10 <- removePercent(RPGdf10)
preAvg10 <- collarAvg(preAvg10, 30389)
preAvg11 <- removePercent(RPGdf11)
preAvg11 <- collarAvg(preAvg11, 30390)
preAvg12 <- removePercent(RPGdf12)
preAvg12 <- collarAvg(preAvg12, 30391)

avgPreGPS <- rbind(preAvg1,preAvg2,preAvg3,preAvg4,preAvg5,preAvg6,preAvg7,preAvg8,preAvg9,preAvg10,preAvg11,preAvg12)
avgPreGPSn6 <- rbind(preAvg1,preAvg2,preAvg3,preAvg4,preAvg5,preAvg7,preAvg8,preAvg9,preAvg10,preAvg11,preAvg12)


#Avg of GPS Print----


write.csv(avgPreGPS, "F:/Development/Projects/Research/TeamBeef/workingProject/output/avgPreGPS.csv", row.names = FALSE)
write.csv(avgPreGPSn6, "F:/Development/Projects/Research/TeamBeef/workingProject/output/avgPreGPSn6.csv", row.names = FALSE)
write.csv(avgPostGPS, "F:/Development/Projects/Research/TeamBeef/workingProject/output/avgPostGPS.csv", row.names = FALSE)
write.csv(avgPostGPSn6, "F:/Development/Projects/Research/TeamBeef/workingProject/output/avgPostGPSn6.csv", row.names = FALSE)

#Histogram Time ----

#Working Tests Distance----

# testData <- read.csv(file = 'Data/GPS/testGPS.csv', header = TRUE)
# yot <- mstConversion(yot)
# 
# testGPS1 <- testData[90,]
# testGPS2 <- testData[91,]
# 
# avgSpeed <- speedFromGPS(PGdf1[(142),], PGdf1[10000,])
# 
# 
# new <- speedFromGPS(testGPS1, testGPS2)
# 
# #Working Test Proximity Selc
# #need to extract before converting to MST 
# yot <- filter(tempData, Fix %in% c("GPS Schedule"))
# 
# #Testing Functions ----
#tempData <- PGdf1[PGdf1$Fix == "GPS Schedule", ]
# #  "Proximity GPS Schedule")
# for(i in 2:nrow(tempData)) {
#   speed <- speedFromGPS(tempData[i-1,], tempData[i,])
# 
#   print(speed)
# }
# #   
# # tempData <- mstConversion(tempData)
#  test <- analyzeFixes(tempData)
# # tempData <- tempData
# # 
# # 
# # #testing why inf speed, if can't figure out will just set to 0 
# # speedFromGPS(tempData[169,], tempData[170,])
# # 
# # yot <- TRUE

#Functions ----

mstConversion <- function(data)
{
  #remove if wanted to analyze Schedulde and Proximity together, will remove when fixed up. 
  data <- data[data$Fix == "GPS Schedule", ]
  
  tempDate <- strptime(data$GMT, "%Y-%m-%d %H:%M", tz = "GMT")
  tempData  <- with_tz(tempDate, "America/Edmonton")
  data$GMT <- tempData 
  
  return(data)  
}

indexGPSZero <- function(dataInput)
{
  #selects the data within the headers specified in the argument
  tempData <- dataInput %>% select(Latitude, Longitude, Altitude)
  #checks the headers specified if a 0 is within 
  tempData <- tempData[tempData$Latitude == 0 | tempData$Longitude == 0 | tempData$Altitude == 0, ]
  #previous loads as char, type cast to numeric for later use
  dataOutput <- as.numeric(rownames(tempData))
  
  return(dataOutput)
  
}

indexCopyFrame5 <- function(dataInput, index)
{
  #set the same structure as the input dataframe
  copyFrame <- dataInput[0, ]
  
  #runs the length of the index vector
  for(i in 1:length(index))
  {
    #checks if it is not out of bounds
    if (index[i] >= 3)
      #appends the specifed index of the input data, to bottom of copyFrame
      copyFrame <- rbind(copyFrame, dataInput[index[i] - 2,])
    
    if (index[i] >= 2)
      copyFrame <- rbind(copyFrame, dataInput[index[i] - 1,])
    
    copyFrame <- rbind(copyFrame, dataInput[index[i],])
    
    if (index[i] <= (nrow(dataInput) - 1))
      copyFrame <- rbind(copyFrame, dataInput[index[i] + 1,])
    
    if (index[i] <= (nrow(dataInput) - 2))
      copyFrame <- rbind(copyFrame, dataInput[index[i] + 2,])

    #for sake of formatting, adds a row of NA
    copyFrame[nrow(copyFrame) + 1,] <- NA
  }
  
  return(copyFrame)
}

indexCopyFrame6 <- function(dataInput, index)
{
  #set the same structure as the input dataframe
  copyFrame <- dataInput[0, ]
  
  #runs the length of the index vector
  for(i in 1:length(index))
  {
    #checks if it is not out of bounds
    if (index[i] >= 3)
      #appends the specifed index of the input data, to bottom of copyFrame
      copyFrame <- rbind(copyFrame, dataInput[index[i] - 2,])
    
    if (index[i] >= 2)
      copyFrame <- rbind(copyFrame, dataInput[index[i] - 1,])
    
    copyFrame <- rbind(copyFrame, dataInput[index[i],])
    
    if (index[i] <= (nrow(dataInput) - 1))
      copyFrame <- rbind(copyFrame, dataInput[index[i] + 1,])
    
    if (index[i] <= (nrow(dataInput) - 2))
      copyFrame <- rbind(copyFrame, dataInput[index[i] + 2,])
    
    if (index[i] <= (nrow(dataInput) - 3))
      copyFrame <- rbind(copyFrame, dataInput[index[i] + 3,])
    
    #for sake of formatting, adds a row of NA
    copyFrame[nrow(copyFrame) + 1,] <- NA
  }
  
  return(copyFrame)
}


analyzeFixes <- function(data)
{
  #2020-07-13 for pre/post switch over 2020-06-18, 
  dates <- seq(as.Date("2020-06-18"), as.Date("2020-07-13"), by="days")
  
  copyFrame <- data.frame(Dates=character(0), Expected_Fixes=numeric(0), On_Time_Fix=numeric(0), 
                          No_Fix=numeric(0), Early_Fix=numeric(0), Late_Fix=numeric(0), Avg_Speed_Day=numeric(0),
                          Avg_Speed_Night=numeric(0), Out_of_Bounds=numeric(0), Per_No_Fix=character(0), Per_Missing_Fix=character(0))
  
  
  
  for(i in 1:length(dates))
  {
    dateCounter <- 0
    
    noFix <- 0 
    lateFix <- 0 
    earlyFix <- 0 
    Fix <- 0 
    
    differenceTime <- 0 
    firstCheck <- TRUE 
    dateStart <- 0 
    dateEnd <- 0 
    
    perNo <- 0
    perMiss <- 0 
    expectedFix <- 0 
    
    avgSpeedDay <- 0
    avgSpeedNight <- 0
    speedcounterD <- 0
    speedcounterN <- 0
    speedEvent <- TRUE
    outOfBounds <- 0
    
    day <- FALSE
    night <- FALSE
    
    for(j in 2:(nrow(data)-1))
    {
      if(as.Date(data$GMT[j]) == dates[i]) { #same day
        
        differenceTime <- as.numeric((data$GMT[j] - data$GMT[j - 1]), units ="mins")
        speedCheck <- speedFromGPS(data[(j-1),], data[j,])
        
        if(speedCheck > 10)
          speedEvent <- FALSE
        
        daytime <- format(data$GMT[j], format = "%H")

        if(daytime == "06") {
          day <- TRUE
          night <- FALSE
        }
        else if(daytime == "20") {
          night <- TRUE
          day <- FALSE
        }
        
        if(data$Latitude[j] == 0 | data$Longitude[j] == 0 | data$Altitude[j] == 0) {
          noFix <- noFix + 1
        }
        
        else if(3 < differenceTime && differenceTime < 7 & speedEvent) { #fix
          Fix <- Fix + 1
          
          if(day) {
          avgSpeedDay <- avgSpeedDay + speedFromGPS(data[(j-1),], data[j,])
          speedcounterD <- speedcounterD + 1
          }
          else if(night) {
          avgSpeedNight <- avgSpeedNight + speedFromGPS(data[(j-1),], data[j,])
          speedcounterN <- speedcounterN + 1         
          }
          
        }
        
        else if(3 >= differenceTime & speedEvent) { #early
          earlyFix <- earlyFix + 1
          
          if(day) {
            avgSpeedDay <- avgSpeedDay + speedFromGPS(data[(j-1),], data[j,])
            speedcounterD <- speedcounterD + 1
          }
          else if(night) {
            avgSpeedNight <- avgSpeedNight + speedFromGPS(data[(j-1),], data[j,])
            speedcounterN <- speedcounterN + 1         
          }
          
        }
        
        else if(differenceTime <= 7 & speedEvent) { #late
          lateFix <- lateFix + 1
          
          if(day) {
            avgSpeedDay <- avgSpeedDay + speedFromGPS(data[(j-1),], data[j,])
            speedcounterD <- speedcounterD + 1
          }
          else if(night) {
            avgSpeedNight <- avgSpeedNight + speedFromGPS(data[(j-1),], data[j,])
            speedcounterN <- speedcounterN + 1         
          }
          
        }
        
        else if(!speedEvent) {
          outOfBounds <- outOfBounds + 1
          speedEvent <- TRUE
        }
        
        
        if(firstCheck) {  #first
          dateStart <- data$GMT[j]
          firstCheck <- FALSE
        }
        
        else if(TRUE) { #last
          dateEnd <- data$GMT[j]
        }
        
        dateCounter <- dateCounter + 1
      }
      
    }
    
    rowDate <- as.character(dates[i])
    
    
    if(dateCounter == 0) {
      holdFrame <- data.frame(rowDate, expectedFix, Fix, noFix, earlyFix, lateFix, avgSpeedDay, avgSpeedNight, outOfBounds, perNo, perMiss)
      copyFrame <- rbind(copyFrame, holdFrame)
    }
    
    else {
      periodDate <- as.interval(dateStart, dateEnd)
      
      if (i == 1) {
        expectedFix <- ceiling( periodDate / minutes(5) )
      }
      else if (i == length(dates)) {
        expectedFix <- ceiling( periodDate / minutes(5) )
      }
      else {
        expectedFix <- 288
        
      }
      
      perNo <- (noFix / (lateFix + earlyFix + Fix)) * 100 
      perNo <- paste(perNo, "%")
      
      perMiss <- ((expectedFix - (lateFix + earlyFix + Fix)) / expectedFix) * 100 
      perMiss <- paste(perMiss, "%")

      avgSpeedDay <- avgSpeedDay / speedcounterD
      avgSpeedNight <- avgSpeedNight / speedcounterN

      holdFrame <- data.frame(rowDate, expectedFix, Fix, noFix, earlyFix, lateFix, avgSpeedDay, avgSpeedNight, outOfBounds, perNo, perMiss)
      copyFrame <- rbind(copyFrame, holdFrame)
    }
    
    
  }
  
  colnames(copyFrame) <- c("Date", "Expected_Fixes", "On_Time_Fix", "No_fix", "Early_Fix", "Late_Fix", "Avg_Speed_Day", "Avg_Speed_Night", "Out_of_Bounds", "Percent_No_Fix_to_Total", "Per_Missing__to_Expected_Fix")
  return(copyFrame)
}

speedFromGPS <- function(p1, p2)
{
  p1GPS <- p1 %>% select(Latitude, Longitude)
  p2GPS <- p2 %>% select(Latitude, Longitude)
  
  p1GPS <- p1GPS[c('Longitude', "Latitude")]
  p2GPS <- p2GPS[c('Longitude', "Latitude")]
  
  distance <- distGeo(p1GPS, p2GPS)
  
  time <- as.numeric((p2$GMT - p1$GMT), units ="secs")

  speed <- abs(distance / time)

  if(speed == Inf)
    speed <- 0 
  
  return(speed)
}

removePercent <- function(data)
{
 data$Percent_No_Fix_to_Total <- as.numeric(gsub("[\\%,]", "", data$Percent_No_Fix_to_Total))
 data$Percent_No_Fix_to_Total <- data$Percent_No_Fix_to_Total / 100 
 
 data$Per_Missing__to_Expected_Fix <- as.numeric(gsub("[\\%,]", "", data$Per_Missing__to_Expected_Fix))
 data$Per_Missing__to_Expected_Fix <- data$Per_Missing__to_Expected_Fix / 100 
 
 data <- transform(data, Per_Good_Fixes = data$On_Time_Fix / (data$No_fix + data$Early_Fix + data$On_Time_Fix))
 data$Out_of_Bounds <-  data$Out_of_Bounds / (data$No_fix + data$Early_Fix + data$On_Time_Fix)
 
 data[] <- lapply(data, function(x) replace(x, !is.finite(x), 1))
 
 return(data)
 
}

collarAvg <- function(data, ID)
{
  copyFrame <- data.frame(ID=numeric(0),Per_Good_Fixes=numeric(0), Percent_No_Fix_to_Total=numeric(0), Per_Missing__to_Expected_Fix=numeric(0), Per_Out_of_Bounds=numeric(0))
  goodFixes <- mean(data$Per_Good_Fixes) 
  
  noFix <- data[data$Percent_No_Fix_to_Total < 5,]
  noFix <- mean(noFix$Percent_No_Fix_to_Total)
  
  missingFix <- data[data$Per_Missing__to_Expected_Fix < 5,]
  missingFix <- mean(missingFix$Per_Missing__to_Expected_Fix) 
  
  outFix <- mean(data$Out_of_Bounds)
  
  holdFrame <- data.frame(ID, goodFixes, noFix, missingFix, outFix)
  copyFrame <- rbind(copyFrame, holdFrame)
  
  colnames(copyFrame) <- c("ID", "Per_Good_Fix_to_Exp", "Per_No_Fix_to_Total","Per_Missing_Fix_to_Total","Per_Out_of_Bounds_Fix")
  
  return(copyFrame)
}


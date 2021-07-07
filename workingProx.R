#Library Loading ----

library(tidyverse)
#library(anytime)
#library(plyr)
#library(dplyr)
#library(readr)
library(lubridate)
#library(geosphere)

#to remove all variables when needed
#rm(list=ls())

#Data Loading ---- 

testData1 <- read.csv(file = "raw/testData/PostProx1.csv", header = TRUE, stringsAsFactors=FALSE)
testData1 <- transform(testData1, ID = 30378)
 

testData2 <- read.csv(file = "raw/testData/PostProx2.csv", header = TRUE, stringsAsFactors=FALSE)
testData2 <- transform(testData2, ID = 30379)

combTest <- testData1[1,]
combTest$Stop <- testData1$Stop[2] 
combTest$Session <- combTest$Session +testData1$Session[2]
#Working Tests ----
testNewNew <- proxPrep("1.", 30380)

newTest <- rbind(testData1, testData2)

df <- transform(testData1, Test1 = as.numeric(testData1$Start))

newTest <- testData2

combTest <- newTest[1,]
copyFrame <- newTest[0,]

for(i in 20:40)#(nrow(newTest) - 1))
{
if(newTest$Proximity[i] == newTest$Proximity[i + 1] & newTest$Stop[i] == newTest$Start[i + 1])
 {
    copyFrame <- newTest[i, ]
    
    while(newTest$Stop[i] == newTest$Start[i + 1])
    {
      if(newTest$Proximity[i] == newTest$Proximity[i + 1])
      {
        copyFrame$Stop <- newTest$Stop[i + 1]
        copyFrame$Session <- copyFrame$Session + newTest$Session[i + 1]
        copyFrame$RSSI <- (copyFrame$RSSI + newTest$RSSI[i + 1]) / 2
        i <- i + 1
      }
      else
      {
        i <- i + 1
      }
      print("while")
      print(i)
    }
 }
 else
 {
   copyFrame <- newTest[i,]
 }
  print("comb")
  print(i)
  combTest <- rbind(combTest, copyFrame)
}

combTest <- subset(combTest, Proximity == 30387)
testData2 <- subset(testData2, Proximity == 30387)


#Testing Comb ----
combTest <- newTest[1,]
copyFrame <- newTest[1,]


if(newTest$Proximity[8] == newTest$Proximity[8 + 1] & newTest$Stop[8] == newTest$Start[8 + 1])
{
  copyFrame <- newTest[8, ]
  copyFrame$Stop <- newTest$Stop[8 + 1]
  copyFrame$Session <- copyFrame$Session + newTest$Session[8 + 1]
  copyFrame$RSSI <- (copyFrame$RSSI + newTest$RSSI[8 + 1]) / 2
  i <- 8 + 1
}

#apply this since it will give seconds since the epoch, add +/- some to be determined value and check to see if the start time falls within the range of it. 
#as.numeric((df$GMT[2] - df$GMT[2118]), units ="secs")

testingLoop <- transform(testingLoop, StartNum = as.numeric(testingLoop$Start), StopNum = as.numeric(testingLoop$Stop))
df1 <- tail(df, -1)
 testData1 <- mstConversion(testData1)
 
 
 #Function Testing Proxiimity Comparision Test ----
 
testingLoop <- rbind(testData1, testData2)

firstItr <- testingLoop[5,]
firstItr
firstFilter <- testingLoop[firstItr$Proximity == testingLoop$ID,]

firstFilterDate <- firstFilter[firstFilter$StartNum <= firstItr$StopNum, ]

if(nrow(firstFilterDate) == 0)
  disagreement++
else
  for(j in 1:nrow())
  firstFilter[nrow(firstFilter),]

#Test View ----
test1 <- testingLoop[(testingLoop$Proximity == 30379) & (testingLoop$ID == 30378),]
test2 <- testingLoop[(testingLoop$Proximity == 30378) & (testingLoop$ID == 30379),]

test2 <- rbind(test2,test2)

testSelect <-  hold %>% select(StartNum, StopNum)

ID_Vec <- c(30378,30379,30380,30381,30382,30383,30384,30385,30386,30387,30388,30389,30390,30391)

copyFrame <- data.frame(Start_Difference=numeric(0), Stop_Difference=numeric(0), ID=numeric(0), Proximity=numeric(0))

for(i in 1:14)
{
  checkingID <- data[data$ID == ID_Vec[i],]
  
  for(j in 1:14)
  {
    verifyingID <- data[data$ID == ID_Vec[j],]
    
    if(verifyingID$ID == checkingID$ID) {
      copyFrame[nrow(copyFrame) + 1,] <- NA
    }
    else if(nrow(verifyingID) == nrow(checkingID)) {
      holdFrame <- verifyingID %>% select(StartNum, StopNum)
      checkFrame <- checkingID %>% select(StartNum, StopNum)
      holdFrame <- holdFrame - checkFrame
      holdFrame <- transform(holdFrame, ID = ID_Vec[i], Proximity = ID_Vec[j])
      
      copyFrame <- rbind(copyFrame, holdFrame)
    }
    else {
      copyFrame[nrow(copyFrame) + 1,] <- NA
    }
    
  }
  
}



#Clean Test ----
df1 <- proxPrep("1.", 30378)
df2 <- proxPrep("2.", 30379)
df3 <- proxPrep("3.", 30380)
df4 <- proxPrep("4.", 30381)
df5 <- proxPrep("5.", 30382)
df6 <- proxPrep("6.", 30383)
df7 <- proxPrep("7.", 30384)
df8 <- proxPrep("8.", 30385)
df9 <- proxPrep("9.", 30386)
df10 <- proxPrep("10", 30387)
df11 <- proxPrep("11", 30388)
df12 <- proxPrep("12", 30389)
df13 <- proxPrep("13", 30390)
df14 <- proxPrep("14", 30391)


df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14)
testResult <- proxCheck(df)

write.csv(testResult, "F:/Development/Projects/Research/TeamBeef/workingProject/output/pre/PreProxResult.csv", row.names = FALSE)

#Functions ----
mstConversion <- function(data)
{
  tempData1 <- strptime(data$Start, "%Y-%m-%d %H:%M", tz = "GMT")
  tempData1  <- with_tz(tempData1, "America/Edmonton")
  data$Start <- tempData1 
  
  tempData2 <- strptime(data$Start, "%Y-%m-%d %H:%M", tz = "GMT")
  tempData2 <- with_tz(tempData2, "America/Edmonton")
  data$Stop <- tempData2 
  
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

indexCopyFrame <- function(dataInput, index)
{
  #set the same structure as the input dataframe
  copyFrame <- dataInput[0, ]
  
  #runs the length of the index vector
  for(i in 1:length(index))
  {
    copyFrame <- rbind(copyFrame, dataInput[index[i],])
  }
  
  return(copyFrame)
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
  #2020-07-13 for pre/post switch over
  dates <- seq(as.Date("2020-06-18"), as.Date("2020-09-16"), by="days")
  
  copyFrame <- data.frame(Dates=character(0), Expected_Fixes=numeric(0), On_Time_Fix=numeric(0), 
                          No_Fix=numeric(0), Early_Fix=numeric(0), Late_Fix=numeric(0), 
                          Per_No_Fix=character(0), Per_Missing_Fix=character(0))
  
  
  
  for(i in 1:length(dates))
  {
    dateCounter <- 0
    noFix <- 0 
    lateFix <- 0 
    earlyFix <- 0 
    Fix <- 0 
    expFix <- 0 
    differenceTime <- 0 
    firstCheck <- TRUE 
    dateStart <- 0 
    dateEnd <- 0 
    perNo <- 0
    perMiss <- 0 
    expectedFix <- 0 
    
    for(j in 2:(nrow(data)-1))
    {
      if(as.Date(data$MST[j]) == dates[i]) { #same day
        
        differenceTime <- as.numeric((data$MST[j] - data$MST[j - 1]), units ="mins")
        
        if(data$Latitude[j] == 0 | data$Longitude[j] == 0 | data$Altitude[j] == 0) {
          noFix <- noFix + 1
        }
        
        else if(3 < differenceTime && differenceTime < 7) { #fix
          Fix <- Fix + 1
        }
        
        else if(3 >= differenceTime) { #early
          earlyFix <- earlyFix + 1
        }
        
        else if(differenceTime <= 7) { #late
          lateFix <- lateFix + 1
        }
        
        if(firstCheck) {  #first
          dateStart <- data$MST[j]
          firstCheck <- FALSE
        }
        
        else if(TRUE) { #last
          dateEnd <- data$MST[j]
        }
        
        dateCounter <- dateCounter + 1
      }
      
    }
    
    rowDate <- as.character(dates[i])
    
    
    if(dateCounter == 0) {
      holdFrame <- data.frame(rowDate, expectedFix, Fix, noFix, earlyFix, lateFix, perNo, perMiss)
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
      
      holdFrame <- data.frame(rowDate, expectedFix, Fix, noFix, earlyFix, lateFix, perNo, perMiss)
      copyFrame <- rbind(copyFrame, holdFrame)
    }
    
    
  }
  
  colnames(copyFrame) <- c("Date", "Expected_Fixes", "On_Time_Fix", "No_fix", "Early_Fix", "Late_Fix", "Percent_No_Fix_to_Total", "Per_Missing__to_Expected_Fix")
  return(copyFrame)
}

speedFromGPS <- function(p1, p2)
{
  p1GPS <- p1 %>% select(Latitude, Longitude)
  p2GPS <- p2 %>% select(Latitude, Longitude)
  
  p1GPS <- p1GPS[c('Longitude', "Latitude")]
  p2GPS <- p2GPS[c('Longitude', "Latitude")]
  
  distance <- distGeo(p1GPS, p2GPS)
  
  time <- as.numeric((p1$GMT - p2$GMT), units ="secs")
  
  speed <- distance / differenceTime
  
  return(speed)
}

acclPrep <- function(index)
{
  testList <- list.files("raw/accl/post")
  counter <- 0 
  
  for(i in 1:length(testList))
  {
    if(substr(testList[i], 9, 10) == index) {
      
      filePath <- paste('raw/accl/post/', testList[i], sep="") 
      
      holdFrame <- read.csv(file = filePath, header = TRUE)
      
      if(counter == 0) {
        copyFrame <- holdFrame
        
      }
      else {
        copyFrame <- rbind(copyFrame, holdFrame)
      }
      
      counter <- counter + 1
      
    }
  }
  
  data <- mstConversion(copyFrame)
  
  data$X <- data$X * 0.31392 
  data$Y <- data$Y * 0.31392 
  data$Z <- data$Z * 0.31392 
  
  return(data)
}

proxPrep <- function(index, inputID)
{
  testList <- list.files("raw/prox/post")
  counter <- 0 
  
  for(i in 1:length(testList))
  {
    if(substr(testList[i], 9, 10) == index) {
      
      filePath <- paste('raw/prox/post/', testList[i], sep="") 

      holdFrame <- read.csv(file = filePath, header = TRUE)

      if(counter == 0) {
        copyFrame <- holdFrame

      }
      else {
        copyFrame <- rbind(copyFrame, holdFrame)
      }

      counter <- counter + 1

      }
  }
  
  data <- mstConversion(copyFrame)
  
  data <- transform(data, ID = inputID)
  data <- transform(data, StartNum = as.numeric(data$Start), StopNum = as.numeric(data$Stop))
  
  return(data)
}

proxCheck <- function(data)
{
  ID_Vec <- c(30378,30379,30380,30381,30382,30383,30384,30385,30386,30387,30388,30389,30390,30391)
  copyFrame <- data.frame(StartNum=numeric(0), StopNum=numeric(0), ID=numeric(0), Proximity=numeric(0))
  
  for(i in 1:14)
  {
    checkingID <- data[data$ID == ID_Vec[i],]

    for(j in i:14)
    {
      checkFrame <- data[(data$ID == ID_Vec[i]) & (data$Proximity == ID_Vec[j]), ]
      verifyingID <- data[(data$ID == ID_Vec[j]) & (data$Proximity == ID_Vec[i]), ]

      if(is.na(verifyingID$ID[1] == checkingID$ID[1])) {
      #  holdFrame <- c(NA, NA, ID_Vec[i], ID_Vec[j])
      # copyFrame <- rbind(copyFrame, holdFrame)
        copyFrame[nrow(copyFrame) + 1,] <- NA
      }
      else if(verifyingID$ID[1] == checkingID$ID[1]) {
      #  holdFrame <- c(NA, NA, ID_Vec[i], ID_Vec[j])
      #  copyFrame <- rbind(copyFrame, holdFrame)
        copyFrame[nrow(copyFrame) + 1,] <- NA
      }
      else if(nrow(verifyingID) == nrow(checkFrame)) {
        holdFrame <- verifyingID %>% select(StartNum, StopNum)
        checkFrameSub <- checkFrame %>% select(StartNum, StopNum)
        holdFrame <- holdFrame - checkFrameSub
        holdFrame <- transform(holdFrame, ID = ID_Vec[i], Proximity = ID_Vec[j])
        print(holdFrame)
        print(copyFrame)
        copyFrame <- rbind(copyFrame, holdFrame)
      }
      else {
        copyFrame[nrow(copyFrame) + 1,] <- NA
      }
      
    }
    
  }
  return(copyFrame)
}

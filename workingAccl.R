#Library Loading ----

library(tidyverse)
#library(anytime)
#library(plyr)
#library(dplyr)
#library(readr)
library(lubridate)
#library(geosphere)
library(outliers)
library(signal)
library(climtrends)
install.packages('climtrends')
install.packages('signal')
#to remove all variables when needed
#rm(list=ls())

#Loading Data ----
#Developed function to load in data 
df1 <- acclPrep("1 ")
df2 <- acclPrep("2 ")
#df3 <- acclPrep("3 ")
df4 <- acclPrep("4 ")
df5 <- acclPrep("5 ")
df6 <- acclPrep("6 ")
df7 <- acclPrep("7 ")
df8 <- acclPrep("8 ")
df9 <- acclPrep("9 ")
df10 <- acclPrep("10")
df11 <- acclPrep("11")
df12 <- acclPrep("12")
df13 <- acclPrep("13")
df14 <- acclPrep("14")

#Loading 3, 6, 10
df6 <- read.csv(file = 'raw/accl/post/PostAccl6.csv', header = TRUE)
df6 <- mstConversion(df6)

df6$X <- df6$X * 0.31392 
df6$Y <- df6$Y * 0.31392 
df6$Z <- df6$Z * 0.31392 

df6 <- transform(df6, SUM = sqrt(X*X + Y*Y + Z*Z))
testData <- df12
testData <- testData[date(testData$GMT) == "2020-07-13", ]
#omit since only has X, Y data
#df3 <- acclPrep()

#Clean Run ----

Rdf1 <- analyzeAccl(df1)
Rdf2 <- analyzeAccl(df2)
Rdf4 <- analyzeAccl(df4)
Rdf5 <- analyzeAccl(df5)
Rdf6 <- analyzeAccl(df6)
Rdf7 <- analyzeAccl(df7)
Rdf8 <- analyzeAccl(df8)
Rdf9 <- analyzeAccl(df9)
Rdf10 <- analyzeAccl(df10)
Rdf11 <- analyzeAccl(df11)
Rdf12 <- analyzeAccl(df12)
Rdf13 <- analyzeAccl(df13)
Rdf14 <- analyzeAccl(df14)

#Clean Printing ----
write.csv(Rdf1, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost1Accl.csv", row.names = FALSE)
write.csv(Rdf2, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost2Accl.csv", row.names = FALSE)
write.csv(Rdf3, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost3Accl.csv", row.names = FALSE)
write.csv(Rdf4, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost4Accl.csv", row.names = FALSE)
write.csv(Rdf5, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost5Accl.csv", row.names = FALSE)
write.csv(Rdf6, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost6Accl.csv", row.names = FALSE)
write.csv(Rdf7, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost7Accl.csv", row.names = FALSE)
write.csv(Rdf8, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost8Accl.csv", row.names = FALSE)
write.csv(Rdf9, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost9Accl.csv", row.names = FALSE)
write.csv(Rdf10, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost10Accl.csv", row.names = FALSE)
write.csv(Rdf11, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost11Accl.csv", row.names = FALSE)
write.csv(Rdf12, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost12Accl.csv", row.names = FALSE)
write.csv(Rdf13, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost13Accl.csv", row.names = FALSE)
write.csv(Rdf14, "F:/Development/Projects/Research/TeamBeef/workingProject/output/post/APost14Accl.csv", row.names = FALSE)

#Working Test ----

#subset works super nicely 
#what think will do, is run the same for loop over the date intervals to develop the data 
#will then look to  have the subsetted data for the features that we are curious in searching for
test <- acclPrep("1 ")
newTest <- subset(test,  ye$statistic[1] < test$SUM | ye$statistic[2] > test$SUM) 

test2 <- df[df$SUM > upper_bound,]
test3 <- df[ ye$statistic[1] < test$SUM]

#this method will work well, can take this general idea to compare 
df <- transform(df, Test1 = as.numeric(df$GMT))
test4 <- df[df$Test1 > 1595500980, ]
#how to handle GPS prox comparison is still being thought of

trt <- butter(3, test$X)

newTest <- df %>% select(SUM)
#this provides teh magnitude of forces
df <- transform(test, SUM = sqrt(X*X + Y*Y + Z*Z))

df <- transform(test, test = newTest)

test <- df(df$GMT) == as.Date("2020-07-12")

max(df$sum)

#Testing Viz for Outlier Detection
ggplot(test) +
  aes(x = "", y = SUM) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

qqnorm(df$SUM, pch = 1, frame = FALSE)

#Stats Test Test ----
lower_bound <- quantile(test$SUM, 0.0005)
upper_bound <- quantile(test$SUM, 0.9995)

lower_bound <- quantile(test$SUM, 0.0005)
upper_bound <- quantile(test$SUM, 0.9995)

lower_bound <- quantile(test$SUM, 0.0005)
upper_bound <- quantile(test$SUM, 0.9995)

lower_bound <- median(test$SUM) - 3 * mad(test$SUM, constant = 1)


ye <- grubbs.test(test$SUM)


test <- rosnerTest(df$SUM, k = floor(sqrt(length(outliers))), alpha = 0.01)
test

outliers <- boxplot(test$SUM)$out

lofactor(df$SUM, 10)

#Jackson Thinking Tests
testLength <- boxplot.stats(df$SUM)$out
length(testLength)

testRos <- rosnerTest(df$SUM, k = floor(sqrt(length(testLength))))
testView <- testRos$all.stats$Value

#Functions ----
mstConversion <- function(data)
{
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
      
      #cannot read a single file in its current itreation, will need to fix this
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
  
  data <- transform(data, SUM = sqrt(X*X + Y*Y + Z*Z))
 #data <- transform(data, SUM_RES = SUM - mean(SUM))
  
  return(data)
}

analyzeAccl <- function(data)
{
  #2020-07-13 for pre/post switch over
  dates <- seq(as.Date("2020-06-18"), as.Date("2020-09-16"), by="days")
  
  copyFrame <- data.frame(Dates=character(0), Per_Exp=numeric(0), Per_95_Ac=numeric(0), Per_99_Ac=numeric(0), Per_99_H=numeric(0))
  
  lower_bound95 <- quantile(data$SUM, 0.025)
  upper_bound95 <- quantile(data$SUM, 0.975)
  
  lower_bound99 <- quantile(data$SUM, 0.005)
  upper_bound99 <- quantile(data$SUM, 0.995)
  
  upper_bound99H <- quantile(data$SUM, 0.9995)
  
  for(i in 1:length(dates))
  {
    perExp <- 0 
    per95 <- 0 
    per99 <- 0
    per99H <- 0 
    rowDate <- as.character(dates[i])
    #collar <- "Unknown"
    
    dataDay <- data[date(data$GMT) == dates[i], ]
    
    
    if(nrow(dataDay) == 0) {
      holdFrame <- data.frame(rowDate, perExp, per95, per99, per99H)
      copyFrame <- rbind(copyFrame, holdFrame)
    }
    
    else {
      perExpTotal <- nrow(dataDay)
      perExp <- perExpTotal / (86400/2)
      
      per95 <- nrow(dataDay[(dataDay$SUM <= lower_bound95) | (dataDay$SUM >= upper_bound95),]) / perExpTotal
      per99 <- nrow(dataDay[(dataDay$SUM <= lower_bound99) | (dataDay$SUM >= upper_bound99),]) / perExpTotal
      per99H <- nrow(dataDay[(dataDay$SUM >= upper_bound99H),]) / perExpTotal
      
      holdFrame <- data.frame(rowDate, perExp, per95, per99, per99H)
      copyFrame <- rbind(copyFrame, holdFrame)
    }
    

  }
    
  #Need to add a first last day check and change the percent expected with that in mind, the calendar of events non hard-coded is needed for sure. 
  
  
  colnames(copyFrame) <- c("Date", "Percent Expected Recordings", "95 Percentile Activities", "99 Percentile Activities", '99.99 Percentile High Activities')
  return(copyFrame)
}


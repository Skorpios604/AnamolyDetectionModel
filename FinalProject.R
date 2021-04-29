library("tidyverse") # tidyverse library where all the magic happens
library("psych")
library("lubridate")
library("zoo")
library("timetk")
library("anomalize")
library("depmixS4")
# Installation Package, Installation Over Please Skip
#install.packages("devtools")
#install_github("vqv/ggbiplot")

#install.packages("devtools", repo="http://cran.us.r-project.org")
#library(devtools)
#install_github("vqv/ggbiplot")

setwd("C:/Users/Skorpios/Desktop/318FinalProject")
data <- read_csv(
  "TermProjectData.txt",
  col_types = cols(
    Date = col_date("%d/%m/%Y"),
    Time = col_time(format = "%H:%M:%S"),
    Global_active_power = col_double(),
    Global_reactive_power = col_double(),
    Voltage = col_double(),
    Global_intensity = col_double(),
    Sub_metering_1 = col_double(),
    Sub_metering_2 = col_double(),
    Sub_metering_3 = col_double(),
    weekday = col_double()
  )
)
data$weekday <- weekdays(data$Date)

#PART 2, creating multivariate HMM (selecting the variables with PCA is at the bottom)

#Make trainDataFridaysOnlySevenToNinePm by taking all data before or on 2007-07-22
trainData <- subset(data, Date >= "2007-07-22")
trainDataFridaysOnly <- trainData %>% na.omit() %>%  filter(weekday== "Friday")
trainDataFridaysOnlySevenToNinePm <- trainDataFridaysOnly %>% na.omit() %>%  filter(Time >= hours(19) & Time <= hours(21))
trainDataFridaysOnlySevenToNinePm$weekday <- NULL

#Scale the training data
trainDataScaled <- trainDataFridaysOnlySevenToNinePm
trainDataScaled$Global_active_power <- scale(trainDataScaled$Global_active_power, scale = TRUE)
trainDataScaled$Global_reactive_power <- scale(trainDataScaled$Global_reactive_power, scale = TRUE)
trainDataScaled$Voltage <- scale(trainDataScaled$Voltage, scale = TRUE)
trainDataScaled$Global_intensity <- scale(trainDataScaled$Global_intensity, scale = TRUE)
trainDataScaled$Sub_metering_1 <- scale(trainDataScaled$Sub_metering_1, scale = TRUE)
trainDataScaled$Sub_metering_2 <- scale(trainDataScaled$Sub_metering_2, scale = TRUE)
trainDataScaled$Sub_metering_3 <- scale(trainDataScaled$Sub_metering_3, scale = TRUE)


#Make testDataFridaysOnlySevenToNinePm by by taking all data after 2007-07-22
testData <- subset(data, Date < "2007-07-22")
testDataFridaysOnly <- testData %>% na.omit() %>%  filter(weekday== "Friday")
testDataFridaysOnlySevenToNinePm <- testDataFridaysOnly %>% na.omit() %>%  filter(Time >= hours(19) & Time <= hours(21))
testDataFridaysOnlySevenToNinePm$weekday <- NULL
#scale the test data
testdataScaled <- testDataFridaysOnlySevenToNinePm
testdataScaled$Global_active_power <- scale(testdataScaled$Global_active_power, scale = TRUE)
testdataScaled$Global_reactive_power <- scale(testdataScaled$Global_reactive_power, scale = TRUE)
testdataScaled$Voltage <- scale(testdataScaled$Voltage , scale = TRUE)
testdataScaled$Global_intensity <- scale(testdataScaled$Global_intensity, scale = TRUE)
testdataScaled$Sub_metering_1 <- scale(testdataScaled$Sub_metering_1, scale = TRUE)
testdataScaled$Sub_metering_2 <- scale(testdataScaled$Sub_metering_2, scale = TRUE)
testdataScaled$Sub_metering_3 <- scale(testdataScaled$Sub_metering_3, scale = TRUE)



#Get the count of each date's number of entries
trainDataCount<-(trainDataScaled %>% group_by(Date) %>% count())
trainN <- trainDataCount %>% pull(n)

#Training our multivariate HMM with 8 states using Voltage and Global_intensity
model4 <- depmix(list(trainDataScaled$Voltage ~1, trainDataScaled$Global_intensity ~1), data=trainDataScaled,nstates=4,transition=~Pacc, family=list(gaussian(),gaussian()), ntimes=trainN)
fitModel <- fit(model4)



#Run model using test data to check for overfitting
testDataCount<-(testdataScaled %>% group_by(Date) %>% count())
testN <- testDataCount %>% pull(n)

testmodel4 <- depmix(list(testdataScaled$Voltage ~1, testdataScaled$Global_intensity ~1), data=testdataScaled,nstates=4,family=list(gaussian(),gaussian()), ntimes=testN)
testForwardBackward <- forwardbackward(testmodel8)
all.equal(-sum(log(testForwardBackward$sca)),testForwardBackward$logLike)


#Training our multivariate HMM with 16 states using Voltage and Global_intensity
model4 <- depmix(list(trainDataScaled$Voltage ~1, trainDataScaled$Global_intensity ~1), data=trainDataScaled,nstates=4,family=list(gaussian(),gaussian()), ntimes=trainN)
fitModel <- fit(model16)

#Testing with 16 models
testmodel4 <- depmix(list(testdataScaled$Voltage ~1, testdataScaled$Global_intensity ~1), data=testdataScaled,nstates=4,family=list(gaussian(),gaussian()), ntimes=testN)
testForwardBackward16 <- forwardbackward(testmodel16)
(testForwardBackward16$logLike)

#Training our multivariate HMM with 14 states using Voltage and Global_intensity
model4 <- depmix(list(trainDataScaled$Voltage ~1, trainDataScaled$Global_intensity ~1), data=trainDataScaled,nstates=4,family=list(gaussian(),gaussian()), ntimes=trainN)
fitModel4 <- fit(model14)

# Sample of wavelet analysis
# Code adapted from Cayelan Carey and Dexter Howard
# fDOM data from Cayelan Carey and Dexter Howard
# Adapted by pch on 23 Jan 2021

# Install packages as necessary, e.g.,
# install.packages("dplR",repos="https://cloud.r-project.org")
# install.packages("wavelets",repos="https://cloud.r-project.org")
# install.packages("Rwave",repos="https://cloud.r-project.org")

###Reading in packages 
library(zoo)
library(dplR) #Use this instead of Rwave for the morlet analysis
# library(wavelets)
# library(Rwave)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)


source("./scripts/WaveHelpers.R")

########################
#### User setup generated signal
GenerateData = FALSE # Whether or not to generate the signal
NativeSamplingPeriod = 1/12 # as fraction of year; 1/12 = monthly
SamplesPerYear = 1/NativeSamplingPeriod
nYears = 30 # duration
SineWavePeriods = c(1,4,10) # Periods of embedded signals
NoiseLevel = 0 # Noise to be added to the signal in sd
Delay = 0.5 # Delay as proportion of nYears (this is NOT years; should be 0-1)
dataCol = 3 # The column that has the data to use wavelets on

#### Data file from Carey Lab
#### If you want to read in data and analyze, this is an example
# InDataFile = "./data/fdomdata4wavelet.csv"
# NativeSamplingPeriod = (10/(1440))  # Normalized to day, in this case 10 min sampling period
# SubSample = 1 # skip nth SubSamples

PlotHeatWave = TRUE # whether or not to plot the heat map of the wavelet transform
# End user setup
########################

if (GenerateData){
  SineWaveFreqs = 1/SineWavePeriods
  myData = GenerateSignal(SamplesPerYear,nYears,SineWaveFreqs,NoiseLevel,Delay)
  SamplingPeriod = NativeSamplingPeriod
}else{
  # Read data from disk
  InData = read.csv(InDataFile,header=TRUE,stringsAsFactors = FALSE)
  nInData = dim(InData)[1]
  
  # Subsample data if user specifies
  if (SubSample>1){
    iNew = seq(1,nInData,SubSample)
    myData = InData[iNew,]
    # Update sampling period
    SamplingPeriod = NativeSamplingPeriod * SubSample # In this example,it's proportion of day
  }else{
    myData = InData
    SamplingPeriod = NativeSamplingPeriod
  }
}

#########################
# Wavelet transform
#run this so the COI countor appears for entire plot
options("max.contour.segments" = 250000) # can make number larger if needed 
#Wavelet analysis is this line!
if (GenerateData){
  output<-morlet(myData[,dataCol], myData$SeqTime, dj=(1/12), siglvl = 0.95) ###p2 is 2^ whatever value thats set. # NULL sets p2 to 15. #Chose 14.5 so that plot has minimal area above COI
}else{
  output<-morlet(myData[,dataCol], myData$SeqTime, dj=(1/12), siglvl = 0.95, p2= 14.5) ###p2 is 2^ whatever value thats set. # NULL sets p2 to 15. #Chose 14.5 so that plot has minimal area above COI
}

# Post processing wavelet transform
#setting period to days for plot
output$period <- round((output$period * SamplingPeriod), digits = 4)  #This works with correcting period to days for y axis  
#View(output$period)

#calculating mean global power per scale period 
ScalePower <- c(1:175) # Not sure why 175 is a magic number
for (j in 1:ncol(output$Power)) {
  ScalePower[j] <- mean(output$Power[,j])
}

# create a data frame with the scale period and the mean global power
powerplot <- as.data.frame(cbind(ScalePower, output$period))
powerplot <- rename(powerplot, c( "mean_power" = "ScalePower"))
powerplot <- rename(powerplot, c( "period" = "V2"))

# Filter global power up to 80 days 
powerplotmonths <- powerplot %>% 
  filter(period < nYears)

# Filter global power up to 5 days 
powerplotdays <- powerplot %>% 
  filter(period < 5)

####################################
# Plotting functions

# Plot original time series
# plot(as.POSIXct(myData$DateTime),myData[,dataCol],type='l',xlab="Date",ylab=colnames(myData)[dataCol])

# Plot global power up to several months scale
plot(powerplotmonths$period[1:100],powerplotmonths$mean_power[1:100],type='l',xlab="Peroid (d)",ylab="Mean Power^2")
# plot(powerplotmonths$period,powerplotmonths$mean_power,type="l",xlab="Peroid (d)",ylab="Mean Power^2")
grid()

# Plot global power up to several days scale
# plot(powerplotdays$period,powerplotdays$mean_power,type="l",xlab="Peroid (d)",ylab="Mean Power^2")
# plot(powerplotmonths$period[1:50],powerplotmonths$mean_power[1:50],type='l',xlab="Peroid (d)",ylab="Mean Power^2")
# grid()

#creating plot; THIS CAN TAKE A WHILE. Don't try and run other functions until plot is completely rendered in plot window (R will get angry and crash)
#make new graphical function to fix period vs. scale issue
cols1<-c('blue3', 'blue', "dodgerblue3", "cyan", "green", "greenyellow", "yellow","orange","red", "red3")
if (PlotHeatWave){
  print('Plotting heat map may take several minutes...')
  wavelet.plot(output)
  #wavelet.plot.new(output) #export in 977 x 701 for all numbers to show on color grid 
}

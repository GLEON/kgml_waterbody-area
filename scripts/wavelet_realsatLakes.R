# Lake Expedition wavelet analysis 
# code adapted from CCC, DWH, and PCH
# 25 Jan 2021 - HLW

#read in libraries
pacman::p_load(zoo,dplR,dplyr,tidyverse,ggplot2,ggpubr,sf)

#-------------------------------------------------------------------------------#
#### User setup generated signal
NativeSamplingPeriod = 1/12 # normalized to year 
nYears = 30 # duration

#read in realsat data
area_timeseries = readRDS("./data/area_timeseries.rds")
USlakes <- read_sf("data/ReaLSAT-R-2.0.shp")

#add reservoir col to timeseries
area_timeseries$Reservoir <- USlakes$RESERVOIR[match(area_timeseries$id, USlakes$ID)]

#select only relevent cols and remove negative rows
InData <- area_timeseries %>% select(date, id, area_rm_missing, Reservoir) %>%
            filter(area_rm_missing >=0) %>% group_by(id) %>%
            mutate(SeqTime = time(date)) #might have to fix this if looking across multiple lakes

#subset area timeseries to one lake
InData <- InData[InData$id=="705918",] #lakes: 647128, 572881, 470900, 718893, 712381, 729711, 735202, 698472
#plot(InData$area_rm_missing~InData$date,type="l")    #Reservoirs: 712490, 712642, 712700, 712459, 705670, 705918

#normalize data
InData$area_normalized <- (InData$area_rm_missing - mean(InData$area_rm_missing)) / sd(InData$area_rm_missing)

nInData = dim(InData)[1]

PlotHeatWave = TRUE # whether or not to plot the heat map of the wavelet transform
# End user setup
#------------------------------------------------------------------------------#
# Wavelet transform

#not subsetting data so...
  myData = InData
  SamplingPeriod = NativeSamplingPeriod

#run this so the COI countor appears for entire plot
options("max.contour.segments" = 250000) # can make number larger if needed 

#Wavelet analysis is this line!
  output<-morlet(myData$area_normalized, myData$SeqTime, dj=1/12, siglvl = 0.95) ###p2 is 2^ whatever value that's set. # NULL sets p2 to 15. #Chose 14.5 so that plot has minimal area above COI
    #NOTE: had to revert to default p2 because would not plot (I think numbers/dataframe too large??)
  
# Post processing wavelet transform
#setting period to days for plot
output$period <- round((output$period * SamplingPeriod), digits = 4)  #period is in years here (I think?)
#View(output$period)

#calculating mean global power per scale period 
ScalePower <- c(1:ncol(output$Power)) # make sure this matches power length
for (j in 1:ncol(output$Power)) {
  ScalePower[j] <- mean(output$Power[,j])
}

# create a data frame with the scale period and the mean global power
powerplot <- as.data.frame(cbind(ScalePower, output$period))
powerplot <- rename(powerplot, c( "mean_power" = "ScalePower"))
powerplot <- rename(powerplot, c( "period" = "V2"))

# Filter global power up to 30 years
powerplotyears <- powerplot %>% 
  filter(period < nYears)

# Filter global power up to 1 year
powerplotmonths <- powerplot %>% 
  filter(period < 1)

#------------------------------------------------------------------------------#
# FIGURES 

# Plot global power up to several years scale
plot(powerplotyears$period,powerplotyears$mean_power,type='l',xlab="Peroid (years)",ylab="Mean Power^2")
grid()

# Plot global power up to several months scale
plot(powerplotmonths$period,powerplotmonths$mean_power,type='l',xlab="Peroid (months)",ylab="Mean Power^2")
grid()

#creating plot; THIS CAN TAKE A WHILE. Don't try and run other functions until plot is completely rendered in plot window (R will get angry and crash)
#make new graphical function to fix period vs. scale issue

#jpeg("./figures/lake_705918_wavelet.jpg", width = 6, height = 4, units = "in",res = 300)
cols1<-c('blue3', 'blue', "dodgerblue3", "cyan", "green", "greenyellow", "yellow","orange","red", "red3")
if (PlotHeatWave){
  print('Plotting heat map may take several minutes...')
  wavelet.plot(output,key.cols = cols1)
}
#dev.off()

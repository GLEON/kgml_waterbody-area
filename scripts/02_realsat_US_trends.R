#Realsat data exploration - trends in US lake/reservoir area
#Created 28 Oct 2020 - HLW

#load packages
pacman::p_load(dplyr,sf,mapview,ggplot2,tidyverse)

#read in US shapefile and monthly timeseries
dt <- read_sf("data/ReaLSAT-R-2.0.shp")
timeseries <-  readRDS("data/area_timeseries.rds")

#-------------------------------------------------#
######## Categorization and Calculations ########
#-------------------------------------------------#

#group lakes by area; small= 80-1500 ha , medium= 1500-10000 ha, large= 10000-90000 ha
#Note: arbitrarily categorizing now, but this code can be easily changed 
USlakes <-dt

USlakes_final <- USlakes %>% select(c(ID,RESERVOIR,AREA)) %>% 
  mutate(LakeSize=ifelse(AREA<=1500,"small", ifelse(AREA>1500 & 
  AREA<=10000,"medium","large")))

#select middle lat/long from list
USlakes_final_points <- st_centroid(USlakes_final)
#split into lat/long
US_lake_area <- USlakes_final_points %>%
  mutate(lat = unlist(map(USlakes_final_points$geometry,1)),
         long = unlist(map(USlakes_final_points$geometry,2)))

#visualize spread of lakes by size
#mapview(USlakes_final,zcol="LakeSize")

#not sure if area is in pixels or km2, but might have to multiply by 0.0009 to get to km2
#also a lot -1 values (which I assume means that there was no data?) - going to filter those out for now
timeseries_cleaned <- timeseries %>% filter(area >=0)

#calculate rate of change for each year/lake (the farther away from 0, the greater the change)
#not sure how good of a metric the mean_rate is because it gets rid of the monthly variability
timeseries_final <- timeseries_cleaned %>% group_by(id) %>%
  mutate(rate = 100 * (area - lag(area))/lag(area)) %>% 
  mutate(mean_rate=mean(is.finite(rate)))

#calculate mean rate of change for each year/lake
annual_means <- timeseries_final %>% group_by(id,year) %>%
  summarise(annual_mean_change=mean(rate))

#annual area for each lake
annual_timeseries <-  timeseries_cleaned %>% group_by(id,year) %>%
  summarise(mean_area=mean(area))
#add annual change in area 
annual_timeseries$annual_change <- annual_means$annual_mean_change

#create df with just id, mean rate
mean_rate <- timeseries_final %>% group_by(id) %>% summarize(mean_rate=mean(mean_rate))

#Make sure all dfs are in same order
mean_rate[order(match(mean_rate$id,USlakes_final$ID)),]
US_lake_area[order(match(US_lake_area$ID,mean_rate$id)),]

#add LakeSize, Reservoir, lat, and long to df
mean_rate$LakeSize <- USlakes_final$LakeSize
mean_rate$Reservoir <- USlakes_final$RESERVOIR
mean_rate$Lat <- US_lake_area$lat
mean_rate$Long <- US_lake_area$long  

#-----------------------------------------------------------------------------------------------
######## Create plots for mean change in SA across different sized lakes and reservoirs ########
#-----------------------------------------------------------------------------------------------
#histogram --> mean rate of change for small-large lakes vs reservoirs
ggplot(mean_rate,aes(x=mean_rate,fill=LakeSize)) +geom_histogram(bins=50,binwidth=0.1,alpha=0.9) +
  scale_fill_manual(values=c("#69b3a2", "#404080","#CC0000")) +theme_classic()
#not super meaningful, but a lot of lakes experience a lot of change at the annual scale (average of doubling in area during the 1984-2015 period)
#yeah, this completely ignores the annual changes, which are probably more meaningful when comparing different climate variables

#histogram plot for small, medium, and large lake area distributions
ggplot(USlakes_final, aes(x=AREA, fill=LakeSize)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position = 'identity', bins=50) +
  scale_fill_manual(values=c("#69b3a2", "#404080","#CC0000")) +
  theme_classic() +labs(fill="")

#lake area vs year
ggplot() + geom_point(data=annual_timeseries, aes(x=mean_area, y=year)) +
  scale_color_viridis_d() + theme_classic()

#lake percent change vs year
ggplot() + geom_point(data=subset(annual_timeseries,!is.na(annual_change)), aes(x=annual_change, y=year)) +
  scale_color_viridis_d() + theme_classic()

#create map of lake area for small, med, and large lakes
ggplot() +geom_point(data=subset(US_lake_area, LakeSize %in% "small"),aes(x=lat,y=long, color=factor(RESERVOIR), alpha=AREA)) +
  borders("state") 

ggplot() +geom_point(data=subset(US_lake_area, LakeSize %in% "medium"),aes(x=lat,y=long, color=factor(RESERVOIR),alpha=AREA)) +
  borders("state")

ggplot() +geom_point(data=subset(US_lake_area, LakeSize %in% "large"),aes(x=lat,y=long, color=factor(RESERVOIR),alpha=AREA)) +
  borders("state") 
#larger lakes seem to be reservoirs more often than natural lakes

#------------------------------------------------------------------------------#
#trying to move away from discrete groups for some data viz

#plot lat/long vs area
ggplot() + geom_point(data=mean_rate, aes(x=mean_rate, y=Lat),color="dark blue",alpha=0.3)  + theme_classic()
#cool - seem to be bands around lat -80, -90, -100; potentially less change at -120 (some clustering of points)
ggplot() + geom_point(data=mean_rate, aes(x=mean_rate, y=Long),color="dark red",alpha=0.3)  + theme_classic()
#maybe some patterns, but harder to tell

#boxplot to see how area rate of change differs between lakes vs reservoirs and small-large
ggplot(subset(mean_rate, Reservoir==0),aes(x=LakeSize,y=mean_rate,fill=LakeSize)) + geom_boxplot() #lakes
ggplot(subset(mean_rate, Reservoir==1),aes(x=LakeSize,y=mean_rate,fill=LakeSize)) + geom_boxplot() #reservoirs
#hard to tell, but MAYBE small lakes/reservoirs are changing area more so than larger lakes/reservoirs

#visualizing monthly percent change from 1984-2015 for a few select/random lakes
ggplot() + geom_point(data=subset(timeseries_final,id==470558) , aes(x=rate, y=year)) +
  scale_color_viridis_d() + theme_classic()

#WA reservoir id - 712798
ggplot() + geom_point(data=subset(timeseries_final,id==712798) , aes(x=rate, y=year)) +
  scale_color_viridis_d() + theme_classic()
#NM lake id - 735485
ggplot() + geom_point(data=subset(timeseries_final,id== 735485) , aes(x=rate, y=year)) +
  scale_color_viridis_d() + theme_classic()
#ME lake id - 471229
ggplot() + geom_point(data=subset(timeseries_final,id== 471229) , aes(x=rate, y=year)) +
  scale_color_viridis_d() + theme_classic()
#FL lake id - 705957
ggplot() + geom_point(data=subset(timeseries_final,id== 705957) , aes(x=rate, y=year)) +
  scale_color_viridis_d() + theme_classic()

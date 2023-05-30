#  Read Delaware data for Precipitation and Temperature
rm(list = ls())
Sys.setenv(TZ='UTC')
library(ncdf4)
library(ggplot2)
library(fields)
library(tidyverse)
library(data.table)



#  CDO commands:

#  CDO yearsum P
# cdo -L -f nc4 -z zip_6 -b F64 timmean -seldate,1985-01-01T00:00:00,2015-12-31T00:00:00 -yearsum -sellonlatbox,-126,-66,23,50 precip.mon.total.v501.nc us_average_P_1985_2015.nc 

#  CDO yearmean T
# cdo -L -f nc4 -z zip_6 -b F64 timmean -seldate,1985-01-01T00:00:00,2015-12-31T00:00:00 -yearmean -sellonlatbox,-126,-66,23,50 air.mon.mean.v501.nc us_average_T_1985_2015.nc 

#  CDO elevation
# cdo -L -f nc4 -z zip_6 -b F64 sellonlatbox,-126,-66,23,50 GMTED2010_15n120_0500deg.nc us_elevation.nc 


#  Read realsat data
lakes_realsat <- read.csv('data/all_groups.csv') 

lakes_realsat <- lakes_realsat %>%
  select(ID,long,lat) %>%
  rename(Lon=long,Lat=lat)


mynetcdf <- 'data/us_average_P_1985_2015.nc'
ncFile <- nc_open(mynetcdf)
ilongitude <- ncvar_get(ncFile,"lon")
ilatitude <- ncvar_get(ncFile,"lat")
pr_mean <- ncvar_get(ncFile,"precip")
Lon <- ilongitude
Lat <- ilatitude
image.plot(pr_mean)

mynetcdf_t <- 'data/us_average_T_1985_2015.nc'
ncFile_t <- nc_open(mynetcdf_t)
ilongitude <- ncvar_get(ncFile_t,"lon")
ilatitude <- ncvar_get(ncFile_t,"lat")
t_mean <- ncvar_get(ncFile_t,"air")
Lon <- ilongitude
Lat <- ilatitude
image.plot(t_mean)

#  convert matrix to dataframe for T
data_t <- expand.grid(Lon, Lat) %>%
  
  rename(Lon = Var1, Lat = Var2) %>%
  
  mutate(Lon = ifelse(Lon > 180, -(360 - Lon), Lon),
         
         t_mean = as.vector(t_mean))

data_t$min_Lon <- data_t$Lon-0.25
data_t$max_Lon <- data_t$Lon+0.25

data_t$min_Lat <- data_t$Lat-0.25
data_t$max_Lat <- data_t$Lat+0.25
setDT(data_t)
setDT(lakes_realsat)

data_cross_temp <- lakes_realsat[data_t, on = .(Lon >= min_Lon , Lon <= max_Lon,                # indicate x range
                                             Lat >= min_Lat , Lat <= max_Lat), nomatch = NA, # indicate y range
                              .(t_mean,Lon, Lat, min_Lon, max_Lon, min_Lat, max_Lat,ID)]  # indicate columns in the output

data_cross_temp <- data_cross_temp %>% drop_na(c("ID")) %>%
  select(ID,Lon,Lat,t_mean)

data_temp <- left_join(data_cross_temp,lakes_realsat,by='ID')

data_temp <- data_temp %>%
  select(ID,t_mean,Lon.y,Lat.y) %>%
  rename(Lon=Lon.y,Lat=Lat.y)

#  Precipitation

data_pr <- expand.grid(Lon, Lat) %>%
  
  rename(Lon = Var1, Lat = Var2) %>%
  
  mutate(Lon = ifelse(Lon > 180, -(360 - Lon), Lon),
         
         pr_mean = as.vector(pr_mean))

data_pr$min_Lon <- data_pr$Lon-0.25
data_pr$max_Lon <- data_pr$Lon+0.25

data_pr$min_Lat <- data_pr$Lat-0.25
data_pr$max_Lat <- data_pr$Lat+0.25
setDT(data_pr)


data_cross_pr <- lakes_realsat[data_pr, on = .(Lon >= min_Lon , Lon <= max_Lon,                # indicate x range
                                                Lat >= min_Lat , Lat <= max_Lat), nomatch = NA, # indicate y range
                                 .(pr_mean,Lon, Lat, min_Lon, max_Lon, min_Lat, max_Lat,ID)]  # indicate columns in the output

data_cross_pr <- data_cross_pr %>% drop_na(c("ID")) %>%
  select(ID,Lon,Lat,pr_mean)

data_precip <- left_join(data_cross_pr,lakes_realsat,by='ID')

data_precip <- data_precip %>%
  select(ID,pr_mean,Lon.y,Lat.y) %>%
  rename(Lon=Lon.y,Lat=Lat.y)

data_pr_temp <- left_join(data_temp,data_precip,by=c('ID','Lon','Lat'))

data_pr_temp <- data_pr_temp[, c(1,3,4,2,5)]

#  Read elevation file

file <- 'data/us_elevation.nc'
ncFile_alt <- nc_open(file)
ilongitude <- ncvar_get(ncFile_t,"lon")
ilatitude <- ncvar_get(ncFile_t,"lat")
elev_mean <- ncvar_get(ncFile_alt,"elevation")
image.plot(elev_mean)

Lon=ilongitude
Lat=ilatitude

data_elev <- expand.grid(Lon, Lat) %>%
  
  rename(Lon = Var1, Lat = Var2) %>%
  
  mutate(Lon = ifelse(Lon > 180, -(360 - Lon), Lon),
         
         elev_mean = as.vector(elev_mean))

data_elev$min_Lon <- data_elev$Lon-0.25
data_elev$max_Lon <- data_elev$Lon+0.25

data_elev$min_Lat <- data_elev$Lat-0.25
data_elev$max_Lat <- data_elev$Lat+0.25
setDT(data_elev)


data_cross_elev <- lakes_realsat[data_elev, on = .(Lon >= min_Lon , Lon <= max_Lon,                # indicate x range
                                               Lat >= min_Lat , Lat <= max_Lat), nomatch = NA, # indicate y range
                               .(elev_mean,Lon, Lat, min_Lon, max_Lon, min_Lat, max_Lat,ID)]  # indicate columns in the output

data_cross_elev <- data_cross_elev %>% drop_na(c("ID")) %>%
  select(ID,Lon,Lat,elev_mean)

data_elevation <- left_join(data_cross_elev,lakes_realsat,by='ID')

data_elevation <- data_elevation %>%
  select(ID,elev_mean,Lon.y,Lat.y) %>%
  rename(Lon=Lon.y,Lat=Lat.y)

data_final <- left_join(data_pr_temp,data_elevation,by=c('ID','Lon','Lat'))

write.csv(data_final,'data/pr_temp_elev_us_1985_2015.csv')

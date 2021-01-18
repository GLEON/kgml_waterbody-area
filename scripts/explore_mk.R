#  Trends in lake/reservoir area using Mann Kendall modified version by Hamed and Rao (1998)

rm(list = ls())
Sys.setenv(TZ='UTC')

library(zoo)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(hydroTSM)
library(data.table)
library(modifiedmk)
library(sf)


#  reading files ----
area_timeseries <- readRDS("data/area_timeseries.rds")
dt_us_pnt <- sf::st_read("data/dt_us_pnt.gpkg")
dt_us <- sf::st_read("data/dt_us.gpkg")

area_timeseries[area_timeseries == -1] <- NA # setting -1 as NA

data <- area_timeseries[,c("id","area","year")]

#  Calculating annual area per lake, this mean may be biased since we have outliers and 
#  lots of missing data
data <- aggregate(data,by = list(data$year,data$id), FUN = mean,na.rm=TRUE) 

data <- data[,c("id","area","year")]

# ommit NA's for the analysis
data <- data[complete.cases(data), ] 

#  apply Mann Kendall trend analysis by id
data_mk <- lapply(split(data,data$id),function(data) ((mmkh(as.numeric(data$area),ci=0.95))))

# Getting the P-value for significance and Sen's slope for magnitude of change
data_mk <- lapply(data_mk, function(x) {x <- as.data.frame(t(x[c("new P-value","Sen's slope")]))})

data_mk <- rbindlist( data_mk, idcol = TRUE )

colnames(data_mk)[1]<- "ID"

data_mk$ID <- as.numeric(data_mk$ID)

# merge both dataframes for the plot
data_mk <- merge(data_mk,dt_us_pnt,by="ID",all.x=TRUE, all.y=TRUE)

#  Loading joe's function
usa_sf <- function(crs){
  res <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  state_key <- data.frame(state = datasets::state.abb,
                          ID = tolower(datasets::state.name),
                          stringsAsFactors = FALSE)
  res <- dplyr::left_join(res, state_key, by = "ID")
  dplyr::filter(res, !is.na(.data$state))
}

usa_outline     <- usa_sf()

#  Rename the column names 
colnames(data_mk)<- c("ID","pvalue","sen","res","hylak","hylakfrc","area","tag","continent","geom")


data_mk$sign <- with(data_mk, ifelse(sen  < 0, "negative",
                                  ifelse(sen > 0, "positive",
                                    ifelse(sen == 0, "no trend",""))))

#  if the p-value >= 0.05, the trend is significant
data_mk$sig <- ifelse(data_mk$pvalue >= 0.05,"significant","not significant")


data_mk <- data_mk %>%
  mutate(sig = if_else(is.na(sig), "not significant", sig))

data_mk$raw <- paste0(data_mk$sign," ",data_mk$sig)

data_mk$raw <- ifelse(data_mk$raw=="no trend significant","no trend",data_mk$raw)

data_mk <- st_as_sf(data_mk)

# png("Trend_mann_kendall.png", units="in", width=11, height=6, res=300)
ggplot() +
  geom_sf(data = usa_outline,fill=NA) +
  geom_sf(data = data_mk, aes(color=raw, size = sen,shape=raw))+
  scale_color_manual(name="Legend",values=c("red","red","grey","blue","blue")) +
  theme_map() +
  scale_size_area(guide = FALSE)+
  ggtitle("Trend",
          subtitle = "")+
  scale_shape_manual(name="Legend",values=c(1,19,1,1,19))+
  theme( plot.background = element_blank(),
         panel.background = element_blank())

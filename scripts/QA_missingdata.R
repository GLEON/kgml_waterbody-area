# This script is to analyze realsat data 
rm(list = ls())
# setwd("C:/Users/Maartje/OneDrive - McGill University/Lake_Expedition_2020/Realsat/Lake_expedition")

library(zoo)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(hydroTSM)
library(data.table)
library(modifiedmk)
library(sf)

#  reading files ---- choose all lakes ('area_timeseries_all') or lakes > 80ha ('area_timeseries')

area_timeseries <- readRDS("data/area_timeseries.rds")
dt_us_pnt <- sf::st_read("data/dt_us_pnt.gpkg")


#  setting all -1 to NA
area_timeseries[is.na(area_timeseries)] <- -1

#  setting seasons 
area_timeseries <- mutate(area_timeseries,month=as.numeric(month))

area_timeseries <- mutate(area_timeseries,season=case_when(
  month %in%  9:11 ~ "Fall",
  month %in%  c(12, 1, 2)  ~ "Winter",
  month %in%  3:5  ~ "Spring",
  month %in% 6:8 ~ "Summer"))


#  Counting number of months available per season per year, per lake/reservoir ----
area_timeseries$count <- ave(area_timeseries$area == "-1",area_timeseries$year, area_timeseries$id,area_timeseries$season, FUN=cumsum)

#  Group by id, year and season and get the max count per year
area_count <- area_timeseries %>% group_by(id,year,season) %>% summarise(max_count=max(count))

data <- filter(area_count,season=="Spring")

png("figures/Histogram_data_quality_spring.png", units="in", width=11, height=8, res=300)
ggplot(data, aes(x = max_count)) +
  geom_histogram(position = "dodge", binwidth = 1, col = "black") +
  facet_wrap(vars(year)) +
  scale_x_continuous(breaks = c(0,1,2,3)) +
  labs(x="No. of missing months",y="No. of lakes with A > 80km2",title="") +
  # geom_vline(xintercept = 5, color = "red") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     strip.placement = "outside",
                     strip.text = element_text(size = 11, face = "bold"),
                     axis.text = element_text(size = 11),
                    axis.title = element_text(size = 14))
dev.off()



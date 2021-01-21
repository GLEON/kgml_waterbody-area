# This script is to analyze missing data of realsat datasets
rm(list = ls())

library(zoo)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(hydroTSM)
library(data.table)
library(modifiedmk)
library(sf)
# source("scripts/00_functions.R")
# source("scripts/00_packages.R")


#  reading files ----
area_timeseries <- readRDS("data/area_timeseries.rds")
dt_us_pnt <- sf::st_read("data/dt_us_pnt.gpkg")
dt_us <- sf::st_read("data/dt_us.gpkg")

#  setting all NA to -1
area_timeseries[is.na(area_timeseries)] <- -1

#  Counting number of months available per year per lake/reservoir ----
area_timeseries$count <- ave(area_timeseries$area == "-1", area_timeseries$id,area_timeseries$year, FUN=cumsum)
area_count <- area_timeseries %>% group_by(id,year) %>% top_n(1, count)
area_count <- area_count[,c("id","year","count")]

#  ploting a heatmap of data availability
area_count_heatmap <- area_count[1:2243,] # selecting some lakes
area_count_heatmap$id <- as.character(area_count_heatmap$id)

textcol <- "grey40"

png("Data_availability.png", units="in", width=11, height=6, res=300)
ggplot(area_count_heatmap,aes(x=year,y=id,fill=count))+
  geom_tile(colour="white",size=0.2,height=0.6)+
  guides(fill=guide_legend(title="# Months missing data"))+
  labs(x="",y="",title="")+
  scale_fill_viridis_c()+
  scale_y_discrete(expand=c(0,0))+
  theme_grey(base_size=10)+
  theme(legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=7,face="bold"),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(size=10,colour=textcol),
        axis.text.y=element_text(vjust=0.6,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=14,face="bold"))+
  labs(y="ID",x=element_blank(), colour = "")+theme_bw()
dev.off()

# Plotting a histogram of 'number of months with data gaps' by 'number of lakes', faceted by year (excluding 0 counts and 2015)
area_count_histo <- area_count %>% 
  filter(count > 0) %>% 
  filter(year != '2015')

ggplot(data = area_count_histo, aes(count)) +
  geom_histogram(position = "dodge", binwidth = 1) +
  facet_wrap(~year) +
  scale_x_continuous(breaks = c(1,3,5,7,9,11)) +
  labs(x="No. of missing months",y="No. of lakes",title="") +
  geom_vline(xintercept = 5, color = "red")

#Plotting a histogram showing number of lakes with no gaps by year
area_count_zero <- area_count %>% 
  filter(count == 0)

ggplot(data = area_count_zero, aes(year)) +
  geom_histogram(position = "dodge", binwidth = 1) +
  labs(x="",y="No. of lakes",title="")

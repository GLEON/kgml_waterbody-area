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


#  Counting number of months available per year per lake/reservoir ----
area_timeseries$count <- ave(area_timeseries$area == "-1", area_timeseries$id,area_timeseries$year, FUN=cumsum)
# area_timeseries_all$count <- ave(area_timeseries_all$area == "-1", area_timeseries_all$id,area_timeseries_all$year, FUN=cumsum)

#  Group by id and year and get the max count per year
area_count <- area_timeseries %>% group_by(id,year) %>% summarise(max_count=max(count))



# Plotting a histogram of 'number of months with data gaps' by 'number of lakes', faceted by year (excluding 0 counts and 2015)
lake_count <- length(unique(area_all_count$id))
area_count_histo <- area_count %>% 
  #filter(max_count > 0) %>% 
  filter(year != '2015')

png("Histogram_data_quality1.png", units="in", width=11, height=8, res=300)
ggplot(data = area_count_histo, aes(x = max_count)) +
  geom_histogram(position = "dodge", binwidth = 1, col = "black") +
  facet_wrap(~year) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12)) +
  labs(x="No. of missing months",y="No. of lakes (total = 103,932)",title="") +
  geom_vline(xintercept = 5, color = "red") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     strip.placement = "outside",
                     strip.text = element_text(size = 11, face = "bold"),
                     axis.text = element_text(size = 11),
                    axis.title = element_text(size = 14))
dev.off()

#Plotting a histogram showing number of lakes with no gaps by year
area_count_zero <- area_count %>% 
  filter(max_count == 0)

png("Histogram_data_quality2.png", units="in", width=9, height=6, res=300)
ggplot(data = area_count_zero, aes(year)) +
  geom_histogram(position = "dodge", binwidth = 1, col = "black") +
  labs(x="",y="No. of lakes with no data gaps (total = 103,932)",title="") +
  scale_x_continuous(breaks = c(1985,1990,1995,2000,2005,2010,2015)) +
  theme_bw() + theme(axis.text = element_text(size = 11),
                     axis.title = element_text(size = 12))
dev.off()



area_count <- area_count[1:2243,] 
area_count$id <- as.character(area_count$id)

#  ploting a heatmap of data availability
textcol <- "grey40"

# png("./figures/Data_availability_heatmap.png", units="in", width=11, height=6, res=300)
 ggplot(area_count,aes(x=year,y=id,fill=max_count))+
   geom_tile(colour="white",size=0.2,height=0.6)+
   guides(fill=guide_legend(title="# missing\nmonths"))+
   labs(x="",y="",title="")+
   scale_fill_viridis_c()+
   scale_y_discrete(expand=c(0,0))+
   theme_grey(base_size=10)+ labs(y="ID",x=element_blank(), colour = "")+theme_bw() +
   theme(legend.position="right",legend.direction="vertical",
         legend.title=element_text(colour=textcol),
         legend.margin=margin(grid::unit(0,"cm")),
         legend.text=element_text(colour=textcol,size=10,face="bold"),
         legend.key.height=grid::unit(1,"cm"),
         legend.key.width=grid::unit(0.9,"cm"),
         text=element_text(size=16),
         axis.text.x=element_text(size=16,colour=textcol),
         axis.text.y=element_blank(),
         axis.ticks=element_line(size=0.4),
         plot.background=element_blank(),
         panel.border=element_blank(),
         plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
         plot.title=element_text(colour=textcol,hjust=0,size=14,face="bold"))
#dev.off()   


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
area_timeseries_all <- readRDS("data/area_timeseries_all.rds")
# area_timeseries <- readRDS("data/area_timeseries.rds")

#  setting all NA to -1
# area_timeseries[is.na(area_timeseries)] <- -1
area_timeseries_all[is.na(area_timeseries_all)] <- -1


#  Counting number of months available per year per lake/reservoir ----
# area_timeseries$count <- ave(area_timeseries$area == "-1", area_timeseries$id,area_timeseries$year, FUN=cumsum)

# do not execute this xxxx
# area_timeseries_all$count <- ave(area_timeseries_all$area == "-1", area_timeseries_all$id,area_timeseries_all$year, FUN=cumsum)

#  Group by id and year and get the max count per year
# xxxx
# area_count <- area_timeseries_all %>% 
#   group_by(id,year) %>% 
#   summarise(max_count=max(count)) %>% 
#   mutate(perc_missing = max_count/12 * 100)
#area_all_count <- area_timeseries_all %>% group_by(id,year) %>% summarise(max_count=max(count))

# Get percentage of missing data per lake
area_timeseries_all <- mutate(area_timeseries_all, missing = (area == -1))
# perc_missing <- area_timeseries_all %>% 
#   group_by(id) %>%
#   summarise(perc_missing = round(length(id[area_rm_missing == -1])/length(id)*100))

# Get the runlength of missing values:
lake_ids <- unique(area_timeseries_all$id)
# RLE_summary <- data.frame(matrix(ncol = 4))
# colnames(RLE_summary) <- c("id", "min_rle", "max_rle", "count_rle")

# selecting post 2000
area_timeseries_post2000 <- filter(area_timeseries_all,year >= 2000)
# creating summary post 2000 

# getting the same with a faster code 
rlength <- function(x){
  
  rlen <- rle(x$missing)
  rlen.filter <- filter(data.frame(length = rlen$lengths, values = rlen$values), values == 'TRUE')
  min <- min(rlen.filter$length)
  max <- max(rlen.filter$length)
  length <- length(rlen.filter$length)
  id <- x$id[1]
  df <- data.frame(id=id,min=min,max=max,length=length)
}

#  getting all lakes----

data_rle_all <- lapply(split(area_timeseries_all,area_timeseries_all$id),function(area_timeseries_all) ((rlength(area_timeseries_all))))
data_rle_all <- rbindlist(data_rle_all, idcol = TRUE)
data_rle_all <- na.omit(data_rle_all)
data_rle_all_filter <- filter(data_rle_all,min != "Inf",max!= "-Inf" )

# x <- filter(area_timeseries_all,id=="574514")

# getting rle for post 2000 ----
data_rle <- lapply(split(area_timeseries_post2000,area_timeseries_post2000$id),function(area_timeseries_post2000) ((rlength(area_timeseries_post2000))))
data_rle <- rbindlist(data_rle, idcol = TRUE)
#  removing na
data_rle <- na.omit(data_rle)
# removing lakes that don't have missing data
data_rle_filter <- filter(data_rle,min != "Inf",max!= "-Inf" )
# some lakes don't have any missing gaps e.g. id=574514
#  around 15750 don't have any missing data at all!


# for (i in lake_ids) {
#   df <- filter(area_timeseries_all, id == i)
#   RLE <- rle(df$missing)
#   df_RLE <- filter(data.frame(length = RLE$lengths, values = RLE$values), values == 'TRUE')
#   RLE_summary[(nrow(RLE_summary) + 1), 1] <- i
#   RLE_summary[(nrow(RLE_summary)), 2] <- min(df_RLE$length)
#   RLE_summary[(nrow(RLE_summary)), 3] <- max(df_RLE$length)
#   RLE_summary[(nrow(RLE_summary)), 4] <- length(df_RLE$length)
# }
# 
# RLE_summary <- na.omit(RLE_summary)
# RLE_summary <- merge(RLE_summary, perc_missing)
# write.csv(RLE_summary, "RLE_summary.csv")

#  all years pre 2000 --------------- ------------------------------------------

png("figures/Histogram_qa_rle_min_all.png", units="in", width=9, height=6, res=300)
ggplot(data = data_rle_all_filter, aes(x = min)) +
  geom_histogram(position = "dodge", binwidth = 1, col = "black") +
  scale_x_continuous(breaks = seq(1,26,2)) +
  labs(x="Min_rle",y="No. of lakes (Total=103,932 lakes)",title="") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     strip.placement = "outside",
                     strip.text = element_text(size = 11, face = "bold"),
                     axis.text = element_text(size = 11),
                     axis.title = element_text(size = 14))
dev.off()


png("figures/Histogram_qa_rle_max_all.png", units="in", width=9, height=6, res=300)
ggplot(data = data_rle_all_filter, aes(x = max)) +
  geom_histogram(position = "dodge", binwidth = 1, col = "black") +
  scale_x_continuous(breaks = seq(0,221,10)) +
  labs(x="Max_rle",y="No. of lakes (Total=103,932 lakes)",title="") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     strip.placement = "outside",
                     strip.text = element_text(size = 11, face = "bold"),
                     axis.text = element_text(size = 11),
                     axis.title = element_text(size = 14))
dev.off()

#  some lakes have lots of missing data e.g. 526033



#  post 2000 -------------------------------------------------------------------

png("figures/Histogram_qa_rle_min_post2000_all.png", units="in", width=9, height=6, res=300)
ggplot(data = data_rle_filter, aes(x = min)) +
  geom_histogram(position = "dodge", binwidth = 1, col = "black") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  labs(x="Min_rle",y="No. of lakes (Total=103,932 lakes)",title="") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     strip.placement = "outside",
                     strip.text = element_text(size = 11, face = "bold"),
                     axis.text = element_text(size = 11),
                     axis.title = element_text(size = 14))
dev.off()

png("figures/Histogram_qa_rle_max_post2000_all.png", units="in", width=9, height=6, res=300)
breaks <- seq(0,92,3)
ggplot(data = data_rle_filter, aes(x = max)) +
  geom_histogram(position = "dodge", binwidth = 1, col = "black") +
  scale_x_continuous(breaks = breaks) +
  labs(x="Max_rle",y="No. of lakes (Total=103,932 lakes)",title="") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     strip.placement = "outside",
                     strip.text = element_text(size = 11, face = "bold"),
                     axis.text = element_text(size = 11),
                     axis.title = element_text(size = 14))
dev.off()



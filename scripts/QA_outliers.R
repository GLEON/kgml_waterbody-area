# This script is for the removal of outliers on RealSat data

rm(list = ls())

library(data.table)

source("scripts/99_packages.R")

#  reading files ----
area_timeseries <- readRDS("data/area_timeseries_all.rds")
dt_us_pnt <- sf::st_read("data/dt_us_pnt.gpkg")

# ephemeral lake id =="714607"
# regular lake id == 458167
df <- area_timeseries %>% filter (id=="458167")


#  selecting date and area only
df <- df %>% select(date,area)

#  setting all -1 to NA
df[df == -1] <- NA

# Outlier removal

#  method median +- 3*Median Absolute Deviation (MAD), Leys et al. (2013)
remove_outliers_MAD <- function(x, na.rm = TRUE, ...) {
  
  median <- median(x, na.rm = na.rm, ...)
  
  MAD <- mad(x,na.rm = na.rm, ...)
  
  y <- x
  
  y[x < (median - 3*MAD)] <- NA
  
  y[x > (median + 3*MAD)] <- NA
  
  return(y)
}
#  method mean +- 3*sd, Osbourne and Overbay (2004)
remove_outliers_sd <- function(x, na.rm = TRUE, ...) {
  
  mean <- mean(x, na.rm = na.rm, ...)
  
  sd <- sd(x,na.rm = na.rm, ...)
  
  y <- x
  
  y[x < (mean - 3*sd)] <- NA
  
  y[x > (mean + 3*sd)] <- NA
  
  return(y)
}
#  method Inter Quartile Range (IQR)
remove_outliers_iqr <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# applying the different methods for outlier removal
df <- df %>% mutate(area_sd = remove_outliers_sd(df$area),area_mad = remove_outliers_MAD(area),
                    area_iqr = remove_outliers_iqr(area))

plot(df$date,df$area)


#  ephemeral lake
dg 
plot(dg$date,dg$area)
#  plot
ggplot(melt(df,id.vars="date"), aes(x = date, y=value,col=variable)) + 
  geom_point()

# Notes:
#  1.- In lakes with high variability on area, MAD and IQE are not recommendable, sd
#  seems the best option

#  2.- Can high variability be related to lake size?

#  estimate sd and mean area per lake id

# set -1 as NA
area_timeseries[area_timeseries == -1] <- NA

#  estimate sd and mean_area per lake id
sd <- lapply(split(area_timeseries,area_timeseries$id),function(area_timeseries) (sd(area_timeseries$area,na.rm=TRUE)))
mean_area <- lapply(split(area_timeseries,area_timeseries$id),function(area_timeseries) (mean(area_timeseries$area,na.rm=TRUE)))

#  merging sd and area
data_aux <- setDT(data.frame(unlist(sd),unlist(mean_area)),keep.rownames = "ID")

# set ID as numeric
data_aux$ID <- as.numeric(data_aux$ID)

colnames(data_aux)[2:3] <- c("sd","mean_area")

# merge both dataframes for the plot
data <- merge(dt_us_pnt,data_aux,by="ID")

#  Loading jem's function
usa_sf <- function(crs){
  res <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  state_key <- data.frame(state = datasets::state.abb,
                          ID = tolower(datasets::state.name),
                          stringsAsFactors = FALSE)
  res <- dplyr::left_join(res, state_key, by = "ID")
  dplyr::filter(res, !is.na(.data$state))
}

usa_outline     <- usa_sf()

png("figures/variability_lakesize.png", units="in", width=9, height=6, res=300)
ggplot() +
  geom_sf(data = usa_outline,fill=NA) +
  geom_sf(data = data, aes(color=sd, size = mean_area))+
  scale_color_gradientn(colours=rainbow(5)) +
  theme_map() +
  # scale_size_area(guide = FALSE)+
  ggtitle("Variability",
          subtitle = "")+
  # scale_shape_manual(name="Legend",values=c(1,19,1,1,19))+
  theme( plot.background = element_blank(),
         panel.background = element_blank())
dev.off()




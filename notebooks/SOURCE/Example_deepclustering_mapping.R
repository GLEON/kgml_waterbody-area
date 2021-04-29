# source("scripts/99_utils.R")
library(sf)
library(dplyr)
library(ggplot2)

x_ids <- read.csv("notebooks/INPUT/REALSAT_TIMESERIES/X_ids.csv",
  stringsAsFactors = FALSE)
x_ids <- x_ids[1:3200, "id"] # notebook trains on first 3200 rows

y_pred <- read.csv("notebooks/OUTPUT/Joe/REALSAT_TIMESERIES/RESULT/y_pred.csv",
  stringsAsFactors = FALSE)
y_pred <- cbind(x_ids, y_pred)

# dt <- read.csv("data/area_timeseries.csv", stringsAsFactors = FALSE)

# dt_us_pnt <- st_read("data/dt_us_pnt.gpkg")


dt_sf <- st_read("data/dt_us_pnt.gpkg") %>%
  dplyr::filter(ID %in% y_pred$x_ids) %>%
  dplyr::left_join(y_pred, by = c("ID" = "x_ids"))

mapview::mapview(dt_sf, zcol = "cluster_id")

ggplot() +
  geom_sf(data = dt_sf, aes(color = cluster_id))
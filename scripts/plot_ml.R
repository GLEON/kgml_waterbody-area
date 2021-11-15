rm(list = ls())
library(sf)
library(dplyr)
library(ggplot2)

#  Important to load the X_ids_v2, the version with randomly selected
#  lakes
x_ids <- read.csv("ml_model/X_ids_v2.csv",
                  stringsAsFactors = FALSE)
x_ids <- x_ids[, "id"] # notebook trains on first 3200 rows

y_pred <- read.csv("ml_model/y_pred_incl_interpolation.csv",
                   stringsAsFactors = FALSE)
data_map <- as.data.frame(cbind(x_ids, y_pred))

dt_sf <- st_read("data/dt_us_pnt.gpkg") %>%
  dplyr::filter(ID %in% data_map$x_ids) %>%
  dplyr::left_join(data_map, by = c("ID" = "x_ids"))

mapview::mapview(dt_sf, zcol = "cluster_id")

ggplot() +
   geom_sf(data = dt_sf, aes(color = cluster_id))

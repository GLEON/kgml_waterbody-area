source("scripts/99_packages.R")
source("scripts/99_utils.R")

library(mapview)

# compare shapefile areas with time-series area ----
dt_area    <- readRDS("data/area_timeseries.rds")
dt_sf      <- st_read("data/dt_us_pnt.gpkg")

test_id  <- 648301 # Lake Mendota
test_pnt <- dplyr::filter(dt_sf, ID == test_id)
test_ts  <- dplyr::filter(dt_area, id == test_pnt$ID) %>%
  dplyr::mutate(area = dplyr::na_if(area, -1))

landsat_pixels_to_area(test_pnt$AREA)
# 183.51 ha
landsat_pixels_to_area(range(test_ts$area, na.rm = TRUE))
# 25.38 - 128.34 ha

# compare ReaLSAT records with those in the Hydroweb database ----

mead_raw    <- wikilake::lake_wiki(c("Honey Lake"))
mead        <- sf::st_as_sf(
  mead_raw, coords = c("Lon", "Lat"), crs = 4326) %>%
  arrange(Name)
mead_buffer <- st_buffer(mead, 0.1)

mapview(mead_buffer[1,]) + mapview(mead[1,])# + mapview(dt_sf)

mapview(dplyr::filter(dt_sf, ID == 698614))

test <- st_intersects(dt_sf, mead_buffer)
any(unlist(lapply(test, function(x) length(x) > 0)))


(
  dt_mead <- sf::st_join(dt_sf, mead_buffer) %>%
    dplyr::filter(!is.na(Name)) %>%
    arrange(Name)
)


mapview(mead_buffer[1,]) + mapview(mead[1,]) + mapview(dt_sf)
# mapview(dt_mead[1,], color = "red")

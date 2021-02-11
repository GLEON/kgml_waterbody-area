source("scripts/99_packages.R")
source("scripts/99_utils.R")

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

# ----
# dt         <- read_sf("data/ReaLSAT.shp")
# test_shp <- dplyr::filter(dt, ID == test_pnt$ID)
# test_pnt$AREA == test_shp$AREA

# test_csv <- data.frame(t(
#     read.csv(
#     get_ts_file_path(test_pnt$ID, get_ts_file_table()),
#     header = FALSE)
#   ), stringsAsFactors = FALSE)
# identical(
#   dplyr::pull(dplyr::mutate(test_csv,
#                 X3 = dplyr::na_if(X3, -1))),
#   test_ts$area
#   )

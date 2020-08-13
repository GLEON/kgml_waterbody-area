library(sf)
library(LAGOSNE)
library(dplyr)
library(ggplot2)

dt <- read_sf("data/ReaLSAT-R-2.0.shp")

dt_lagosne <- dt %>%
  st_intersects(st_zm(LAGOSNE::lg_extent)) %>%
  lapply(., function(x) length(x)) %>%
  unlist() %>%
  sapply(function(x) x!=0)
dt_lagosne <- dt[dt_lagosne,]

ggplot() +
  geom_sf(data = st_zm(LAGOSNE::lg_extent)) +
  geom_sf(data = dt_lagosne)

dt_lagosne_areas <- dt_lagosne %>%
  st_area() %>%
  units::set_units("ha") %>%
  as.numeric() %>%
  data.frame(area = .)

ggplot(data = dt_lagosne_areas, aes(area)) +
  geom_histogram() +
  scale_x_log10() +
  geom_vline(aes(xintercept = 4))

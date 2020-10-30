# ---- load_data_and_functions ----
source("scripts/99_utils.R")

library(sf)
library(dplyr)
library(ggplot2)
library(cowplot)
library(mapview)

area_timeseries <- readRDS("data/area_timeseries.rds")
dt_us_pnt       <- st_read("data/dt_us_pnt.gpkg")
usa_outline     <- usa_sf()

# mapview(dt_us_pnt[1,])

# ---- create_basic_map ----
ggplot() +
  geom_sf(data = usa_outline) +
  geom_sf(data = dt_us_pnt, size = 0.5) +
  theme_map() +
  ggtitle("Map of ReaLSAT lakes within the contiguous US")

# ---- examine_area_variations ----
calculate_medianmax_ratio <- function(x){
  if(is.na(x[1])){# trim leading NAs
    x <- x[(min(which(!is.na(rle(x)$values)))):length(x)]
  }
  if (x[1] == 0) {# trim leading zeros
    x <- x[(rle(x)$lengths[1] + 1):length(x)]
  }
  median(x, na.rm = TRUE) / max(x, na.rm = TRUE)
}

# high numbers indicate drought troughs
# low numbers indicate flood peaks
dt_medianmax_ratio <- area_timeseries %>%
  group_by(id) %>%
  summarise(ratio = calculate_medianmax_ratio(.data$area_rm_missing)) %>%
  left_join(dt_us_pnt, by = c("id" = "ID")) %>%
  dplyr::arrange(ratio) %>%
  st_as_sf()

ggplot() +
  geom_sf(data = usa_outline) +
  geom_sf(data = dt_medianmax_ratio, aes(color = ratio), size = 0.6) +
  scale_color_gradient2(midpoint = 0.5) +
  #scale_color_brewer(palette = "BrBG", type = "div") +
  theme_map() +
  ggtitle("Ratio of median area to max area",
          subtitle = "High = drought troughs \nLow  = flood peaks")

hilow_pnts <- dt_medianmax_ratio[
  c(1, nrow(dt_medianmax_ratio) - 1),] # low ratio id, high ratio id

(gg_highlow_map <- ggplot() +
  geom_sf(data = usa_outline) +
  geom_sf(data = hilow_pnts, color = c("blue", "red")) +
  # geom_sf_label(data = hilow_pnts, aes(label = ID),
  #               nudge_y = 1, nudge_x = 1) +
  theme_map())

ts_plot <- function(id, dt){
  dt <- dt[dt$id == id,]
  ggplot(data = dt) +
    geom_line(aes(x = date, y = area), col = "red") +
    geom_line(aes(x = date, y = area_rm_missing)) +
    theme_cowplot()
}

(gg_ts <- cowplot::plot_grid(
  ts_plot(hilow_pnts$id[1], area_timeseries),
  ts_plot(hilow_pnts$id[2], area_timeseries) +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank()),
  rel_widths = c(1.1, 1)
))
cowplot::plot_grid(gg_highlow_map,
                   gg_ts, ncol = 1, rel_heights = c(1.9, 1))

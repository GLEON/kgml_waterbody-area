library(sf)
library(LAGOSNE)
library(dplyr)
library(ggplot2)
library(cowplot)

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

# ---- areas ----
dt_lagosne_areas <- dt_lagosne %>%
  st_area() %>%
  units::set_units("ha") %>%
  as.numeric() %>%
  data.frame(area = .)

ggplot(data = dt_lagosne_areas, aes(area)) +
  geom_histogram() +
  scale_x_log10() +
  geom_vline(aes(xintercept = 4))

# ---- area_variations ----

median_max_ratio <- function(x){
  median(x, na.rm = TRUE) / max(x, na.rm = TRUE)
}

get_ts_file_path <- function(id){
  # id <- 572655
  f_list <- dir("data/realsat_r_2_monthly_timeseries/",
                include.dirs = TRUE, full.names = TRUE)

  test    <- lapply(f_list, dir)
  test    <- suppressWarnings(tidyr::unnest(tibble::enframe(test)))
  test$id <- gsub(".csv", "", test$value)

  folder_id <- test[which(test$id == id),]$name - 1
  if(!(length(folder_id) == 0)){
    paste0("data/realsat_r_2_monthly_timeseries/",
           (folder_id), "/",
           id, ".csv")
  }else{
    NA
  }
}
# get_ts_file_path(id)

get_area_timeseries <- function(id){
  # id <- 572655
  f_path <- get_ts_file_path(id)
  if(is.na(f_path)){
    return(NA)
  }
  res       <- data.frame(t(read.csv(f_path,
                                     header = FALSE)),
                          stringsAsFactors = FALSE)

  res       <- setNames(res, c("fill", "update", "area"))
  res$id    <- id
  res$month <- rep(
    sapply(1:12, function(x){
        if (nchar(x) < 2) {
          paste0("0", x)
        }else{
          x
        }
      }), 32) # monthly from 1984-2015
  res$year  <- rep(1984:2015, each = 12)

  # flagging steps with > 90% of pixels labelled as missing in the original data
  res$area_rm_missing                <- res$area
  res$area_rm_missing[res$fill > 90] <- NA

  res$date <- as.Date(
    sapply(seq_len(nrow(res)), function(x) {
      paste0(res[x, "year"], "-", res[x, "month"], "-", "15")
      }),
    format = "%Y-%m-%d")

  res
}

if(!file.exists("data/area_timeseries.rds")){
  test <- lapply(seq_len(nrow(dt_lagosne)),
                 function(i){
                   print(i)
                   get_area_timeseries(dt_lagosne$ID[i])
                 })
  saveRDS(test, "data/area_timeseries.rds")
}

ts_plot <- function(id, dt){
  dt <- dt[dt$id == id,]
  ggplot(data = dt) +
    geom_line(aes(x = date, y = area), col = "red") +
    geom_line(aes(x = date, y = area_rm_missing))
}


test  <- readRDS("data/area_timeseries.rds")
test2 <- test[which(unlist(lapply(test, function(x) !is.null(nrow(x)))))]
test2 <- dplyr::bind_rows(test2)

i <- 1
cowplot::plot_grid(
  ts_plot(unique(test2$id)[i], test2),
  ggplot() + geom_sf(data = dt[which(dt$ID == as.numeric(unique(test2$id)[i])),])
)

i <- 2
cowplot::plot_grid(
  ts_plot(unique(test2$id)[i], test2),
  ggplot() + geom_sf(data = dt[which(dt$ID == as.numeric(unique(test2$id)[i])),])
)

mapview::mapview(dt[which(dt$ID == as.numeric(unique(test2$id)[i])),])

# ---- map_medianmax_ratio ----
dt_lagosne_centroid <- st_centroid(dt_lagosne)

test3 <- test2 %>%
  group_by(id) %>%
  summarise(ratio = median_max_ratio(.data$area_rm_missing))

hist(test3$ratio)
# high numbers indicate drought troughs
# low numbers indicate flood peaks

testaa <- dplyr::left_join(dt_lagosne_centroid, test3,
                           by = c("ID" = "id"))
plot(testaa["ratio"])

library(sf)
library(LAGOSNE)
library(dplyr)
library(ggplot2)
library(cowplot)

# from jsta::usa_sf
usa_sf <- function(crs){
  res <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
  state_key <- data.frame(state = datasets::state.abb,
                          ID = tolower(datasets::state.name),
                          stringsAsFactors = FALSE)
  res <- dplyr::left_join(res, state_key, by = "ID")
  dplyr::filter(res, !is.na(.data$state))
}

dt <- read_sf("data/ReaLSAT-R-2.0.shp")

dt_lagosne <- dt %>%
  st_within(st_zm(LAGOSNE::lg_extent)) %>%
  lapply(., function(x) length(x)) %>%
  unlist() %>%
  sapply(function(x) x!=0)
dt_lagosne <- dt[dt_lagosne,]

lg_ne_states <- usa_sf()[
  unlist(lapply(
    st_contains(usa_sf(), dt_lagosne),
    function(x) length(x) > 7)),] %>%
  dplyr::filter(!(state %in% c("AR", "KS", "NE")))

dt_lagosne_states <- dt_lagosne %>%
  st_within(lg_ne_states) %>%
  lapply(., function(x) length(x)) %>%
  unlist() %>%
  sapply(function(x) x!=0)
dt_lagosne <- dt_lagosne[dt_lagosne_states,]


# ---- areas ----
dt_lagosne_areas <- dt_lagosne %>%
  st_area() %>%
  units::set_units("ha") %>%
  as.numeric() %>%
  data.frame(area = .)

# ---- area_variations ----
# r_seq <- rnorm(40, 2)
# median_max_ratio(r_seq)
# x <- c(rep(0, 20), r_seq)
# x <- c(rep(NA, 20), r_seq)
# median_max_ratio(x)

# median_max_ratio(
#   dplyr::filter(test2, id == 471533)$area_rm_missing)

median_max_ratio <- function(x){
  if(is.na(x[1])){# trim leading NAs
    x <- x[(min(which(!is.na(rle(x)$values)))):length(x)]
  }
  if (x[1] == 0) {# trim leading zeros
    x <- x[(rle(x)$lengths[1] + 1):length(x)]
  }
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

# mapview::mapview(dt[which(dt$ID == as.numeric(unique(test2$id)[i])),])

# ---- map_medianmax_ratio ----
dt_lagosne_centroid <- st_centroid(dt_lagosne)

test3 <- test2 %>%
  group_by(id) %>%
  summarise(ratio = median_max_ratio(.data$area_rm_missing))

# hist(test3$ratio)
# high numbers indicate drought troughs
# low numbers indicate flood peaks

testaa <- dplyr::left_join(dt_lagosne_centroid, test3,
                           by = c("ID" = "id"))

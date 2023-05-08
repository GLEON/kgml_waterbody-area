
#' Generates an sf vector object of the US. Copied from jsta::usa_sf
#'
usa_sf <- function(crs) {
  res <- sf::st_as_sf(
    rnaturalearth::ne_states(country = "united states of america"))
  state_key <- data.frame(state = datasets::state.abb,
    ID = tolower(datasets::state.name),
    stringsAsFactors = FALSE)
  res <- dplyr::left_join(res, state_key, by = c("postal" = "state")) %>%
    dplyr::rename(state = postal) %>%
    dplyr::select(state, ID) %>%
    dplyr::filter(!is.na(.data$state))
  res
}

#' Construct a table of file paths
#'
#' @example
#' get_ts_file_table()
get_ts_file_table <- function() {
  folder_list <- dir("data/monthly_timeseries/",
    include.dirs = TRUE, full.names = TRUE)

  file_list    <- lapply(folder_list, dir)
  file_list    <- suppressWarnings(tidyr::unnest(tibble::enframe(file_list)))
  file_list$id <- gsub(".csv", "", file_list$value)
  file_list
}

#' Discover the file path to the monthly timeseries file associated with a
#' given lake.
#'
#' @example
#' id <- 458167
#' get_ts_file_path(id, get_ts_file_table())
#'
get_ts_file_path <- function(id, file_table) {
  folder_id <- file_table[which(file_table$id == id), ]$name - 1
  if (!(length(folder_id) == 0)) {
    paste0("data/monthly_timeseries/",
      (folder_id), "/",
      id, ".csv")
  } else {
    NA
  }
}

#' Given a lake id, return it's area timeseries.
#'
#' @example
#' id <- 458167
#' file_table <- get_ts_file_table()
#' get_area_timeseries(id, f_table)
get_area_timeseries <- function(id, file_table) {
  f_path <- get_ts_file_path(id, file_table)
  if (is.na(f_path)) {
    return(NA)
  }
  res       <- data.frame(t(read.csv(f_path,
    header = FALSE)),
  stringsAsFactors = FALSE)

  res       <- setNames(res, c("fill", "update", "area"))
  res$id    <- id
  res$month <- rep(
    sapply(1:12, function(x) {
      if (nchar(x) < 2) {
        paste0("0", x)
      } else {
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

landsat_pixels_to_area <- function(x) {
  units::set_units(
    units::as_units(x * 30 * 30, "m2"), "ha")
}
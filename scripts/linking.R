source("scripts/99_packages.R")
source("scripts/99_utils.R")

library(mapview)
library(prism); prism_set_dl_dir("data/prism")
library(wikilake)

dt_us_pnt       <- st_read("data/dt_us_pnt.gpkg")

# ---- link to a precipitation dataset ----
get_prism_annual(type = "ppt", years = 2010, keepZip = FALSE)

load_prism <- function(year){
  prism_files <- pd_to_file(prism_archive_ls())

  prism_query_string <- paste0("PRISM_ppt_stable_4kmM3_", year, "_bil")

  prism_query_path <- prism_files[
    match(
      prism_query_string,
      tools::file_path_sans_ext(basename(prism_files))
    )
  ]

  raster::raster(prism_query_path)
}

dt_prism            <- load_prism(2010)
dt_us_pnt$ppt_prism <- raster::extract(dt_prism, as_Spatial(dt_us_pnt))

plot(dt_us_pnt[,"ppt_prism"])

mapview(dt_us_pnt[1,]) +
  mapview(raster::mask(dt_prism, as_Spatial(dt_us_pnt[1,])))

# ---- link to a lake point layer ----
wi_raw    <- wikilake::lake_wiki(c("Lake Mendota", "Trout Lake (Wisconsin)"))
wi        <- sf::st_as_sf(
  wi_raw, coords = c("Lon", "Lat"), crs = 4326) %>%
  arrange(Name)
wi_buffer <- st_buffer(wi, 0.03)

mapview(wi_buffer[1,]) + mapview(wi[1,])

(
  dt_wi <- sf::st_join(dt_us_pnt, wi_buffer) %>%
    dplyr::filter(!is.na(Name)) %>%
    arrange(Name)
  )

mapview(wi_buffer[1,]) + mapview(wi[1,]) + mapview(dt_wi[1,], color = "red")

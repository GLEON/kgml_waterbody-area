# load data, packages, and helper functions
source("scripts/99_packages.R")
source("scripts/99_utils.R")

dt         <- read_sf("data/ReaLSAT.shp")
us_outline <- usa_sf()

# pull lakes in continental US
within_us <- dt %>%
  st_within(us_outline) %>%
  lapply(., function(x) length(x) != 0) %>%
  unlist()
dt_us <- dt[within_us,]

# pull lakes within set area limits
dt_us_areas <- dt_us %>%
    st_area() %>%
    units::set_units("ha") %>%
    as.numeric() %>%
  data.frame(area = .) %>%
  mutate(id = dt_us$ID) %>%
  arrange(desc(area)) %>%
  # dplyr::filter(area >= 10 & area < 2000)
  dplyr::filter(area >= 80)

dt_us     <- dt_us[dt_us$ID %in% dt_us_areas$id,]
# dt_us_pnt <- st_centroid(dt_us)

if(!file.exists("data/area_timeseries.rds")){
  start_time <- Sys.time()

  ts_file_table <- get_ts_file_table()

  pb <- progress_bar$new(total = nrow(dt_us),
        format = "  pulling timeseries for lake :id [:bar] :percent eta: :eta",
        clear = FALSE, width = 60)

  res <- lapply(seq_len(nrow(dt_us)),
  # res <- lapply(seq_len(12),
                 function(i){
                   # print(i)
                   pb$tick(tokens = list(id = dt_us$ID[i]))
                   get_area_timeseries(dt_us$ID[i], ts_file_table)
                 })
  res_combined <- res[which(unlist(lapply(res, function(x) !is.null(nrow(x)))))]
  res_combined <- dplyr::bind_rows(res_combined)

  saveRDS(res_combined, "data/area_timeseries.rds")
  (elapsed_time <- start_time - Sys.time())
}

# some lakes to investigate because mapview doesn't show them overlying water:
# ID: 696939

# red: time periods when more than 90 percent of pixels for a lake were NA
# black: all other time periods
ts_plot <- function(dt, id){
  dt <- dt[dt$id == id,]
  ggplot(data = dt) +
    geom_line(aes(x = date, y = area), col = "red") +
    geom_line(aes(x = date, y = area_rm_missing)) +
    ylab("Area (sq km)") +
    ggtitle(paste0("Lake: ", id)) +
    theme_cowplot()
}

# mapview::mapview(dt_us[dt_us$ID == 719196,])
# ts_plot(res_combined, 719196)

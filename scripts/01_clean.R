# load data, packages, and helper functions
source("scripts/99_packages.R")
source("scripts/99_utils.R")

dt <- read_sf("data/ReaLSAT.shp")

# pull lakes in continental US
us_outline <- usa_sf()
dt_us <- dt %>%
  st_within(us_outline) %>%
  lapply(., function(x) length(x)) %>%
  unlist() %>%
  sapply(function(x) x!=0)
dt_us <- dt[dt_us,]

# pull lakes within set area limits
dt_us_areas <- dt_us %>%
  st_area() %>%
  units::set_units("ha") %>%
  as.numeric() %>%
  data.frame(area = .) %>%
  mutate(id = dt_us$ID) %>%
  dplyr::filter(area >= 10 & area < 2000)
dt_us <- dt_us[dt_us$ID %in% dt_us_areas$id,]

if(!file.exists("data/area_timeseries.rds")){
  pb <- progress_bar$new(total = nrow(dt_us),
        format = "  pulling timeseries for lake :id [:bar] :percent eta: :eta",
        clear = FALSE, width = 60)

  ts_file_table <- get_ts_file_table()
  # start_time <- Sys.time()
  res <- lapply(seq_len(nrow(dt_us)),
  # res <- pblapply(seq_len(12),
                 function(i){
                   # print(i)
                   pb$tick(tokens = list(id = dt_us$ID[i]))
                   get_area_timeseries(dt_us$ID[i], ts_file_table)
                 })
  # (elapsed_time <- start_time - Sys.time())

  saveRDS(res, "data/area_timeseries.rds")
}

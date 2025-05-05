packages <- c("readr", "purrr", "dplyr", "janitor", "lubridate")
installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)
if (length(to_install)) install.packages(to_install)

library(readr)
library(purrr)
library(dplyr)
library(janitor)
library(lubridate)

data_dir <- "Data"
csv_files <- list.files(
  path       = data_dir,
  pattern    = "\\.csv$",
  full.names = TRUE
)

if (length(csv_files) == 0) {
  stop("No CSV files found in 'Data/'. Check your folder path.")
}

uber_data <- csv_files %>%
  set_names(basename(.)) %>%                      
  map_dfr(read_csv, .id = "raw_file") %>%       
  clean_names() %>%                                
  rename(
    lat = any_of(c("lat","latitude")),
    lon = any_of(c("lon","longitude"))
  ) %>%
  mutate(
    date_time = mdy_hms(date_time),
    date      = as_date(date_time),
    month     = month(date_time, label = TRUE, abbr = FALSE),
    day       = day(date_time),
    hour      = hour(date_time),
    wday      = wday(date_time, label = TRUE),
    week      = week(date_time)
  )

rds_path <- file.path(data_dir, "uber_data_all.rds")
saveRDS(uber_data, file = rds_path, compress = "xz")

message("✅ Wrote ", rds_path, 
        " (", format(object.size(uber_data), units="auto"), ")")
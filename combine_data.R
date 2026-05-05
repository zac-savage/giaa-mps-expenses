# Packages
library(tidyverse)
library(here)
library(janitor)

# Check data available
files <- list.files(path = here("data"), pattern = ".csv", full.names = FALSE) 
files

# Read all data
read_file <- function(file) {
  read.csv(here("data", file),
           colClasses = "character") |>
    clean_names() |>
    mutate(source_file = basename(file))
}

index <- 1:15

data_list <- lapply(files[index], read_file)
names(data_list) <- files[index]

# Bind all data to get a time series
time_series <- bind_rows(data_list)

# Extract date, assuming FY and taking first day of FY as single point
time_series <- time_series |>
  mutate(
    year_pair = str_extract(source_file, "\\d{2}_\\d{2}"),
    start_year = 2000 + as.numeric(substr(year_pair, 1, 2)),
    fy_start_date = as.Date(paste0(start_year, "-04-01"))
    )

# Validate this is correct
time_series |>
  ggplot(aes(x = fy_start_date, y = source_file)) +
  geom_point() +
  theme(legend.position = "none")

# Packages
library(tidyverse)
library(here)
library(janitor)
library(sf)

# Check sf# Check data available
files <- list.files(path = here("data"), pattern = ".csv", full.names = FALSE) 
files

# Read all data
read_file <- function(file) {
  read.csv(here("data", file),
           colClasses = "character" # Issues with mismatched col types so forcing character
           ) |>
    clean_names() |>
    mutate(source_file = basename(file))
}

index <- 1:15 # Which files should be joined

data_list <- lapply(files[index], read_file)
names(data_list) <- files[index]

# Bind all data to get a time series
time_series <- bind_rows(data_list)

# Extract date, assuming FY and taking first day of FY as single point
time_series <- time_series |>
  mutate(
    year_pair = str_extract(source_file, "\\d{2}_\\d{2}"),
    start_year = 2000 + as.numeric(substr(year_pair, 1, 2)),
    year_start = as.Date(paste0(start_year, "-04-01"))
    )

# Validate this is correct visually
time_series |>
  ggplot(aes(x = year_start, y = source_file)) +
  geom_point() +
  theme(legend.position = "none")

# Clean up the messy numeric columns
clean_currency <- function(col) {
  col |>
    str_replace("^-£", "£-") |>
    parse_number()
}

# Test function
time_series$office_spend_clean <- clean_currency(time_series$office_spend)

# Check random rows
time_series |> slice_sample(n = 5) |> select(office_spend, office_spend_clean)

# Visually check function is working
time_series |>
  group_by(year_start) |>
  summarise(mean_spend = mean(office_spend_clean, na.rm = TRUE)) |>
  ggplot(aes(x = year_start, y = mean_spend)) +
  geom_line() +
  geom_smooth()

message("Function to extract spend is behaving as expected")

# Apply function accross the data
time_series$overall_total_spend_for_this_financial_year <- clean_currency(time_series$overall_total_spend_for_this_financial_year)
time_series$office_spend <- clean_currency(time_series$office_spend)
time_series$staffing_spend <- clean_currency(time_series$staffing_spend)
time_series$subtotal_of_office_running_costs <- clean_currency(time_series$subtotal_of_office_running_costs)
time_series$accommodation_spend <- clean_currency(time_series$accommodation_spend)
time_series$travel_and_subsistence_spend <- clean_currency(time_series$travel_and_subsistence_spend)
time_series$subtotal_of_other_parliamentary_costs <- clean_currency(time_series$subtotal_of_other_parliamentary_costs)
time_series$travel_and_subsistence_uncapped <- clean_currency(time_series$travel_and_subsistence_uncapped)
# Further tidying of the data
time_series <- time_series |>
  rename(mp_name = x_mp_s_name)

# Sense check number of MPs
time_series |>
  group_by(start_year) |>
  summarise(n_mp = length(unique(x_mp_s_name))) |>
  mutate(election_year = if_else(start_year %in% c(2015, 2017, 2019, 2024), "Yes", "No")) |>
  ggplot(aes(x = start_year, y = n_mp, fill = election_year)) +
  geom_col() +
  geom_hline(yintercept = 650) +
  labs(x = "Year start",
       y = "",
       title = "Number of MP's in records for each FY year",
       subtitle = "Horizontal line at 650 for number of seats in Commons",
       fill = "Election year") +
  theme_minimal()

# Attach spatial data
map <- read_sf(here("data", "Westminster_Parliamentary_Constituencies_July_2024_Boundaries_UK_BFC_-1420043245393085943.gpkg")) |>
  select(constituency = PCON24NM) |>
  mutate(constituency = str_to_upper(constituency)) # fix to upper case so it all matches

# This map is the redraw for the 2024 election and is only valid for year start 2024 (most recent data)
# In the most recent data, constituency is not populated.
# Previous constituency is marked for name before redraw
# New constituency name is also marked where the MP moved constituency

time_series_24 <- time_series |>
  mutate(constituency = constituency_since_5_july_2024) |>
  filter(start_year > 2023) |> # Only final year of data
  filter(constituency != "N/A") |> # Remove NA (lost seat after redraw of map)
  mutate(constituency = str_to_upper(constituency)) |> # fix to upper case so it all matches
  mutate(constituency = str_remove(constituency, " BC")) |> # remove suffix
  mutate(constituency = str_remove(constituency, " CC")) |> # remove suffix
  mutate(constituency = if_else(
    constituency == "MONTGOMERYSHIRE AND GLYNDŴR",
    "MONTGOMERYSHIRE AND GLYNDWR",
    constituency)
    ) # Remove special characters to allow match

# Get unique values from each to test 
map_cons <- unique(map$constituency) |> sort()
data_cons <- unique(time_series_24$constituency) |> sort()

length(data_cons[data_cons %in% map_cons])
message("All constituency names now match!")

# Join to map
time_series_24_map <- left_join(map, time_series_24)

# Test visually with travel spend
time_series_24_map |>
  st_simplify(dTolerance = 1000) |>
  ggplot(aes(fill = travel_and_subsistence_uncapped)) +
  geom_sf(colour = NA) + 
  scale_fill_viridis_c() +
  labs(title = "Constituency map of travel spend",
         subtitle = "Post 2024 election with redrawn election map",
         fill = "Total travel and subsistence spend (£)") +
  theme_minimal()

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
    # Rename
    rename(office_budget = any_of(c("office_maximum_budget_available", "office_budget")),
           staffing_budget = any_of(c("staffing_maximum_budget_available", "staffing_budget")),
           wind_up_budget = any_of(c("wind_up_maximum_budget_available", "winding_up_budget")),
           wind_up_spend = any_of(c("winding_up_spend", "wind_up_spend")),
           accommodation_budget = any_of(c("accommodation_maximum_budget_available", "accommodation_budget")),
           travel_and_subsistence_budget = any_of(c("travel_and_subsistence_maximum_budget_available", "travel_and_subsistence_uncapped")),
           other_costs_budget = any_of(c("other_costs_maximum_budget_available", "other_costs_uncapped")),
           remaining_wind_up_budget  = any_of(c("remaining_wind_up_budget", "remaining_winding_up_budget")),
           
           ) |>
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

# Remove the test column 
time_series <- time_series |> select(-office_spend_clean)

# Apply function accross the data
monetary_cols <- c(
  "office_budget",
  "office_spend",
  "remaining_office_budget",
  "staffing_budget",
  "staffing_spend",
  "remaining_staffing_budget",
  "wind_up_budget",
  "wind_up_spend",
  "remaining_wind_up_budget",
  "subtotal_of_office_running_costs",
  "accommodation_budget",
  "accommodation_spend",
  "remaining_accommodation_budget",
  "travel_and_subsistence_budget",
  "travel_and_subsistence_spend",
  "other_costs_budget",
  "other_costs_spend",
  "subtotal_of_other_parliamentary_costs",
  "overall_total_spend_for_this_financial_year",
  "start_up_maximum_budget_available",
  "start_up_spend",
  "remaining_start_up_budget"
)

time_series[monetary_cols] <- lapply(time_series[monetary_cols], clean_currency)

# Further tidying of the data
time_series <- time_series |>
  rename(mp_name = x_mp_s_name)

# Sense check number of MPs
time_series |>
  group_by(start_year) |>
  summarise(n_mp = length(unique(mp_name))) |>
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

expenses_24_25 <- time_series |>
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
data_cons <- unique(expenses_24_25$constituency) |> sort()

length(data_cons[data_cons %in% map_cons])
message("All constituency names now match!")

# Join to map
expenses_24_25_map <- left_join(map, expenses_24_25)

# Test visually with travel spend
expenses_24_25_map |>
  st_simplify(dTolerance = 1000) |>
  ggplot(aes(fill = travel_and_subsistence_budget)) +
  geom_sf(colour = NA) + 
  scale_fill_viridis_c() +
  labs(title = "Constituency map of travel spend",
         subtitle = "Post 2024 election with redrawn election map",
         fill = "Total travel and subsistence spend (£)") +
  theme_minimal()

# Save some data output
saveRDS(expenses_24_25_map, here("outputs", "expenses_24_25_map.RDS"))
saveRDS(time_series, here("outputs", "time_series.RDS"))
saveRDS(data_list, here("outputs", "data_list.RDS"))

#-----------------------------------------------------------------------------------------------------------------------

# Title:            Clean and Compare Raw Data
# Description:      Clean the raw tag data from both sources (old csvs and freshly tagged from WildTrax), and compare the
#                   results to evaluate how many tags Mateen changed.

# Authors:          Marcus Becker, Mateen Hessami
# Date:             September 2022

# Previous Scripts: None

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(wildRtrax) # For downloading updated data from WildTrax
library(keyring)   # For storing credentials securely

library(readr)
library(purrr)
library(dplyr)
library(fs)
library(stringr)
library(tidyr)
library(lubridate)

# Root directory (Shared Google Drive)
root <- "G:/Shared drives/ABMI Camera Mammals/data/"

#-----------------------------------------------------------------------------------------------------------------------

# Obtain data

# (1) Older data - moose tags have not been updated by Mateen.

# Projects tagged so far (2015 and 2016)
projects <- paste0("Ecosystem_Health_", rep(2015:2016))

# File paths to raw data csv
file_paths <- dir_ls(paste0(root, "base/raw/from_WildTrax/ABMI/")) |>
  as_tibble() |>
  filter(str_detect(value, paste(projects, collapse = "|")))

# Import
df_old <- map_df(file_paths, .f = read_csv) |>
  # Just Moose images at Ecosystem Health sites (no Off-Grid)
  filter(common_name == "Moose",
         !str_detect(location, "OG"))

# (2) Newer data - Moose tags have been updated by Mateen

# Authenticate into WildTrax
Sys.setenv(WT_USERNAME = key_get("WT_USERNAME", keyring = "wildtrax"),
           WT_PASSWORD = key_get("WT_PASSWORD", keyring = "wildtrax"))

wt_auth()

# Obtain project IDs
projects <- map_chr(projects, .f = ~ str_replace_all(., "_", " "))

proj_ids <- wt_get_download_summary(sensor_id = "CAM") |>
  filter(str_detect(project, paste(projects, collapse = "|"))) |>
  select(project_id) |>
  pull() |>
  unlist()

# Download
df_new <- map_df(.x = proj_ids,
                 .f = ~ wt_download_report(
                   project_id = .x,
                   sensor_id = "CAM",
                   cols_def = FALSE,
                   weather_cols = FALSE))

# All deployments in df_new - will need for later.
all_dep <- df_new |> select(project, location) |> distinct()

# Filter for just Moose tags
df_new <- df_new |> filter(common_name == "Moose")

#-----------------------------------------------------------------------------------------------------------------------

# Amalgamate tags of same species in the same image
df_old_am <- df_old |>
  mutate(number_individuals = as.numeric(number_individuals)) |>
  # Expand age and sex variables by the number of individuals (separated by ", ")
  mutate(age_class = trimws(strrep(str_c(age_class, ", "), number_individuals), whitespace = ", "),
         sex = trimws(strrep(str_c(sex, ", "), number_individuals), whitespace = ", ")) |>
  group_by(project, location, date_detected, common_name) |>
  # Combine all tags of one image into a single row
  mutate(number_individuals = sum(number_individuals),
         age_class = paste0(age_class, collapse = ", "),
         sex = paste0(sex, collapse = ", ")) |>
  distinct(project, location, date_detected, common_name, number_individuals, .keep_all = TRUE) |>
  ungroup() |>
  # Add marker variable denoting this as the older data
  mutate(data = "old")

# Same thing for the updated data
df_new_am <- df_new |>
  mutate(number_individuals = as.numeric(number_individuals)) |>
  mutate(age_class = trimws(strrep(str_c(age_class, ", "), number_individuals), whitespace = ", "),
         sex = trimws(strrep(str_c(sex, ", "), number_individuals), whitespace = ", ")) |>
  group_by(project, location, date_detected, common_name) |>
  mutate(number_individuals = sum(number_individuals),
         age_class = paste0(age_class, collapse = ", "),
         sex = paste0(sex, collapse = ", ")) |>
  distinct(project, location, date_detected, common_name, number_individuals, .keep_all = TRUE) |>
  ungroup() |>
  mutate(data = "new")

# Save new data
write_csv(df_new_am, "./data/clean/abmi-eh151617-moose_clean.csv")

# Save deployment location names
write_csv(all_dep, "./data/lookup/abmi-eh151617_deployments.csv")

#-----------------------------------------------------------------------------------------------------------------------

# Compare the old data to the new:

df_compare <- df_new_am |>
  mutate(date_detected = ymd_hms(date_detected)) |>
  bind_rows(df_old_am) |>
  separate_rows(age_class, sex, sep = ", ") |>
  group_by(data, project, common_name, age_class, sex) |>
  tally() |>
  pivot_wider(id_cols = c(project, common_name, age_class, sex), names_from = data, values_from = n) |>
  mutate(difference = new - old,
         difference_pct = round(difference / old, digits = 4) * 100) |>
  select(1:4, old, new, 7, 8)

#-----------------------------------------------------------------------------------------------------------------------


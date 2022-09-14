#-----------------------------------------------------------------------------------------------------------------------

# Title:            Calculate Density of Moose
# Description:      Calculate density of various moose demographic categories using the time in front of camera method.

# Authors:          Marcus Becker, Mateen Hessami
# Date:             September 2022

# Previous Scripts: 01_clean-compare-raw

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# Root directory (Shared Google Drive)
root <- "G:/Shared drives/ABMI Camera Mammals/"

# Import data and lookup tables/values:

# Moose tags
df_data <- read_csv("./data/clean/abmi-eh1516-moose_clean.csv") |>
  # Only want images within range of field of view
  filter(field_of_view == "WITHIN") |>
  # Only care about certain columns
  select(project, location, date_detected, common_name, age_class, sex, number_individuals)

# Deployments in the 2015-2016 projects
dep <- read_csv("./data/lookup/abmi-eh1516_deployments.csv") |>
  unite(location, project, col = "location_project", sep = "_", remove = TRUE)

# Gap classes (processed elsewhere)
df_gap <- read_csv(paste0(root, "data/processed/probabilistic-gaps/abmi-cmu_all-years_gap-class-raw_2021-10-07.csv")) |>
  filter(common_name == "Moose",
         str_detect(project, "Health 2015|Health 2016")) |>
  # Combine when multiple gap classes for a single image
  group_by(location, date_detected, common_name) |>
  mutate(gap_class = paste0(gap_class, collapse = ", ")) |>
  distinct(location, date_detected, common_name, .keep_all = TRUE) |>
  ungroup()

# Probabilistic gaps
df_leave_prob_pred <- read_csv(paste0(root, "data/processed/probabilistic-gaps/gap-leave-prob_predictions_2021-10-05.csv")) |>
  filter(gap_group == "Moose") |>
  rename(common_name = gap_group)

# Average time between photos (processed elsewhere)
tbp_moose <- read_csv(paste0(root, "data/processed/time-btwn-images/abmi-all-years_tbp_2021-06-21.csv")) |>
  filter(common_name == "Moose") |>
  select(tbp) |>
  pull()

# Number of operating days by deployment and season (tbd = time by day)
df_tbd <- read_csv(paste0(root, "data/processed/time-by-day/abmi-cmu_all-years_tbd-summary_2021-10-07.csv")) |>
  filter(str_detect(project, "Health 2015|Health 2016")) |>
  unite(location, project, col = "location_project", sep = "_", remove = TRUE)

# Seasonal start/end dates (julian day):
summer.start.j <- 106 # April 16
summer.end.j <- 288 # October 15

#-----------------------------------------------------------------------------------------------------------------------

# Step 1. Define independent events (series) by age/sex category

# Note: This is a little wonky since most juvenile detections occur at the same time as adult females.

df_events <- df_data |>
  # Join gap class
  left_join(df_gap, by = c("location", "project", "date_detected", "common_name")) |>
  # Spread rows
  separate_rows(age_class, sex, sep = ", ") |>
  mutate(number_individuals = 1) |>
  group_by(project, location, date_detected, common_name, age_class, sex) |>
  mutate(number_individuals = sum(number_individuals)) |>
  distinct(project, location, date_detected, common_name, number_individuals, .keep_all = TRUE) |>
  ungroup() |>
  # Remove Unkn's from age_class; We'll keep sex Unkn's for now (i.e., Adult-Unkn's will stay) ...
  filter(!str_detect(age_class, "Unkn")) |>
  # Create field field with demographic categories
  mutate(demo_category = paste0(common_name, " ", age_class, " ", sex)) |>
  # Delineate series, with 4 categories of moose (Juv, Adult-Male, Adult-Female, Adult-Unkn)
  arrange(project, location, demo_category, date_detected) |>
  mutate(series_num = 0,
         date_detected_lag = lag(date_detected),
         date_detected_lead = lead(date_detected),
         diff_time_lag = as.numeric(date_detected - date_detected_lag),
         diff_time_lead = as.numeric(abs(date_detected - date_detected_lead)),
         location_lag = lag(location),
         diff_location = ifelse(location != location_lag, TRUE, FALSE),
         demo_category_lag = lag(demo_category),
         diff_dc = ifelse(demo_category != demo_category_lag, TRUE, FALSE),
         gap_check = ifelse(diff_location == FALSE & diff_dc == FALSE & (diff_time_lag <= 120 & diff_time_lag >= 20), 1, 0),
         gap_class_previous = replace_na(lag(gap_class), ""),
         diff_series = ifelse(diff_location == TRUE | diff_dc == TRUE | diff_time_lag > 120 | str_detect(gap_class_previous, "N|L"), 1, 0),
         series_num = c(0, cumsum(diff_series[-1])),
         gap_prob = replace_na(ifelse(gap_check == 1 & (gap_class_previous == "" | str_detect(gap_class_previous, "U")), 1, 0), 0)) |>
  group_by(series_num) |>
  mutate(diff_time_lag = ifelse(row_number() == 1, 0, diff_time_lag),
         diff_time_lead = ifelse(row_number() == n(), 0, diff_time_lead)) |>
  ungroup() |>
  # Join gap leaving predictions
  left_join(df_leave_prob_pred, by = c("common_name", "diff_time_lag" = "diff_time")) |>
  # Adjust time difference between ordered images that require probabilistic time assignment
  mutate(pred = replace_na(pred, 1),
         diff_time_lag_adj = ifelse(gap_prob == 1, diff_time_lag * (1 - pred), diff_time_lag),
         diff_time_lead_adj = ifelse(lead(gap_prob == 1), diff_time_lead * (1 - lead(pred)), diff_time_lead))

# Calculate total time in front of the camera, by series (tts = Total Time by Series)
df_tts <- df_events |>
  group_by(series_num) |>
  mutate(# Check whether the image was first or last in a series
    bookend = ifelse(row_number() == 1 | row_number() == n(), 1, 0),
    # Calculate time for each individual image
    image_time = ifelse(bookend == 1,
                        ((diff_time_lag_adj + diff_time_lead_adj) / 2) + (tbp_moose / 2),
                        (diff_time_lag_adj + diff_time_lead_adj) / 2),
    # Multiply image time by the number of animals present
    image_time_ni = image_time * number_individuals) |>
  # Group by common name as well to add it as a variable to output
  group_by(demo_category, .add = TRUE) |>
  # Calculate total time and number of images for each series
  summarise(n_images = n(),
            series_total_time = sum(image_time_ni)) |>
  ungroup()

# Calculate total time in front of camera, by deployment, project, and species (tt = total time)

df_tt <- df_events |>
  group_by(series_num) |>
  arrange(date_detected, .by_group = TRUE) |>
  filter(row_number() == 1) |>
  left_join(df_tts, by = c("series_num", "demo_category")) |>
  select(project, location, date_detected, demo_category, series_num, series_total_time) |>
  ungroup() |>
  mutate(julian = as.numeric(format(date_detected, "%j")),
         season = ifelse(julian >= summer.start.j & julian <= summer.end.j, "summer", "winter")) |>
  unite(location, project, col = "location_project", sep = "_", remove = TRUE) |>
  mutate_at(c("location_project", "demo_category", "season"), factor) |>
  group_by(location_project, demo_category, season, .drop = FALSE) |>
  summarise(total_duration = sum(series_total_time)) |>
  ungroup() |>
  mutate_if(is.factor, as.character)

# Demographic categories
cat <- as.character(sort(unique(df_tt$demo_category)))

# For deployments in 2015/2016 without any Moose events
df_tt_nm <- dep |>
  anti_join(df_tt, by = c("location_project")) |>
  expand(location_project, season = c("winter", "summer"), demo_category = cat) |>
  mutate(total_duration = 0)

# Join together - all combinations of moose events, deployments, and season.
df_tt_full <- bind_rows(df_tt, df_tt_nm) |>
  arrange(location_project, demo_category, season) |>
  left_join(df_tbd, by = "location_project") |>
  mutate(total_season_days = ifelse(season == "summer", total_summer_days, total_winter_days)) |>
  select(location_project:season, total_season_days, total_duration)

#-----------------------------------------------------------------------------------------------------------------------

# Calculate density

# VegHF categories lookup:
df_veghf <- read_csv(paste0(root, "data/lookup/veghf/abmi-cmu_all-years_veghf-soilhf-detdistveg_2021-10-06.csv")) |>
  filter(str_detect(project, "Health 2015|Health 2016")) |>
  select(project, location, VegForDetectionDistance) |>
  distinct() |>
  unite(location, project, col = "location_project", sep = "_", remove = TRUE)

# Effective detection distance (EDD) predictions lookup (processed elsewhere)
df_edd <- read_csv(paste0(root, "data/processed/detection-distance/predictions/edd_veghf_season.csv"))

# Camera field of view angle
cam_fov_angle <- 40

# Density of each Moose category at each deployment
df_dep_density <- df_tt_full |>
  mutate(dist_group = "BigForestUngulates") |>
  # Join detection distance vegetation categories for each deployment
  left_join(df_veghf, by = "location_project") |>
  # Join EDD predictions
  left_join(df_edd, by = c("dist_group", "season", "VegForDetectionDistance")) |>
  # Remove deployments with missing detdist values <- something to check on.
  filter(!is.na(detdist)) |>
  # Calculate density
  mutate(effort = total_season_days * (detdist ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = total_duration / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000) |>
  # All the NaNs are just combinations where the total_seasons_days is 0. Remove NAs.
  filter(!is.na(cpue_km2)) |>
  select(location_project, demo_category, season, total_season_days, total_duration, density_km2 = cpue_km2)

# Write results
write_csv(df_dep_density, "./data/processed/abmi_eh1516_moose_density-deployment.csv")

# Calculate density at each 'site' - i.e., the average of the four density values from each quadrant (NW, NE, SW, SE)

# Minimum seasonal days requirement for inclusion
min_season_days <- 20
# Minimum total days requirement for inclusion
min_total_days <- 40

df_site_density <- df_dep_density |>
  # Total days condition
  filter(total_season_days >= min_season_days) |>
  group_by(location_project, demo_category) |>
  # Seasonal days condition
  mutate(total_days = sum(total_season_days)) |>
  filter(total_days >= min_total_days) |>
  # Calculate combined season estimate
  summarise(full_density_km2 = mean(density_km2)) |>
  ungroup() |>
  separate(location_project, into = c("site", "quadrant", "project"), sep = "-|_") |>
  group_by(site, project, demo_category) |>
  # Calculate combined site estimate, including number of cameras (mostly 4)
  summarise(full_density_km2 = mean(full_density_km2),
            n_deployments = n())

# Write results
write_csv(df_site_density, "./data/processed/abmi_eh1516_moose_density-site.csv")

#-----------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------

# Exploratory data analysis on moose recruitment and demographics
# January 2022
# M.Becker & M.Hessami

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(dplyr)
library(tidyr)
library(readr)
library(fs)
library(stringr)
library(purrr)
library(ggplot2)

# Root directory w/ data
root <- "G:/Shared drives/ABMI Camera Mammals/data/"

# File paths to raw ABMI, CMU, NWSAR, and BG data.
file_paths <- dir_ls(paste0(root, "base/clean/")) %>%
  as_tibble() %>%
  filter(str_detect(value, "native-sp")) %>%
  pull(value)

# Load data, immediately filter for just moose among other criteria:
df <- map_df(file_paths, .f = read_csv) %>%
  filter(common_name == "Moose",
         field_of_view == "WITHIN",
         str_detect(project, "Ecosystem|CMU|Focal"),
         !str_detect(location, "HF2|RIVR|CUDDE|CITSCI|T$"))

# Date cutoffs
start <- 135 # May 15
end <- 274 # October 1

# Lookup data

# Time between image value for Moose
tbp <- read_csv(paste0(root, "processed/time-btwn-images/abmi-cmu_all-years_tbp_2021-06-25.csv")) %>%
  filter(common_name == "Moose") %>%
  select(tbp) %>%
  pull()

# Probabilistic gaps
df_leave_prob_pred <- read_csv(paste0(root, "processed/probabilistic-gaps/gap-leave-prob_predictions_2021-10-05.csv")) %>%
  filter(gap_group == "Moose") %>%
  select(diff_time, pred)

# Time by day
time.by.day <- read_csv(paste0(root, "processed/time-by-day/intermediate/time.by.day.csv"))

# Vector of all deployments (including those that didn't record a moose.)
dep <- time.by.day %>%
  select(project_location) %>%
  filter(str_detect(project_location, "Ecosystem|CMU|Focal")) %>%
  filter(!str_detect(project_location, "HF2|RIVR|CUDDE|CITSCI|T$")) %>%
  pull()

#-----------------------------------------------------------------------------------------------------------------------

# OK, so there is a slight issue with how the data was summarized before.
# In a small number of cases, when there is a group of animals, a tag was lost in age_class or sex.
# So we'll make an assumption that the lost tag is a Juvenile when another juvenile is present (and the adult is a Female)

df_issue <- df %>%
  filter(number_individuals > 2) %>%
  mutate(age_categories = str_count(age_class, ",") + 1) %>%
  filter(!age_categories == "1",
         !number_individuals == age_categories) %>%
  mutate(age_class = ifelse(str_detect(age_class, "Juv"), paste0(age_class, ", Juv"), paste0(age_class, ", Adult")),
         sex = paste0(sex, ", Unkn")) %>%
  select(-age_categories)

#-----------------------------------------------------------------------------------------------------------------------

# OK, what we need to do is tackle this by series.
# Basically there are 3 categories that we are interested in:
#   - Juveniles
#   - Adult Females
#   - Adult Males
#   - Adult Unkns

# Let's look at series.
df_series <- df %>%
  # Remove 'problem' data points and then add them back in with the fix.
  anti_join(df_issue, by = c("project", "location", "date_detected")) %>%
  bind_rows(df_issue) %>%
  # Need to filter out series from certain times of the year - tags aren't reliable (per Corrina)
  mutate(julian = as.numeric(format(date_detected, "%j")),
         include = ifelse(julian >= start & julian <= end, "yes", "no")) %>%
  filter(include == "yes") %>%
  # Re-do age_class, sex categories.
  mutate(number_individuals = ifelse(str_detect(age_class, ","), 1, number_individuals)) %>%
  separate_rows(age_class, sex, sep = ", ") %>%
  # Remove 'Juv - Female' tags - change to Juv Unkn so can combine with other Juv
  mutate(sex = ifelse(age_class == "Juv", "Unkn", sex)) %>%
  # Remove Unkn-Unkn (453) and Unkn-Female (1); We'll keep Adult-Unkn for now.
  filter(!age_class == "Unkn") %>%
  mutate(common_name = paste0(common_name, "-", age_class, "-", sex)) %>%
  # Delineate series, with 4 categories of moose (Juv, Adult-Male, Adult-Female, Adult-Unkn)
  arrange(project, location, common_name, date_detected) %>%
  mutate(series_num = 0,
         date_detected_lag = lag(date_detected),
         diff_time = as.numeric(date_detected - date_detected_lag),
         location_lag = lag(location),
         diff_location = ifelse(location != location_lag, TRUE, FALSE),
         common_name_lag = lag(common_name),
         diff_sp = ifelse(common_name != common_name_lag, TRUE, FALSE),
         diff_series = ifelse(diff_location == TRUE | diff_sp == TRUE | diff_time > 120, 1, 0),
         series_num = c(0, cumsum(diff_series[-1]))) %>%
  select(1, 3, 12, 4, 6, 9, 14) %>%
  # Calculate mean number of individuals per series
  group_by(series_num) %>%
  mutate(avg_individuals = mean(number_individuals)) %>%
  ungroup() %>%
  # Join gap leaving probabilities
  left_join(df_leave_prob_pred, by = "diff_time") %>%
  mutate(pred = replace_na(pred, 1),
         diff_time_adj = ifelse(pred == 1, diff_time, diff_time * (1 - pred)))

# Calculate total time in front of camera, by series
df_tts_multiple <- df_series %>%
  # Remove first image from dataframe (as to not include diff_time in time totals for the series)
  mutate(series_num_previous = lag(series_num)) %>%
  filter(series_num_previous == series_num) %>%
  group_by(series_num) %>%
  summarise(n_images = n(),
            total_time = sum(diff_time_adj))

df_tts_single <- df_series %>%
  group_by(series_num) %>%
  summarise(n_images = n()) %>%
  # Keep only series with 1 image
  filter(n_images == 1) %>%
  mutate(total_time = 0)

df_tts_all <- df_tts_multiple %>%
  bind_rows(df_tts_single) %>%
  arrange(series_num)

df_tts_final <- df_series %>%
  select(series_num, common_name, avg_individuals) %>%
  distinct() %>%
  left_join(df_tts_all, by = "series_num") %>%
  mutate(series_total_time = (total_time + tbp) * avg_individuals) %>%
  select(-c(total_time))

# Time by day summary for our specified time period.
df_tbd_summary <- time.by.day %>%
  mutate(total_days = rowSums(select(., -project_location)),
         total_demographic_days = rowSums(select(., -c(project_location:135, 274:366)))) %>%
  select(project_location, total_days, total_demographic_days)

df_tt <- df_series %>%
  group_by(series_num) %>%
  arrange(date_detected, .by_group = TRUE) %>%
  filter(row_number() == 1) %>%
  left_join(df_tts_final, by = c("series_num", "common_name")) %>%
  select(project, location, date_detected, common_name, series_num, series_total_time) %>%
  ungroup() %>%
  unite(project, location, col = "project_location", sep = "_", remove = TRUE) %>%
  mutate_at(c("project_location", "common_name"), factor) %>%
  group_by(project_location, common_name, .drop = FALSE) %>%
  summarise(total_duration = sum(series_total_time)) %>%
  ungroup() %>%
  mutate_if(is.factor, as.character) %>%
  left_join(df_tbd_summary, by = "project_location")

# 'Species' of interest - i.e. all the moose categories that we've made.
sp <- c("Moose-Adult-Female", "Moose-Adult-Male", "Moose-Adult-Unkn", "Moose-Juv-Unkn")

# For deployment with no moose
df_tt_nm <- df_tbd_summary %>%
  # Filter out non-ABMI deployments in df_tbd
  filter(project_location %in% dep) %>%
  # Retrieve only those that had no images of native species
  anti_join(df_tt, by = "project_location") %>%
  expand(project_location, common_name = sp) %>%
  # Re-join time-by-day information
  left_join(df_tbd_summary, by = "project_location") %>%
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0)

# Bind together
df_tt_full <- df_tt %>%
  bind_rows(df_tt_nm) %>%
  arrange(project_location, common_name)

# Now, let's calculate density.

# Load effective detection distance (EDD) modeling:
load(paste0(root,"processed/detection-distance/predictions/Detection distances by site species and season_2021-10-07.rdata"))

# Camera field of view angle
cam_fov_angle <- 40

# Join detection distance to total time
df_dens_ing <- dd %>%
  as.data.frame() %>%
  rownames_to_column(var = "location_project") %>%
  gather(key = "SpGroupSeason", value = "detdist", WTDeer.summer:`Pronghorn.winter`) %>%
  mutate(SpGroupSeason = str_replace_all(SpGroupSeason, "[//(//)// ]", "")) %>%
  # Create two new columns: Detection Distance Group and Season, sep by "."
  separate(SpGroupSeason, into = c("dist_group", "season")) %>%
  mutate(dist_group = str_replace(dist_group, "wapiti", ""),
         season = tolower(season)) %>%
  filter(dist_group == "BigForestUngulates",
         season == "summer") %>%
  separate(location_project, into = c("location", "project"), sep = "_") %>%
  mutate(project_location = paste0(project, "_", location)) %>%
  select(project_location, detdist) %>%
  right_join(df_tt_full, by = "project_location")

# Calculate density
df_density <- df_dens_ing %>%
  mutate(effort = total_demographic_days * (detdist ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = total_duration / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000) %>%
  select(project_location, common_name, total_duration, total_demographic_days, density_km2 = cpue_km2) %>%
  separate(project_location, into = c("project", "location"), sep = "_")

#-----------------------------------------------------------------------------------------------------------------------

# Write results.

write_csv(df_density, "./data/processed/moose_demographic_density.csv")

#-----------------------------------------------------------------------------------------------------------------------




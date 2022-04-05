#-----------------------------------------------------------------------------------------------------------------------

# Title: Calculate moose cow/bull ratios by WMU
# Date: March 2022
# Author: Marcus Becker

#-----------------------------------------------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)

root <- "G:/Shared drives/ABMI Camera Mammals/"

# Load functions
source(paste0(root, "src/R/summarise-density_2021-06-23.R"))

# ABMI camera sites (a mix of 'true' and public buffered sites)
sf_abmi <- st_read("C:/Users/mabec/Documents/R/abmi-shiny-app/data/processed/abmi_cam_locations.shp")
# ABMI Off-Grid
sf_abmi_og <- st_read(paste0(root, "data/spatial/og.shp"))
# CMU, NWSAR, BG
sf_cmu <- st_read(paste0(root, "data/lookup/locations/spatial/cmu_2017-2020.shp"))

df_density <- read_csv("./data/processed/moose_demographic_density.csv")

# WMUs in the Oilsands Region:
ab_wmu <- st_read("./Data/SpatialData/AB_WMU-OSR-Intersect.shp", stringsAsFactors = FALSE, quiet = TRUE) %>%
  separate(WMUNIT_COD, into = c("zeroes", "WMU"), sep = 2, remove = FALSE) %>%
  select(-c(zeroes, Shape_STAr, Shape_STLe, WMUNIT_COD, OBJECTID)) %>%
  st_transform(4326)

# OK, first we need to sort out the OG data. We want the northern focal areas stuff.
df_focal <- df_density %>%
  filter(str_detect(project, "Northern Focal")) %>%
  select(location, project) %>%
  distinct()

sf_focal <- sf_abmi_og %>%
  filter(str_detect(Site_ID, "^OG-ABMI-701-|^OG-ABMI-638-")) %>%
  select(Site_ID, Site) %>%
  mutate(location = str_remove(Site, "-CAM$|-BOTH$")) %>%
  left_join(df_focal, by = "location") %>%
  filter(!is.na(project)) %>%
  select(location, project) %>%
  distinct()

# Spatial Join
sf_all <- sf_abmi %>%
  select(-actual_loc) %>%
  bind_rows(sf_cmu, sf_focal) %>%
  st_transform(4326)

# Join with OSR WMUs ...
sf_all_wmu <- st_join(sf_all, ab_wmu, left = FALSE) %>%
  # Remove trail deployments
  filter(!str_detect(location, "T$")) %>%
  st_set_geometry(NULL)

df_dens_some <- df_density %>%
  filter(!str_detect(project, "Adopt|Southern|Amphibian"),
         !str_detect(location, "RIVR|CL$")) %>%
  # Filter join for just the cameras in WMUs that we're interested in.
  inner_join(sf_all_wmu, by = c("project", "location")) %>%
  filter(total_demographic_days >= 20,
         !is.na(density_km2))

# Now we summarise.
summarised_wmu <- df_dens_some %>%
  summarise_density(group_id = WMUNIT_NAM,
                    agg_samp_per = TRUE,
                    species_col = common_name,
                    dens_col = density_km2,
                    conflevel = 0.9)

# Now we calculate ratios.

# Let's start with bull:cow.

ratio_bc <- summarised_wmu %>%
  filter(common_name == "Moose-Adult-Male" | common_name == "Moose-Adult-Female") %>%
  select(WMUNIT_NAM, common_name, density_avg) %>%
  pivot_wider(WMUNIT_NAM, names_from = "common_name", values_from = "density_avg") %>%
  mutate(ratio_bulls_cows = `Moose-Adult-Male` / `Moose-Adult-Female`)











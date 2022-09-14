#-----------------------------------------------------------------------------------------------------------------------

# Title:            Compute moose demographic ratios for WMUS
# Description:      Compute various moose demographic ratios (bull:cow, calf:cow) for Wildlife Management Units (WMUs)

# Authors:          Marcus Becker, Mateen Hessami
# Date:             September 2022

# Previous Scripts: 01_clean-compare-raw, 02_calculate-density

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages
library(readr)
library(dplyr)
library(sf)
library(stringr)
library(tidyr)

root <- "G:/Shared drives/ABMI Camera Mammals/"

# Load data:

# Wildlife Management Units (WMU)
sf_wmus <- st_read(paste0(root, "data/spatial/ab_wmus_all.shp"))

# Aerial survey data
df_aerial <- read_csv("./data/raw/aerial-surveys_osr_moose-ratios.csv")

# ^ Note: We need more aerial survey data from WMUs in different parts of the province.

# Moose densities at the deployment level (processed in the previous script)
df_dep_density <- read_csv("./data/processed/abmi_eh1516_moose_density-deployment.csv")

#-----------------------------------------------------------------------------------------------------------------------

# 1. Perform spatial join of camera locations to WMU polygons
# 2. Calculate ratios using the camera density results; compare (plot, map) to aerial survey results.

#-----------------------------------------------------------------------------------------------------------------------

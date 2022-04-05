#-----------------------------------------------------------------------------------------------------------------------

# Title: Visualize
# Date: March 2022
# Author: Marcus Becker

#-----------------------------------------------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(tmap)
library(Hmisc)

# Root
root <- "G:/Shared drives/ABMI Camera Mammals/"

# Spatial data
ab_wmu <- st_read("./Data/SpatialData/AB_WMU-OSR-Intersect.shp", stringsAsFactors = FALSE, quiet = TRUE) %>%
  separate(WMUNIT_COD, into = c("zeroes", "WMU"), sep = 2, remove = FALSE) %>%
  select(-c(zeroes, Shape_STAr, Shape_STLe))

# Data from Government reports
df_all <- read_csv("./Data/Raw/Metrics.csv")
# Density data from more recent Government reports
df_gov_recent <- read_csv("./Data/Raw/OSM_Ungulate_Survey_Data_2013-2018_Density.csv")

# Prepare df of demographic data
df_demo <- df_all %>%
  filter(Response == "CalfCow" | Response == "MaleFemale",
         Species == "Moose",
         !is.na(WMU)) %>%
  separate(MetricText, into = c("numerator", "denominator"), sep = ":") %>%
  mutate(numerator = as.numeric(numerator),
         denominator = as.numeric(denominator),
         ratio = numerator / denominator) %>%
  mutate(ratio = if_else(is.na(ratio), MetricNum, ratio)) %>%
  select(WMU, StartYear, Response, ratio) %>%
  filter(!WMU == "518;519;529;530;531" & !WMU == "506;510" &
           !WMU == "518;530;531" & !WMU == "530 North" & !WMU == "530 South") %>%
  mutate(WMU = as.numeric(if_else(WMU =="530 Full", "530", WMU)),
         Response = str_replace_all(Response, "MaleFemale", "Bull:Cow"),
         Response = str_replace_all(Response, "CalfCow", "Calf:Cow"))

df_demo2 <- df_demo %>%
  # Important to group by both WMU and reponse in order to retain two
  # observations for each WMU (CalfCow & MaleFemale)
  group_by(WMU, Response) %>%
  slice(which.max(StartYear))

# Prepare colors (Blues)
c1 <- c('#f7fbff','#deebf7','#c6dbef',
        '#9ecae1','#6baed6','#4292c6',
        '#2171b5','#08519c','#08306b')
c2 <- colorRampPalette(c1)
legend_title_demo1 <- "Calves/Bulls per Cow"

ab_wmu_demo <- ab_wmu %>%
  mutate(WMU = as.numeric(WMU)) %>%
  # Full join in order to retain both Response types
  full_join(df_demo2, by = "WMU") %>%
  select(WMUNIT_NAM, WMU, StartYear:ratio) %>%
  mutate(bin = as.numeric(cut2(ratio, g = 10)),
         colour = c2(10)[bin]) %>%
  mutate(colour = if_else(is.na(colour), "#999999", colour))

# Map 1 (CalfCow)
tm_map_demo1 <- ab_wmu_demo %>%
  filter(Response == "Calf:Cow" | is.na(Response)) %>%
  tm_shape() +
  tm_fill(col = "ratio",
          palette = c1,
          style = "cont",
          colorNA = "grey60",
          textNA = "Missing",
          title = legend_title_demo1) +
  tm_borders(col = "black") +
  tm_text("WMU", size = 0.65) +
  tm_compass(type = "4star", size = 2,
             position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Calf:Cow", title.size = 1,
            legend.title.size = 1, frame = FALSE)

# Map 2 (BullCow)
tm_map_demo2 <- ab_wmu_demo %>%
  filter(Response == "Bull:Cow" | is.na(Response)) %>%
  tm_shape() +
  tm_fill(col = "colour",
          style = "cont") +
  tm_borders(col = "black") +
  tm_text("WMU", size = 0.65) +
  tm_layout(title = "Bull:Cow", title.size = 1,
            legend.title.size = 1, frame = FALSE)

# Combine two maps
tm_map_demo_both <- tmap_arrange(tm_map_demo1, tm_map_demo2)

tmap_save(tm_map_demo_both, filename = "./demo.png")


tm_map_demo_both


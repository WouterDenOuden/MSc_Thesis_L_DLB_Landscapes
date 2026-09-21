library(readxl)
library(moments)
library(ggplot2)
library(dplyr)
library(writexl)
library(forcats)
library(purrr)
library(factoextra)
library(cluster)
library(tidyr)

# this dataset contains information of all lakes, with the corresponding 10x10 grid cell 
lakes_grids <- read_excel("C:/Users/woute/MSc thesis L-DLB/Data/lakes_per_grid.xlsx")


# filter out the lakes smaller than 14400km2 (split lakes)
lakes_grids_filter <- lakes_grids[lakes_grids$SHAPE_Area >= 14400, ]

# convert lakes to km2
lakes_grids_filter$lake_area_km2 <- lakes_grids_filter$SHAPE_Area / 1000000 # convert lake sizes to km2

# convert lake size to log10
lakes_grids_filter$log_lake_size <- log10(lakes_grids_filter$lake_area_km2)


# load in dataset with empty grid cells
grid_blank <- read_excel("C:/Users/woute/MSc thesis L-DLB/Data/grid_blank.xlsx")

# load in dataset with areal fractions of L-DLB raster
tabulate_allgrids <- read_excel("C:/Users/woute/MSc thesis L-DLB/Data/Tabulat_grids_v2.xlsx")


# calculate variables per grid
grid_stats <- lakes_grids_filter |>
  group_by(FID_fihsnet_districts_v2_ClipLayer) |>
  summarise(
    districts = paste(sort(unique(District_1)), collapse = ", "),
    mean_lake_size = mean(lake_area_km2, na.rm = TRUE),
    median_lake_size = median(lake_area_km2, na.rm = TRUE),
    variance_size = var(lake_area_km2, na.rm = TRUE),
    skewness_size = skewness(lake_area_km2, na.rm = TRUE),
    log_variance_size = var(log_lake_size, na.rm = TRUE),
    log_skewness_size = skewness(log_lake_size, na.rm = TRUE),
    n_lakes = n(),
    mean_circ  = mean(circularity_index, na.rm = TRUE) )

grid_stats$log_median_lake_size <- log10(grid_stats$median_lake_size)


# add areal fraction values to grid
grid_stats <- grid_stats |>
  left_join(
    tabulate_allgrids |>
      select(OBJECTID_1, VALUE_0, VALUE_1, VALUE_10, VALUE_20, VALUE_30),
    by = c("FID_fihsnet_districts_v2_ClipLayer" = "OBJECTID_1"))


# add the area of each grid cell (as some don't make up the full 10x10km)
grid_stats <- grid_stats |>
  left_join(
    grid_blank |>
      select(OBJECTID, SHAPE_Area),
    by = c("FID_fihsnet_districts_v2_ClipLayer" = "OBJECTID"))


### calculate relative areal fractions for each grid

# sum of all the LC types 
grid_stats$VALUE_sum <- grid_stats$VALUE_0 +
  grid_stats$VALUE_1 + 
  grid_stats$VALUE_10 + 
  grid_stats$VALUE_20 +
  grid_stats$VALUE_30

# calculates the relative frequency of lake, DLB, and upland
grid_stats$relative_freq_upland <- grid_stats$VALUE_0 / grid_stats$VALUE_sum
grid_stats$relative_freq_dlb <- grid_stats$VALUE_10 / grid_stats$VALUE_sum
grid_stats$relative_freq_lake <- grid_stats$VALUE_20 / grid_stats$VALUE_sum


# grid area in km2
grid_stats$grid_area_km2 <- grid_stats$SHAPE_Area / 1000000

# convert lake density to lakes per km2
grid_stats$n_lakes_km2 <- grid_stats$n_lakes / grid_stats$grid_area_km2

#  filter grids smaller than 50km2
grid_stats <- grid_stats |>
  filter(grid_area_km2 >= 50)



### district edits

# rename the districts
grid_stats$districts_new <- dplyr::recode(grid_stats$districts,
                                          "Lena / Viljuy"      = "Tyung river",
                                          "North Russia"       = "Nenets",
                                          "West Siberia"       = "Nadym river",
                                          "Yamal"              = "Yamal peninsula",
                                          "Gulf of Ob"         = "Central Yamalia",
                                          "Gydan"              = "Gydan peninsula",
                                          "Anabarsky"          = "North Siberian lowland",
                                          "Tuktut"             = "Northwest Kitikmeot",
                                          "Mackenzie inland"   = "Mackenzie river",
                                          "Northslope"         = "Northslope Alaska",
                                          "Ayon island"        = "Chaunsky district",
                                          "Siberian coast"     = "Yana-Indigirka lowland",
                                          "Seward"             = "Northern Seward peninsula",
                                          "West Tajminsky"     = "Taymir peninsula (west)",
                                          "East Tajminsky"     = "Taymir peninsula (east)",
                                          "Kolyama"            = "Kolyama river",
                                          "Old crow"           = "Old Crow flats")






### grid_stats now contains all grid cells for analysis
###  it stores all landscape metrics per grid

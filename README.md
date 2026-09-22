# MSc_Thesis_L_DLB_Landscapes
R scripts, GIS-Products, and data sheets for my MSc Thesis: Pan-Arctic Analysis of Thermokarst Lake-Drained Lake Basin Landscapes using Hierarchical Clustering

# GIS Data
## GIS input data
input data includes: 
'Occurrence' data derived from Global Surface Water: https://global-surface-water.appspot.com/download
DLB classification data, unpublished. Contact Helena Bergstedt PhD for data accces

## GIS data products: vectorized data
### lakes_14400_smoothed
polygon data with all lakes within districts, derived from the surface water data. lakes smaller than 14400m2 are filtered out
### fihsnet_districts_v2_ClipLayer
grid with 10 x 10km cells. spatial join with cluster_data_sheet# required for visualization. Common ID is: OBJECTID (for fihsnet_districts_v2_ClipLayer), and 
FID_fihsnet_districts_v2_ClipLayer (for cluster_data_sheet#)
### lake_districts_final
boundaries of 28 lake districts
### Circumpolar_Thermokarst_Landscapes
thermokarst lake landscapes, derived from Olefeldt et al. (2016)

## GIS data products: rasterized data
### lake_dlb_raster1
Land cover raster of all districts with classes: lake, DLB, upland, lowland, and river/open water


# R scripts
### data_preparation.R
A few steps to prepare the data for further analysis. Takes lakes_per_grid_xlsx as input data
### boxplots_districts.R
Creates the boxplot visualization between the lake districts. Takes grid_stats (created in data_preparation.R) and lakes_14400_smoothed_TableToExcel.xlsx as input data.
### scatterplot_model_fit.R
Creates scatterplots of districts using statistical moments of lake size distributions. Also calculates model fits. Uses district_means (created in boxplots_districts.R
### hclustering.R
Hierarchical clustering of the grid cells. uses grid_stats (created in data_preparation.R), calculates silhouette values, plots the dendrogram, and returns a data file including cluster classifications per grid: "grid_stats_hc.xlsx"
### pca_six_cluster.R
PCA plot of the six clusters. Takes grid_stats_hc as input (or grid_stats, after it has been edited in hcluster.R)
### heatmap.R
Heatmap and cluster freqeuncies. Takes grid_stats_hc as input (or grid_stats, after it has been edited in hcluster.R)
### elvetaion_etc_boxplots.R
Creates boxplots per cluster for elevation, surface roughness, ground ice and SOC. requires grid_stats_hc as input (or grid_stats, after it has been edited in hcluster.R), grid_soil_data.xlsx, and ground_ice_grids.xlsx.

# Data tables
### lakes_14400_smoothed_TableToExcel.xlsx
All unique lakes within the districts (at least 14.400 m2 in area). 
### ground_ice_grids.xlsx
Ground ice content per grid cell
### grid_soil_data.xlsx
Elevation, (all_dem_table), Surface roughness (all_roughness_table), and SOC (soc1_grid_table) data per district.
### lakes_per_grid.xlsx
All lakes, calculated per grid using a spatial join. 
### grid_stats_hc
Contains all data per grid, including the 6 classified clusters (hc_k6_named)


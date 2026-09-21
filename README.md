# MSc_Thesis_L_DLB_Landscapes
R scripts, GIS-Products, and data sheets for my MSc Thesis: Pan-Arctic Analysis of Thermokarst Lake-Drained Lake Basin Landscapes using Hierarchical Clustering

# GIS Data
## GIS input data
input data includes: 
'Occurrence' data derived from Global Surface Water: https://global-surface-water.appspot.com/download
DLB classification data, unpublished. Contact Helena Bergstedt PhD for data accces

## GIS data products
### lakes_14400_smoothed
polygon data with all lakes within districts, derived from the surface water data. lakes smaller than 14400m2 are filtered out
### fihsnet_districts_v2_ClipLayer
grid with 10 x 10km cells. spatial join with cluster_data_sheet# required for visualization. Common ID is: OBJECTID (for fihsnet_districts_v2_ClipLayer), and 
FID_fihsnet_districts_v2_ClipLayer (for cluster_data_sheet#)
### lake_districts_final
boundaries of 28 lake districts
### Circumpolar_Thermokarst_Landscapes
thermokarst lake landscapes, derived from Olefeldt et al. (2016)
### lake_dlb_raster_all_v2
Land cover raster of all districts with classes: lake, DLB, upland, lowland, and river/open water
### cluster_data_sheet#
data sheet with all information for each grid cell, including the 6 clusters


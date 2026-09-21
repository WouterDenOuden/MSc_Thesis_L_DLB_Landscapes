# create boxplots of elevatiom, roughness, soc, and ground ice for each cluster

library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(readxl)


# load in rdem, roughness, and soc
grids_data1 <- read_excel("C:/Users/woute/MSc thesis L-DLB/Data/grid_soil_data.xlsx")
names(grids_data1)[1] <- "FID_fihsnet_districts_v2_ClipLayer"

merged_df <- grid_stats |>
  left_join(
    grids_data1 |>
      select(
        FID_fihsnet_districts_v2_ClipLayer,
        all_roughness_table.MEAN,
        all_dem_table.MEAN,
        soc1_grid_table.MEAN
      ),
    by = "FID_fihsnet_districts_v2_ClipLayer")


merged_df <- merged_df %>%
  filter(
    !grepl(",", districts),          # remove multi-district grids
    !is.na(districts),               # keep only valid districts
    !is.na(hc_k6_named)) # keep only valid elevation values


# load in ground ice data
grids_data_ice <- read_excel("C:/Users/woute/MSc thesis L-DLB/Data/ground_ice_grids.xlsx")
names(grids_data_ice) <- paste0("all_ground_ice.", names(grids_data_ice))
names(grids_data_ice)[2] <- "FID_fihsnet_districts_v2_ClipLayer"

merged_df <- merged_df |>
  left_join(
    grids_data_ice |>
      select(
        FID_fihsnet_districts_v2_ClipLayer,
        all_ground_ice.MEAN),
    by = "FID_fihsnet_districts_v2_ClipLayer")

cluster_cols <- c(
  "1.B.a"      = "#2A7F9E",  # deep lake blue
  "1.A"    = "#00CE00",  # bright DLB green
  "1.B.b" = "#1F6FE0",  # medium lake blue
  "2.B.a"    = "#B08A55",  # tundra brown
  "2.B.b"    = "#7A7A2F",  # upland olive
  "2.A"  = "#1A7A2F"  # dark olive green
)

p_dem <- ggplot(merged_df, aes(
  x = factor(hc_k6_named),
  y = all_dem_table.MEAN,
  fill = factor(hc_k6_named)
)) +
  geom_boxplot(outlier.alpha = 0.4) +
  scale_fill_manual(values = cluster_cols) +
  labs(x = "Cluster", y = "Mean elevation") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")


p_sr <- ggplot(merged_df, aes(
  x = factor(hc_k6_named),
  y = all_roughness_table.MEAN,
  fill = factor(hc_k6_named)
)) +
  geom_boxplot(outlier.alpha = 0.4) +
  scale_fill_manual(values = cluster_cols) +
  labs(x = "Cluster", y = "Mean surface roughness") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

p_soc <- ggplot(merged_df, aes(
  x = factor(hc_k6_named),
  y = soc1_grid_table.MEAN,
  fill = factor(hc_k6_named)
)) +
  geom_boxplot(outlier.alpha = 0.4) +
  scale_fill_manual(values = cluster_cols) +
  labs(x = "Cluster", y = "Mean SOC") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

p_gi <- ggplot(merged_df, aes(
  x = factor(hc_k6_named),
  y = all_ground_ice.MEAN,
  fill = factor(hc_k6_named)
)) +
  geom_boxplot(outlier.alpha = 0.4) +
  scale_fill_manual(values = cluster_cols) +
  labs(x = "Cluster", y = "Mean ground-ice") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")



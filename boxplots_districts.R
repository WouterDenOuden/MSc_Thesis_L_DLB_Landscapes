# boxplot figures for the 28 distrcits

library(dplyr)
library(scales)
library(dplyr)
library(ggplot2)

# load in lakes dataset: these are all lakes in vectorized format
lakes_smooth <- read_excel("ArcGIS/Projects/L-DLB_V2/lakes_14400_smoothed_TableToExcel.xlsx")

# filter out the lakes smaller than 14400m2 (split lakes, occur at edges of districts)
lakes_smooth <- lakes_smooth[lakes_smooth$Shape_Area >= 14400, ] 

# lakes area in km2
lakes_smooth$lakes_area_km2 <- lakes_smooth$Shape_Area / 1000000

# rename the districts
lakes_smooth$districts_new <- dplyr::recode(lakes_smooth$District,
                                            "Lena / Viljuy"      = "Tyung river",
                                            "North Russia"       = "Nenets",
                                            "West Siberia"       = "Nadym river",
                                            "Yamal"              = "Yamal peninsula",
                                            "Gulf of Ob"         = "Central Yamalia",
                                            "Gydan"              = "Gydan peninsula",
                                            "Anabarsky"          = "North Siberian lowland",
                                            "Tuktut"             = "Northwest Kitikmeot",
                                            "Mackenzie inland"  = "Mackenzie river",
                                            "Northslope"         = "Northslope Alaska",
                                            "Ayon island"        = "Chaunsky district",
                                            "Siberian coast"     = "Yana-Indigirka lowland",
                                            "Seward"             = "Northern Seward peninsula",
                                            "West Tajminsky"     = "Taymir peninsula (west)",
                                            "East Tajminsky"     = "Taymir peninsula (east)",
                                            "Kolyama"            = "Kolyama river",
                                            "Old crow"        = "Old Crow flats")




# rename some labels to new abbreviations
lakes_smooth <- lakes_smooth |>
  mutate(
    abbr = dplyr::recode(districts_new,
                         "Taymir peninsula (west)"  = "Taymir pen. (E)",
                         "Taymir peninsula (east)"  = "Taymir pen. (W)",
                         "North Siberian lowland"   = "N-Siberian LL",
                         "Yana-Indigirka lowland"   = "YI Lowland",
                         "Northern Seward peninsula"= "N-Seward pen.",
                         "Northwest Kitikmeot"      = "NW-Kitikmeot") )


# rename some labels to new abbreviations
grid_stats <- grid_stats |>
  mutate(
    abbr = dplyr::recode(districts_new,
                         "Taymir peninsula (west)"= "Taymir pen. (E)",
                         "Taymir peninsula (east)"= "Taymir pen. (W)",
                         "North Siberian lowland" = "N-Siberian LL",
                         "Yana-Indigirka lowland"= "YI Lowland",
                         "Northern Seward peninsula"= "N-Seward pen.",
                         "Northwest Kitikmeot"= "NW-Kitikmeot"))

## a few grid cells cover two districts (26), and they are removed for the boxplots
# remove the grids that count for two districts, these are:
grid_stats_filter <- grid_stats |>
  filter(!districts %in% c(
    "Kolyama, Siberian coast",
    "East Tajminsky, West Tajminsky"
  ))


# calculate averages for every district
district_means <- grid_stats_filter |>
  group_by(districts_new) |>
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))





# Order districts by decreasing mean lake size
district_order1 <- lakes_smooth %>%
  group_by(abbr) %>%
  summarise(mean_area = mean(lakes_area_km2, na.rm = TRUE)) %>%
  arrange(desc(mean_area)) %>%
  pull(abbr)



# four boxplots for lake size, lake density, variance, and skewness, per district
plot_lake_size <- ggplot(lakes_smooth, aes(
  x = factor(abbr, levels = district_order1),
  y = lakes_area_km2
)) +
  geom_boxplot(fill = "#2C7BB6", alpha = 0.7, outlier.alpha = 0.4) +
  scale_y_log10(
    breaks = 10^(-1:3),
    labels = label_log()) +
  labs(
    x = NULL,
    y = expression("Lake size [km"^2*"]")
  ) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x = element_blank()) +
  geom_hline(yintercept = mean(lakes_smooth$lakes_area_km2),
             linetype = "dashed", colour = "black")



plot_lake_density <- ggplot(grid_stats_filter, aes(
  x = factor(abbr, levels = district_order1),
  y = n_lakes_km2
)) +
  geom_boxplot(fill = "#2C7BB6", alpha = 0.7, outlier.alpha = 0.4) +
  labs(
    x = NULL,
    y = expression("Lake density [n/km"^2*"]")
  ) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x = element_blank()) +
  geom_hline(yintercept = mean(grid_stats_filter$n_lakes_km2),
             linetype = "dashed", colour = "black")


plot_variance <- ggplot(grid_stats_filter, aes(
  x = factor(abbr, levels = district_order1),
  y = variance_size
)) +
  geom_boxplot(fill = "#2C7BB6", alpha = 0.7, outlier.alpha = 0.4) +
  scale_y_log10(labels = label_log()) +
  labs(
    x = NULL,
    y = expression("Variance [km"^4*"]")
  ) +
  theme_minimal(base_size = 17) +
  theme(axis.text.x = element_blank()) +
  geom_hline(yintercept = mean(grid_stats_filter$variance_size, na.rm = TRUE),
             linetype = "dashed", colour = "black")


# this plot contains district labels on x axis
plot_skewness <- ggplot(grid_stats_filter, aes(
  x = factor(abbr, levels = district_order1),
  y = skewness_size
)) +
  geom_boxplot(fill = "#2C7BB6", alpha = 0.7, outlier.alpha = 0.4) +
  labs(
    x = NULL,
    y = "Skewness"
  ) +
  theme_minimal(base_size = 17) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 16),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  geom_hline(yintercept = mean(grid_stats_filter$skewness_size, na.rm = TRUE),
             linetype = "dashed", colour = "black")


png("stacked_boxplot.png", width = 4000, height = 3500, res = 300)
(plot_lake_size / plot_variance / plot_lake_density / plot_lake_density)
dev.off()

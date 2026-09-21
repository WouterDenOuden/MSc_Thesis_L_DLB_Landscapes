## cluster frequencies per district, and correlation heatmap/matrix
library(dplyr)
library(tidyr)
library(pheatmap)
library(viridis)
library(ggplot2)
## use the dataframe grid_stats

# make the abbreviations for the districts
grid_stats_clean <- grid_stats |>
  mutate(
    abbr = dplyr::recode(districts_new,
                         "Taymir peninsula (west)"  = "Taymir pen. (E)",
                         "Taymir peninsula (east)"  = "Taymir pen. (W)",
                         "North Siberian lowland"   = "N-Siberian LL",
                         "Yana-Indigirka lowland"   = "YI Lowland",
                         "Northern Seward peninsula"= "N-Seward pen.",
                         "Northwest Kitikmeot"      = "NW-Kitikmeot"    )  )
################### cluster freq per district ####

# remove grids in 2 districts
grid_stats_clean <- grid_stats_clean %>%
  filter(
    !grepl(",", abbr),      # remove multi-district grids
    !is.na(abbr),
    !is.na(hc_k6_named)
  )


df_freq <- grid_stats_clean %>%
  mutate(weight = grid_area_km2 / 100) %>%        # 50 km2 → 0.5, 100 km2 → 1.0
  group_by(abbr, hc_k6_named) %>%
  summarise(weight_sum = sum(weight), .groups = "drop") %>%
  group_by(abbr) %>%
  mutate(freq_pct = (weight_sum / sum(weight_sum)) * 100) %>%
  ungroup()


########### district simmilarity heatmap #########


#  matrix
wide <- df_freq %>%
  select(abbr, hc_k6_named, freq_pct) %>%
  pivot_wider(
    names_from = hc_k6_named,
    values_from = freq_pct,
    values_fill = 0
  ) %>%
  arrange(abbr)


wide_mat <- as.data.frame(wide)
rownames(wide_mat) <- wide_mat$abbr
wide_mat$abbr <- NULL

dist_mat <- dist(wide_mat, method = "euclidean")
dist_mat <- as.matrix(dist_mat)


png("heatmap_k6.png", width = 2000, height = 2000, res = 300)

#?pheatmap
hm <- pheatmap(
  dist_mat,
  clustering_distance_cols = "euclidean",
  clustering_method = "ward.D2",
  color = colorRampPalette(c("#f7f7f7", "#cccccc", "#969696", "#525252"))(200),
  fontsize = 4,
  fontsize_row = 8,
  fontsize_col = 8,
  border_color = NA,
  legend = TRUE,
  display_numbers = round(dist_mat, 0),   # <-- add numbers
  number_color = "black",                 # <-- choose text colour
  fontsize_number = 6                     # <-- adjust number size
)



hm

dev.off()




######## cluster freq k = 6 per district #####


# this label is for the correct figure
labels_k6 <- c(
  "2.B.b",
  "2.B.a",
  "2.A",
  "1.B.b",
  "1.B.a",
  "1.A")

# this labels is for the correct legend
labels_k6 <- c("1.A",  "1.B.a",  "1.B.b",  "2.A",  "2.B.a",  "2.B.b")

cluster_cols_k6 <- c(
  "1.B.a"      = "#2A7F9E",  # deep lake blue
  "1.A"    = "#00CE00",  # bright DLB green
  "1.B.b" = "#1F6FE0",  # medium lake blue
  "2.B.a"    = "#B08A55",  # tundra brown
  "2.B.b"    = "#7A7A2F",  # upland olive
  "2.A"  = "#1A7A2F"  # dark olive green
)


district_order <- rev(hm$tree_row$labels[hm$tree_row$order])


png("stacked_k6.png", width = 2000, height = 3000, res = 300)

ggplot(df_freq, aes(
  y = factor(abbr, levels = district_order),
  x = freq_pct,
  fill = factor(hc_k6_named, levels = rev(labels_k6))   # reverse order
)) +
  geom_col(width = 0.8) +
  
  # horizontal dotted separators
  geom_hline(
    yintercept = seq(1.5, length(district_order) - 0.5, by = 1),
    linetype = "dotted",
    color = "black",
    linewidth = 0.3
  ) +
  
  scale_fill_manual(
    values = cluster_cols_k6,
    breaks = labels_k6
  ) +
  
  labs(
    x = "Cluster frequency (%)",
    fill = "Cluster"
  ) +
  
  theme_minimal(base_size = 8) +
  theme(
    axis.text.y = element_text(size = 15, colour = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

dev.off()

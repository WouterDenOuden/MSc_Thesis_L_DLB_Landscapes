# code for cluster experiments, finding K using sil values
# relabelling clusters
# create cluster summaries
# plot cluster dendrogram
# calculate silhouette values for the six clusters

library(readxl)
library(moments)
library(ggplot2)
library(dplyr)
library(writexl)
library(forcats)
library(purrr)
library(factoextra)
library(cluster)
library(dendextend)
library(tidyr)
library(stringr)



# select the eight landscape metrics
vars <- grid_stats |>
  dplyr::select(
    relative_freq_upland,
    relative_freq_dlb,
    relative_freq_lake,
    n_lakes_km2,
    mean_circ,
    log_median_lake_size,
    log_variance_size,
    log_skewness_size
  ) |>
  mutate(across(everything(), as.numeric))

# clean and scale variables
vars_clean <- vars[apply(vars, 1, function(x) all(is.finite(x))), ]
vars_scaled <- scale(vars_clean)

#distance matrix
dist_matrix <- dist(vars_scaled, method = "euclidean")

#create3 clustering object
hc <- hclust(dist_matrix, method = "ward.D2")

# dendrogram
plot(hc, labels = FALSE, hang = -1, main = "Hierarchical Clustering Dendrogram")

## cluster 18 times

clusters2 <- cutree(hc, k = 2)
clusters3 <- cutree(hc, k = 3)
clusters4 <- cutree(hc, k = 4)
clusters5 <- cutree(hc, k = 5)
clusters6 <- cutree(hc, k = 6)
clusters7 <- cutree(hc, k = 7)
clusters8 <- cutree(hc, k = 8)
clusters9 <- cutree(hc, k = 9)
clusters10 <- cutree(hc, k = 10)
clusters11 <- cutree(hc, k = 11)
clusters12 <- cutree(hc, k = 12)
clusters13 <- cutree(hc, k = 13)
clusters14 <- cutree(hc, k = 14)
clusters15 <- cutree(hc, k = 15)
clusters16 <- cutree(hc, k = 16)
clusters17 <- cutree(hc, k = 17)
clusters18 <- cutree(hc, k = 18)

####### check the silhouette scores and find a suitable k #####

k_values <- 2:18
sil_cluster_means <- list()

for (k in k_values) {
  
  # retrieve cluster vector (clusters2, clusters3, ..., clusters18)
  clusters_k <- get(paste0("clusters", k))
  
  # compute silhouette
  sil_k <- silhouette(clusters_k, dist_matrix)
  
  # convert to dataframe
  sil_df <- data.frame(
    k = k,
    cluster = clusters_k,
    silhouette_width = sil_k[, "sil_width"]
  )
  
  # compute mean silhouette per cluster
  mean_sil_k <- sil_df %>%
    group_by(k, cluster) %>%
    summarise(mean_sil = mean(silhouette_width), .groups = "drop")
  
  sil_cluster_means[[paste0("k", k)]] <- mean_sil_k
}


### sil means per cluster
sil_summary <- map_df(names(sil_cluster_means), function(kname) {
  tib <- sil_cluster_means[[kname]]
  
  tib %>%
    summarise(
      k = as.integer(gsub("k", "", kname)),
      mean_of_means = mean(mean_sil)
    )
})

sil_summary


silhouette_scores <- numeric(length(k_values))

# sil means for the whole dataset, from k = 2 to 18
for (i in seq_along(k_values)) {
  k <- k_values[i]
  
  # Retrieve your cluster vector: clusters2, clusters3, ..., clusters18
  clusters_k <- get(paste0("clusters", k))
  
  # Compute silhouette
  sil_k <- silhouette(clusters_k, dist_matrix)
  
  # Store the mean silhouette score
  silhouette_scores[i] <- mean(sil_k[, "sil_width"])
  
  cat("For k =", k, "Silhouette Score:", round(silhouette_scores[i], 3), "\n")}


df_sil <- data.frame(
  k = k_values,
  cell_mean = silhouette_scores,
  cluster_mean = sil_summary$mean_of_means)

png("sil_scores.png", width = 2500, height = 1200, res = 300)

ggplot(df_sil, aes(x = k)) +
  geom_line(aes(y = cell_mean, colour = "Cell-level mean"), linewidth = 0.7) +
  geom_point(aes(y = cell_mean, colour = "Cell-level mean"), size = 2) +
  geom_line(aes(y = cluster_mean, colour = "Cluster-level mean"), linewidth = 0.7) +
  geom_point(aes(y = cluster_mean, colour = "Cluster-level mean"), size = 2) +
  
  # Red circles around k = 6
  geom_point(
    data = df_sil[df_sil$k == 6, ],
    aes(x = k, y = cell_mean),
    colour = "red", size = 5, shape = 1, stroke = 1.2
  ) +
  geom_point(
    data = df_sil[df_sil$k == 6, ],
    aes(x = k, y = cluster_mean),
    colour = "red", size = 5, shape = 1, stroke = 1.2
  ) +
  
  scale_colour_manual(values = c("darkred", "darkblue")) +
  labs(
    x = "Number of clusters (k)",
    y = "Mean silhouette score",
    colour = "",
    title = "Mean Silhouette scores across cluster levels"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

dev.off()
### k = 6 is selected



####################### manual label mapping ##########
# table: 2, 1
label_map_k2 <- c(
  "1" = "2",
  "2" = "1")

# table: 2.A, 2.B, 1
label_map_k3 <- c(
  "1" = "2.A",
  "2" = "2.B",
  "3" = "1")

# table: 2.A, 2.B.b, 2.B.a, 1
label_map_k4 <- c(
  "1" = "2.A",
  "2" = "2.B.b",
  "3" = "2.B.a",
  "4" = "1")

# table: 2.A, 2.B.b, 2.B.a, 1.B, 1.A
label_map_k5 <- c(
  "1" = "2.A",
  "2" = "2.B.b",
  "3" = "2.B.a",
  "4" = "1.B",
  "5" = "1.A")

# table: 2.A, 2.B.b, 2.B.a, 1.B.b, 1.B.a, 1.A
label_map_k6 <- c(
  "1" = "2.A",
  "2" = "2.B.b",
  "3" = "2.B.a",
  "4" = "1.B.b",
  "5" = "1.B.a",
  "6" = "1.A")




##################### summaries and profiles ###########

# create named cluster vectors for each k  

clusters2_named  <- label_map_k2[as.character(clusters2)]
clusters3_named  <- label_map_k3[as.character(clusters3)]
clusters4_named  <- label_map_k4[as.character(clusters4)]
clusters5_named  <- label_map_k5[as.character(clusters5)]
clusters6_named  <- label_map_k6[as.character(clusters6)]




#expand cluster vectors back to full dataset

valid_rows <- apply(vars, 1, function(x) all(is.finite(x)))

grid_stats$hc_k2_named <- NA
grid_stats$hc_k3_named  <- NA
grid_stats$hc_k4_named  <- NA
grid_stats$hc_k5_named  <- NA
grid_stats$hc_k6_named  <- NA

grid_stats$hc_k2_named[valid_rows]  <- clusters2_named
grid_stats$hc_k3_named[valid_rows]  <- clusters3_named
grid_stats$hc_k4_named[valid_rows]  <- clusters4_named
grid_stats$hc_k5_named[valid_rows]  <- clusters5_named
grid_stats$hc_k6_named[valid_rows]  <- clusters6_named


## compare the splits of each cluster

# creating cluster profile for each
make_profile <- function(cluster_vec) {
  vars_scaled |>
    as.data.frame() |>
    mutate(cluster = cluster_vec[valid_rows]) |>
    group_by(cluster) |>
    summarise(across(everything(), mean))
}




cluster_profiles_k2  <- make_profile(grid_stats$hc_k2_named)
cluster_profiles_k3  <- make_profile(grid_stats$hc_k3_named)
cluster_profiles_k4  <- make_profile(grid_stats$hc_k4_named)
cluster_profiles_k5  <- make_profile(grid_stats$hc_k5_named)
cluster_profiles_k6  <- make_profile(grid_stats$hc_k6_named)

# and comparing those profiles
compare_children <- function(profiles, child1, child2) {
  
  # Extract rows
  row1 <- profiles[profiles$cluster == child1, ]
  row2 <- profiles[profiles$cluster == child2, ]
  
  # Remove the cluster column
  v1 <- as.numeric(row1[,-1])
  v2 <- as.numeric(row2[,-1])
  
  # Compute difference
  diff <- abs(v1 - v2)
  
  # Build table
  tibble(
    Variable = colnames(profiles)[-1],
    !!child1 := v1,
    !!child2 := v2,
    Difference = diff
  ) |>
    arrange(desc(Difference))
}

# 1st split
compare_children(
  profiles = cluster_profiles_k2,
  child1 = "1",
  child2 = "2")

# 2nd split
compare_children(
  profiles = cluster_profiles_k3,
  child1 = "2.A",
  child2 = "2.B")

#3rd split
compare_children(
  profiles = cluster_profiles_k4,
  child1 = "2.B.a",
  child2 = "2.B.b")

#4th split
compare_children(
  profiles = cluster_profiles_k5,
  child1 = "1.A",
  child2 = "1.B")

#5th split
compare_children(
  profiles = cluster_profiles_k6,
  child1 = "1.B.a",
  child2 = "1.B.b")




# creating a lisgt with summaries for each cluster
var_list <- list(
  mean_log_median_lake_size = "Log median lake size",
  mean_lake_density    = "Lake density",
  mean_variance        = "Variance",
  mean_skewness        = "Skewness",
  mean_circularity     = "Circularity",
  mean_lake_fraction   = "Lake AF",
  mean_dlb_fraction    = "DLB AF",
  mean_upland_area     = "Upland AF"
)

#### compute mean and sd per cluster ############
summary_tbl <- grid_stats %>%
  filter(!is.na(hc_k6_named)) %>%
  group_by(hc_k6_named) %>%
  summarise(
    mean_log_median_lake_size = sprintf("%.2f ± %.2f",
                                   mean(log_median_lake_size, na.rm = TRUE),
                                   sd(log_median_lake_size, na.rm = TRUE)),
    mean_lake_density    = sprintf("%.2f ± %.2f",
                                   mean(n_lakes_km2, na.rm = TRUE),
                                   sd(n_lakes_km2, na.rm = TRUE)),
    mean_variance        = sprintf("%.2f ± %.2f",
                                   mean(log_variance_size, na.rm = TRUE),
                                   sd(log_variance_size, na.rm = TRUE)),
    mean_skewness        = sprintf("%.2f ± %.2f",
                                   mean(log_skewness_size, na.rm = TRUE),
                                   sd(log_skewness_size, na.rm = TRUE)),
    mean_circularity     = sprintf("%.2f ± %.2f",
                                   mean(mean_circ, na.rm = TRUE),
                                   sd(mean_circ, na.rm = TRUE)),
    mean_lake_fraction   = sprintf("%.2f ± %.2f",
                                   mean(relative_freq_lake, na.rm = TRUE),
                                   sd(relative_freq_lake, na.rm = TRUE)),
    mean_dlb_fraction    = sprintf("%.2f ± %.2f",
                                   mean(relative_freq_dlb, na.rm = TRUE),
                                   sd(relative_freq_dlb, na.rm = TRUE)),
    mean_upland_area     = sprintf("%.2f ± %.2f",
                                   mean(relative_freq_upland, na.rm = TRUE),
                                   sd(relative_freq_upland, na.rm = TRUE)),
    n_cells              = as.character(n()),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -c(hc_k6_named),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(variable = recode(variable, !!!var_list)) %>%
  pivot_wider(
    names_from = hc_k6_named,
    values_from = value
  ) %>%
  arrange(factor(variable, levels = c(
    "Log median lake size",
    "Lake density",
    "Variance",
    "Skewness",
    "Circularity",
    "Lake AF",
    "DLB AF",
    "Upland AF",
    "n_cells")))


write_xlsx(summary_tbl, "summary_hc_k6.xlsx")


## add sil values to grid cells

##### silhouette values for k = 6
sil_k6 <- silhouette(clusters6, dist_matrix)
sil_k6_df <- as.data.frame(sil_k6)

grid_stats$sil_k6       <- NA
grid_stats$alt_k6_named <- NA

grid_stats$sil_k6[valid_rows] <- sil_k6_df$sil_width


write_xlsx(grid_stats, "grid_stats_hc.xlsx")



################## k =6 plotting the dendrogram ##############


k <- 6
clusters_k <- clusters6

#colors for the 6 clusters:: left to right
cluster_cols_num <- c(
  "1"  = "#00CE00",
  "2" =  "#2A7F9E",
  "3"  = "#1F6FE0",
  "4"  = "#1A7A2F",
  "5"  = "#B08A55",
  "6"  = "#7A7A2F")

dend <- as.dendrogram(hc)

dend_colored <- dend %>%
  set("branches_k_color", value = cluster_cols_num, k = k)

## coloured dendrogram

png("dendrogram_k6.png", width = 7000, height = 4000, res = 300)

plot(
  dend_colored,
  main = paste("Hierarchical Clustering Dendrogram (k =", k, ")"),
  ylab = "Height",
  leaflab = "none",
  cex.main = 2.5,   # bigger title
  cex.lab  = 1.5,     # bigger axis labels
  cex.axis = 1.4    # bigger tick labels
)

dev.off()

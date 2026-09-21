# scatterplot figures and their formulas


library(scales)


## using district means created in boxplot_districts.R


###### testing model fit

#linear model
m_linear <- lm(variance_size ~ mean_lake_size, data = district_means)

# exponential model 
m_exp <- lm(log(variance_size) ~ mean_lake_size, data = district_means)

# power-law model 
m_power <- lm(log(variance_size) ~ log(mean_lake_size), data = district_means)


# extract model values
model_names <- c("Linear", "Exponential (log-linear)", "Power-law (log-log)")

AIC_vals  <- c(AIC(m_linear),  AIC(m_exp),  AIC(m_power))
BIC_vals  <- c(BIC(m_linear),  BIC(m_exp),  BIC(m_power))
R2_vals   <- c(summary(m_linear)$r.squared,
               summary(m_exp)$r.squared,
               summary(m_power)$r.squared)
adjR2_vals <- c(summary(m_linear)$adj.r.squared,
                summary(m_exp)$adj.r.squared,
                summary(m_power)$adj.r.squared)
sigma_vals <- c(summary(m_linear)$sigma,
                summary(m_exp)$sigma,
                summary(m_power)$sigma)

comparison_table <- data.frame(
  Model = model_names,
  AIC = AIC_vals,
  BIC = BIC_vals,
  R2 = R2_vals,
  Adj_R2 = adjR2_vals,
  Residual_SD = sigma_vals
)

print(comparison_table)

# --> it seems that log-log is the best fit: R2 = 0.78824


######### scatterplots with model fits #######

# fit a log-log model
fit <- lm(log10(variance_size) ~ log10(mean_lake_size), data = district_means)
pred_df <- data.frame(
  mean_lake_size = seq(
    min(district_means$mean_lake_size),
    max(district_means$mean_lake_size),
    length.out = 300
  )
)


pred <- predict(fit, newdata = pred_df, interval = "confidence")

pred_df$fit <- 10^(pred[, "fit"])
pred_df$lwr <- 10^(pred[, "lwr"])
pred_df$upr <- 10^(pred[, "upr"])

summary(fit)$call
names(fit$model)
head(fit$model)


# extract coefficients
b0 <- coef(fit)[1]   # intercept in log10 space
b1 <- coef(fit)[2]   # slope (scaling exponent)

# convert to power-law parameters
a <- 10^(b0)         
b <- b1           

# Print equation
cat("Power-law equation:\n")
cat("Var(X) = ", a, " * A^", b, "\n", sep = "")
# the formula is: Var(X) = 29.22326 * A^2.464175



# pllot scatterplot for mean lake size and variance, per district
pvar <- ggplot(district_means, aes(x = mean_lake_size, y = variance_size)) +
  geom_point(size = 2, alpha = 0.8, color = "#2C7BB6") +
  scale_x_continuous(labels = label_number()) +
  scale_y_continuous(labels = label_number()) +
  labs(
    x = expression("Mean lake size [km"^2*"]"),
    y = expression("Variance [km"^4*"]"),
    title = "Variance vs. Mean Lake Size"
  ) +
  theme_minimal()


R2 <- summary(fit)$r.squared

## add lower and upper confidence intervals
p2 <- pvar +
  geom_ribbon(
    data = pred_df,
    aes(x = mean_lake_size, ymin = lwr, ymax = upr),
    inherit.aes = FALSE,
    fill = "grey85",
    alpha = 0.4
  ) +
  geom_line(
    data = pred_df,
    aes(x = mean_lake_size, y = fit),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.7
  ) + ## add formula in plot
  annotate(
    "text",
    x = max(district_means$mean_lake_size) * 0.6,
    y = max(district_means$variance_size) * 0.95,
    label = paste0("Var(x) == ", round(a, 2), " %*% x^2.46"),
    parse = TRUE,
    color = "grey20",
    size = 5
  ) +
  annotate(
    "text",
    x = max(district_means$mean_lake_size) * 0.6,
    y = max(district_means$variance_size) * 0.85,
    label = paste0("R^2 == ", round(R2, 3)),
    parse = TRUE,
    color = "grey20",
    size = 5
  ) + 
  
  scale_y_continuous(
    breaks = seq(0, 10, by = 2),
    labels = label_number()
  ) +
  coord_cartesian(ylim = c(0, 10))



# for skewness
fit_skew <- lm(skewness_size ~ n_lakes_km2, data = district_means)

# Extract coefficients
b0 <- coef(fit_skew)[1]   # intercept in log10 space
b1 <- coef(fit_skew)[2]   # slope (scaling exponent)

# Convert to power-law parameters
a <- (b0)         # intercept in original units
b <- b1              # exponent

# Print equation
cat("Power-law equation:\n")
cat("Skew(X) = ", a, " + X^", b, "\n", sep = "")
# so my formula is: Skew(X) = 1.124855 + X^4.06333

pred_df_skew <- data.frame(
  n_lakes_km2 = seq(
    min(district_means$n_lakes_km2),
    max(district_means$n_lakes_km2),
    length.out = 300))

R2_skew <- summary(fit_skew)$r.squared
pred_skew <- predict(fit_skew, newdata = pred_df_skew, interval = "confidence")
pred_df_skew$fit <- pred_skew[, "fit"]
pred_df_skew$lwr <- pred_skew[, "lwr"]
pred_df_skew$upr <- pred_skew[, "upr"]

#skewness and lake density plot
pskew <- ggplot(district_means, aes(x = n_lakes_km2, y = skewness_size)) +
  geom_point(size = 2, alpha = 0.8, color = "#2C7BB6") +
  labs(
    x = expression("Lake density [n/km"^2*"]"),
    y = "Skewness",
    title = "Skewness vs. Lake density"
  ) +
  theme_minimal()

# adding the confidence intervals and formula
p1 <- pskew +
  geom_ribbon(
    data = pred_df_skew,
    aes(x = n_lakes_km2, ymin = lwr, ymax = upr),
    inherit.aes = FALSE,
    fill = "grey85",
    alpha = 0.4
  ) +
  geom_line(
    data = pred_df_skew,
    aes(x = n_lakes_km2, y = fit),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.7
  ) +
  #geom_text(
  #  data = subset(district_means, districts_new == "Yamal peninsula"),
  #  aes(x = n_lakes_km2, y = skewness_size, label = "YP"),
  #  vjust = -0.8,
  #  color = "black",
  #  size = 3
  # ) +
  annotate(
    "text",
    x = 0.38,
    y = max(district_means$skewness_size) * 0.94,
    label = paste0("Skew(x) == ", round(a, 2), " + ", round(b, 2), "%*% x"),
    parse = TRUE,
    color = "grey20",
    size = 5
  ) +
  annotate(
    "text",
    x = max(district_means$n_lakes_km2) * 0.47,
    y = max(district_means$skewness_size) * 0.88,
    label = paste0("R² = ", round(R2_skew, 3)),
    color = "grey20",
    size = 5
  ) +
  coord_cartesian(ylim = c(1, max(district_means$skewness_size)))

png("var_skew.png", width = 2500, height = 1250, res = 300)

p2 | p1

dev.off()


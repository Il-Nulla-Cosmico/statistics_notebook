# ============================================================
# Statistics for Life Sciences
# Notebook 01 — Standard Deviation vs Standard Error
# Script 02: Data visualisation
# Dataset: iris — Iris setosa, Petal.Length
# Dependencies: ggplot2
# ============================================================

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)
if (!dir.exists("img")) dir.create("img")

data(iris)
setosa_pl <- iris[iris$Species == "setosa", "Petal.Length"]
n      <- length(setosa_pl)
xbar   <- mean(setosa_pl)
sd_val <- sd(setosa_pl)
se_val <- sd_val / sqrt(n)
t_crit <- qt(0.975, df = n - 1)
ci_low <- xbar - t_crit * se_val
ci_hi  <- xbar + t_crit * se_val

# --- Palette viola / fucsia ---
col_violet      <- "#5e4b8b"   # viola scuro  — SD / main
col_violet_mid  <- "#9b72cf"   # viola medio  — fill
col_violet_lt   <- "#c084fc"   # viola chiaro — accento
col_fuchsia     <- "#e879a0"   # fucsia       — SE / contrasto
col_rose        <- "#f0abcb"   # rosa pallido — fill leggero
col_mean        <- "#1a1a2e"   # quasi nero   — linea media
col_blue        <- "#1565c0"   # blu          — IC 95%

base_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, color = "grey40"),
    plot.caption     = element_text(size = 8,  color = "grey55"),
    panel.grid.minor = element_blank()
  )

df <- data.frame(x = setosa_pl)

# ============================================================
# PLOT 1 — Individual distribution + SD
# ============================================================
p1 <- ggplot(df, aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 0.1, fill = col_violet_mid,
                 color = "white", alpha = 0.70) +
  geom_density(color = col_violet, linewidth = 1.2) +
  geom_vline(xintercept = xbar,
             color = col_mean, linewidth = 1.2) +
  geom_vline(xintercept = c(xbar - sd_val, xbar + sd_val),
             color = col_violet, linetype = "dashed", linewidth = 1) +
  annotate("segment",
           x = xbar - sd_val, xend = xbar + sd_val,
           y = 2.5, yend = 2.5,
           color = col_violet, linewidth = 1.2,
           arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = xbar, y = 2.78,
           label = paste0("SD = \u00b1", round(sd_val, 3), " cm"),
           fontface = "bold", color = col_violet, size = 3.8) +
  annotate("text", x = xbar + 0.01, y = 3.3,
           label = paste0("mean = ", round(xbar, 3), " cm"),
           fontface = "bold", color = col_mean, size = 3.6, hjust = 0) +
  labs(
    title    = "Distribution of individual values — Iris setosa, Petal Length",
    subtitle = "SD describes the spread of individual observations around the mean",
    x = "Petal length (cm)", y = "Density",
    caption  = "Data: iris (Fisher 1936)"
  ) +
  base_theme

# ============================================================
# PLOT 2 — SD vs SE vs 95% CI as error bars
# ============================================================
df_err <- data.frame(
  index = factor(c("Mean \u00b1 SD", "Mean \u00b1 SE", "95% CI"),
                 levels = c("Mean \u00b1 SD", "Mean \u00b1 SE", "95% CI")),
  mean  = rep(xbar, 3),
  lower = c(xbar - sd_val, xbar - se_val, ci_low),
  upper = c(xbar + sd_val, xbar + se_val, ci_hi)
)
p2 <- ggplot(df_err, aes(x = index, y = mean, color = index,
                          ymin = lower, ymax = upper)) +
  geom_point(size = 4.5) +
  geom_errorbar(width = 0.18, linewidth = 1.4) +
  scale_color_manual(values = c(
    "Mean \u00b1 SD" = col_violet,
    "Mean \u00b1 SE" = col_fuchsia,
    "95% CI"        = col_blue
  )) +
  labs(
    title    = "Visual comparison: SD, SE and 95% CI",
    subtitle = "All three share the same unit (cm) but describe different things",
    x = NULL, y = "Petal length (cm)",
    caption  = "Data: iris (Fisher 1936)"
  ) +
  base_theme +
  theme(legend.position = "none")

# ============================================================
# PLOT 3 — Sampling distribution (bootstrap)
# ============================================================
set.seed(42)
boot_means <- replicate(5000, mean(sample(setosa_pl, n, replace = TRUE)))
se_boot    <- sd(boot_means)

p3 <- ggplot(data.frame(m = boot_means), aes(x = m)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = col_rose, color = "white", alpha = 0.85) +
  geom_density(color = col_fuchsia, linewidth = 1.3) +
  geom_vline(xintercept = xbar,
             color = col_mean, linewidth = 1.2) +
  geom_vline(xintercept = c(xbar - se_val, xbar + se_val),
             color = col_fuchsia, linetype = "dashed", linewidth = 1) +
  annotate("text", x = xbar + 0.002, y = 19,
           label = paste0("Theoretical SE = ", round(se_val, 4), " cm\n",
                          "Bootstrap SD   = ", round(se_boot, 4), " cm"),
           color = col_fuchsia, size = 3.5, hjust = 0, fontface = "bold") +
  labs(
    title    = "Sampling distribution of the mean (bootstrap, 5 000 samples)",
    subtitle = "The SE is the SD of this distribution: it quantifies uncertainty of the mean",
    x = "Sample mean (cm)", y = "Density",
    caption  = "Bootstrap resampling from Iris setosa, n = 50"
  ) +
  base_theme

# ============================================================
# PLOT 4 — Three-species comparison with SE (inferential use)
# ============================================================
df_sp <- do.call(data.frame,
  aggregate(Petal.Length ~ Species, data = iris,
    FUN = function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x)))))
names(df_sp) <- c("Species", "mean", "se")

p4 <- ggplot(df_sp, aes(x = Species, y = mean, fill = Species)) +
  geom_col(alpha = 0.80, width = 0.55) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.22, linewidth = 1.2, color = "grey25") +
  geom_point(size = 3.5, color = "grey15") +
  scale_fill_manual(values = c(
    "setosa"     = col_violet,
    "versicolor" = col_fuchsia,
    "virginica"  = col_violet_mid
  )) +
  labs(
    title    = "Mean \u00b1 SE of petal length across three Iris species",
    subtitle = "Correct inferential use of SE in comparative plots",
    x = NULL, y = "Petal length (cm)",
    caption  = "Error bars = \u00b11 SE  |  Data: iris (Fisher 1936)"
  ) +
  base_theme +
  theme(legend.position = "none")

# ============================================================
# SAVE
# ============================================================
ggsave("img/plot1_sd_distribution.png",    p1, width=8, height=5, dpi=150, bg="white")
ggsave("img/plot2_sd_vs_se.png",           p2, width=6, height=5, dpi=150, bg="white")
ggsave("img/plot3_bootstrap_se.png",       p3, width=8, height=5, dpi=150, bg="white")
ggsave("img/plot4_species_comparison.png", p4, width=8, height=5, dpi=150, bg="white")

message("\u2714 Plots saved in img/")
message("  - img/plot1_sd_distribution.png")
message("  - img/plot2_sd_vs_se.png")
message("  - img/plot3_bootstrap_se.png")
message("  - img/plot4_species_comparison.png")

# ============================================================
# Statistics for Life Sciences
# Notebook 02 — The Confidence Interval
# Script 05: Two visualisations
#
# Plot 1: 100 simulated experiments — frequentist definition
# Plot 2: n=8 vs n=80 — how width changes with sample size
#
# Dependencies: ggplot2, patchwork
# ============================================================

pkgs <- c("ggplot2", "patchwork")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(ggplot2)
library(patchwork)
if (!dir.exists("img")) dir.create("img")

# --- Green palette ---
col_green      <- "#1e6b45"   # dark green — CI that hits
col_green_mid  <- "#40916c"   # medium green
col_green_lt   <- "#74c69d"   # light green
col_miss       <- "#c0392b"   # red — CI that misses
col_zero       <- "#1a1a2e"   # near-black — true value / zero line
col_green_pale <- "#d8f3dc"   # pale green fill

base_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 9, color = "grey40"),
    plot.caption     = element_text(size = 8, color = "grey50", lineheight = 1.4),
    panel.grid.minor = element_blank()
  )

# ============================================================
# PLOT 1 — 100 simulated experiments
# ============================================================
set.seed(42)
true_diff <- 150
pop_sd    <- 90
n_sim     <- 40
n_exp     <- 100

sims <- do.call(rbind, lapply(seq_len(n_exp), function(i) {
  a <- rnorm(n_sim, mean = 4200,             sd = pop_sd)
  b <- rnorm(n_sim, mean = 4200 + true_diff, sd = pop_sd)
  tt <- t.test(b, a)
  data.frame(
    exp_id   = i,
    centre   = mean(b) - mean(a),
    ci_low   = tt$conf.int[1],
    ci_high  = tt$conf.int[2],
    contains = tt$conf.int[1] <= true_diff & true_diff <= tt$conf.int[2]
  )
}))

n_hits   <- sum(sims$contains)
n_misses <- n_exp - n_hits

p1 <- ggplot(sims, aes(
    y = exp_id, x = centre,
    xmin = ci_low, xmax = ci_high,
    color = contains
  )) +
  geom_errorbarh(height = 0, linewidth = 0.65, alpha = 0.85) +
  geom_point(size = 1.2, alpha = 0.9) +
  geom_vline(xintercept = true_diff,
             color = col_zero, linewidth = 1.2) +
  scale_color_manual(
    values = c("TRUE" = col_green, "FALSE" = col_miss),
    labels = c("TRUE"  = paste0("Contains true value  (n = ", n_hits, ")"),
               "FALSE" = paste0("Misses true value  (n = ", n_misses, ")"))
  ) +
  annotate("text", x = true_diff + 8, y = n_exp + 2,
           label = paste0("True effect = ", true_diff, " kg/ha"),
           color = col_zero, fontface = "bold", size = 3.3, hjust = 0) +
  labs(
    title    = "Frequentist definition of a 95% Confidence Interval",
    subtitle = paste0(n_exp, " simulated experiments  |  n = ", n_sim, " per group  |  True effect = ", true_diff, " kg/ha"),
    x       = "Estimated effect (kg/ha)",
    y       = "Experiment number",
    color   = NULL,
    caption = paste0(
      "The true effect (vertical line) is fixed — it never moves.\n",
      "The intervals move. ", n_hits, " out of ", n_exp,
      " contain the true value. ", n_misses, " miss it.\n",
      "This is what '95% confidence' means."
    )
  ) +
  base_theme +
  theme(
    plot.title      = element_text(color = col_green, face = "bold", size = 13),
    legend.position = "bottom",
    legend.text     = element_text(size = 9),
    panel.grid.major.y = element_blank()
  )

ggsave("img/plot_ci_definition.png",
       p1, width = 8, height = 10, dpi = 150, bg = "white")
message("\u2714 Saved: img/plot_ci_definition.png")

# ============================================================
# PLOT 2 — n=8 vs n=80: same difference, different width
# ============================================================
set.seed(99)

make_ci <- function(n, label) {
  a <- rnorm(n, mean = 4200,       sd = pop_sd)
  b <- rnorm(n, mean = 4200 + 150, sd = pop_sd)
  tt <- t.test(b, a)
  data.frame(
    label   = label,
    centre  = mean(b) - mean(a),
    ci_low  = tt$conf.int[1],
    ci_high = tt$conf.int[2]
  )
}

df_comp <- rbind(
  make_ci(4,  "4 fields per group\n(8 fields total)"),
  make_ci(40, "40 fields per group\n(80 fields total)")
)
df_comp$label <- factor(df_comp$label, levels = df_comp$label)
df_comp$contains_zero <- df_comp$ci_low < 0

p2 <- ggplot(df_comp, aes(
    x = label, y = centre,
    ymin = ci_low, ymax = ci_high,
    color = contains_zero
  )) +
  geom_hline(yintercept = 0,
             color = col_zero, linewidth = 0.9, linetype = "dashed") +
  geom_errorbar(width = 0.18, linewidth = 1.6) +
  geom_point(size = 5) +
  scale_color_manual(
    values = c("TRUE"  = col_miss,
               "FALSE" = col_green),
    labels = c("TRUE"  = "CI contains zero — no significant difference",
               "FALSE" = "CI does not contain zero — significant difference")
  ) +
  annotate("text", x = 2.38, y = 2,
           label = "zero line", color = col_zero,
           size = 3, fontface = "italic") +
  labs(
    title    = "More fields = narrower interval = better decision",
    subtitle = "Same observed difference (≈150 kg/ha). Different sample sizes.",
    x = NULL, y = "Estimated difference (kg/ha)",
    color = NULL,
    caption = "Error bars = 95% CI  |  Dashed line = zero (no effect)"
  ) +
  base_theme +
  theme(
    plot.title      = element_text(color = col_green, face = "bold", size = 13),
    legend.position = "bottom",
    legend.text     = element_text(size = 9),
    axis.text.x     = element_text(size = 10, lineheight = 1.3)
  )

ggsave("img/plot_ci_width_comparison.png",
       p2, width = 8, height = 6, dpi = 150, bg = "white")
message("\u2714 Saved: img/plot_ci_width_comparison.png")

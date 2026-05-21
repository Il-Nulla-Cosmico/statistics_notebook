# ============================================================
# Statistics for Life Sciences
# Notebook 02 — The Confidence Interval
# Script 04: Calculation of CI from two groups
# Dataset: fertiliser yield experiment (8 fields)
# ============================================================

group_A <- c(4100, 4250, 4150, 4300)
group_B <- c(4400, 4300, 4450, 4250)

n_A  <- length(group_A); n_B <- length(group_B)
sd_A <- sd(group_A);     sd_B <- sd(group_B)

diff_obs <- mean(group_B) - mean(group_A)
se_diff  <- sqrt(sd_A^2 / n_A + sd_B^2 / n_B)
df       <- n_A + n_B - 2
t_crit   <- qt(0.975, df = df)   # two-tailed, 95%

ci_low  <- diff_obs - t_crit * se_diff
ci_high <- diff_obs + t_crit * se_diff

cat("=== Fertiliser yield experiment ===\n\n")
cat(sprintf("  Group A mean      : %.1f kg/ha\n", mean(group_A)))
cat(sprintf("  Group B mean      : %.1f kg/ha\n", mean(group_B)))
cat(sprintf("  Observed diff     : %.1f kg/ha\n", diff_obs))
cat(sprintf("  SD group A        : %.2f\n",        sd_A))
cat(sprintf("  SD group B        : %.2f\n",        sd_B))
cat(sprintf("  SE of difference  : %.2f\n",        se_diff))
cat(sprintf("  df                : %d\n",            df))
cat(sprintf("  t critical (2-t)  : %.3f\n",        t_crit))
cat(sprintf("  95%% CI            : [%.1f — %.1f] kg/ha\n", ci_low, ci_high))
cat("\n")

# Confirm with Student's t.test (assuming equal variances)
result <- t.test(group_B, group_A, var.equal = TRUE)
cat("=== Confirmed via t.test() ===\n\n")
cat(sprintf("  95%% CI            : [%.1f — %.1f] kg/ha\n",
            result$conf.int[1], result$conf.int[2]))
cat(sprintf("  t statistic       : %.3f\n", result$statistic))
cat(sprintf("  degrees of freedom: %.1f\n", result$parameter))
cat(sprintf("  p-value           : %.4f\n", result$p.value))
cat(sprintf("  CI contains zero? : %s\n",
            ifelse(result$conf.int[1] < 0, "YES — no significant difference", "NO — significant difference")))
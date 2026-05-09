# ============================================================
# Statistics for Life Sciences
# Lesson 01 — Standard Deviation vs Standard Error
# Script 01: Calculation of SD and SE
# Dataset: iris — Iris setosa, Petal.Length
# ============================================================

# Load built-in dataset
data(iris)

# Extract: Iris setosa, petal length
setosa_pl <- iris[iris$Species == "setosa", "Petal.Length"]

# --- Basic parameters ---
n    <- length(setosa_pl)       # sample size
xbar <- mean(setosa_pl)         # sample mean

# --- Standard Deviation (SD) ---
# sd() uses Bessel's correction (n-1): unbiased estimator
sd_val <- sd(setosa_pl)

# Manual verification (should return TRUE)
sd_manual <- sqrt(sum((setosa_pl - xbar)^2) / (n - 1))
cat("Manual SD == sd():", isTRUE(all.equal(sd_val, sd_manual)), "\n\n")

# --- Standard Error of the Mean (SE) ---
# No dedicated function in base R — calculated as follows:
se_val <- sd_val / sqrt(n)

# --- 95% Confidence Interval ---
# For small samples, use the t distribution (df = n - 1)
t_crit  <- qt(0.975, df = n - 1)
ci_low  <- xbar - t_crit * se_val
ci_high <- xbar + t_crit * se_val

# --- Summary output ---
cat("=== Iris setosa — Petal Length (cm) ===\n\n")
cat(sprintf("  Sample size (n)                : %d\n", n))
cat(sprintf("  Sample mean (x-bar)            : %.4f cm\n", xbar))
cat(sprintf("  Standard Deviation (SD)        : %.4f cm\n", sd_val))
cat(sprintf("  Standard Error (SE)            : %.4f cm\n", se_val))
cat(sprintf("  SD / SE ratio                  : %.4f  (expected: sqrt(%d) = %.4f)\n",
            sd_val / se_val, n, sqrt(n)))
cat("\n")
cat(sprintf("  Mean +/- SD                    : [%.4f  -  %.4f] cm\n",
            xbar - sd_val, xbar + sd_val))
cat(sprintf("  Mean +/- SE                    : [%.4f  -  %.4f] cm\n",
            xbar - se_val, xbar + se_val))
cat(sprintf("  95%% CI (t Student, df = %d)   : [%.4f  -  %.4f] cm\n",
            n - 1, ci_low, ci_high))
cat("\n")
cat("Interpretation:\n")
cat(sprintf("  - SD (%.3f cm) describes biological variability:\n", sd_val))
cat(sprintf("    ~68%% of petals measure between %.3f and %.3f cm.\n",
            xbar - sd_val, xbar + sd_val))
cat(sprintf("  - SE (%.4f cm) describes precision of the mean estimate:\n", se_val))
cat(sprintf("    the true population mean falls with 95%% confidence\n"))
cat(sprintf("    in the interval [%.4f - %.4f] cm.\n", ci_low, ci_high))

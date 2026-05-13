# ============================================================
# Statistics for Life Sciences
# Notebook 01 — Standard Deviation vs Standard Error
# Script 01: Calculation of SD and SE
# Dataset: iris — Iris setosa, Petal.Length
# ============================================================

data(iris)
setosa_pl <- iris[iris$Species == "setosa", "Petal.Length"]

n    <- length(setosa_pl)
xbar <- mean(setosa_pl)
sd_val <- sd(setosa_pl)          # Bessel's correction (n-1)
se_val <- sd_val / sqrt(n)       # no built-in function in base R

t_crit  <- qt(0.975, df = n - 1)
ci_low  <- xbar - t_crit * se_val
ci_high <- xbar + t_crit * se_val

cat("=== Iris setosa — Petal Length (cm) ===\n\n")
cat(sprintf("  n                    : %d\n",    n))
cat(sprintf("  Mean                 : %.4f cm\n", xbar))
cat(sprintf("  SD                   : %.4f cm\n", sd_val))
cat(sprintf("  SE                   : %.4f cm\n", se_val))
cat(sprintf("  SD / SE ratio        : %.2f  (= sqrt(%d) = %.2f)\n",
            sd_val/se_val, n, sqrt(n)))
cat(sprintf("  Mean +/- SD          : [%.4f - %.4f] cm\n",
            xbar-sd_val, xbar+sd_val))
cat(sprintf("  Mean +/- SE          : [%.4f - %.4f] cm\n",
            xbar-se_val, xbar+se_val))
cat(sprintf("  95%% CI (t, df=%d)   : [%.4f - %.4f] cm\n",
            n-1, ci_low, ci_high))

cat("\nInterpretation:\n")
cat(sprintf("  SD (%.3f cm): ~68%% of petals fall between %.3f and %.3f cm\n",
            sd_val, xbar-sd_val, xbar+sd_val))
cat(sprintf("  SE (%.4f cm): the true mean falls within [%.4f - %.4f] cm (95%% CI)\n",
            se_val, ci_low, ci_high))

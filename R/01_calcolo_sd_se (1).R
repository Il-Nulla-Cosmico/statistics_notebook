# ============================================================
# Statistica per le Scienze della Vita
# Lezione 01 — Deviazione Standard vs Errore Standard
# Script 01: Calcolo di SD e SE
# Dataset: iris — Iris setosa, Petal.Length
# ============================================================

# Caricamento dataset integrato in R
data(iris)

# Estrazione: Iris setosa, lunghezza del petalo
setosa_pl <- iris[iris$Species == "setosa", "Petal.Length"]

# --- Parametri base ---
n     <- length(setosa_pl)        # numerosita' campionaria
media <- mean(setosa_pl)          # media campionaria

# --- Deviazione Standard (SD) ---
# sd() usa la correzione di Bessel (n-1): stimatore non distorto
sd_val <- sd(setosa_pl)

# Verifica manuale (deve dare TRUE)
sd_manuale <- sqrt(sum((setosa_pl - media)^2) / (n - 1))
cat("SD manuale == sd():", isTRUE(all.equal(sd_val, sd_manuale)), "\n\n")

# --- Errore Standard della Media (SE) ---
# Non esiste una funzione dedicata in R base: SI CALCOLA COSI'
se_val <- sd_val / sqrt(n)

# --- Intervallo di Confidenza al 95% ---
# Per campioni piccoli si usa la distribuzione t di Student (df = n-1)
t_critico <- qt(0.975, df = n - 1)
ic_low    <- media - t_critico * se_val
ic_high   <- media + t_critico * se_val

# --- Output riassuntivo ---
cat("=== Iris setosa — Petal.Length (cm) ===\n\n")
cat(sprintf("  n                              : %d\n", n))
cat(sprintf("  Media (x-barra)                : %.4f cm\n", media))
cat(sprintf("  Deviazione Standard (SD)       : %.4f cm\n", sd_val))
cat(sprintf("  Errore Standard (SE)           : %.4f cm\n", se_val))
cat(sprintf("  Rapporto SD/SE                 : %.4f  (atteso: sqrt(%d) = %.4f)\n",
            sd_val / se_val, n, sqrt(n)))
cat("\n")
cat(sprintf("  Media +/- SD                   : [%.4f  -  %.4f] cm\n",
            media - sd_val, media + sd_val))
cat(sprintf("  Media +/- SE                   : [%.4f  -  %.4f] cm\n",
            media - se_val, media + se_val))
cat(sprintf("  IC 95%% (t Student, df = %d)   : [%.4f  -  %.4f] cm\n",
            n - 1, ic_low, ic_high))
cat("\n")
cat("Interpretazione:\n")
cat(sprintf("  - La SD (%.3f cm) descrive la variabilita' biologica:\n", sd_val))
cat(sprintf("    circa il 68%% dei petali misura tra %.3f e %.3f cm.\n",
            media - sd_val, media + sd_val))
cat(sprintf("  - La SE (%.4f cm) descrive la precisione della stima della media:\n", se_val))
cat(sprintf("    la vera media della popolazione cade con 95%% di confidenza\n"))
cat(sprintf("    nell'intervallo [%.4f - %.4f] cm.\n", ic_low, ic_high))

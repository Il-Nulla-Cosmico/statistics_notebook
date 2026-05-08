# ============================================================
# Statistica per le Scienze della Vita
# Lezione 01 — Deviazione Standard vs Errore Standard
# Script 02: Visualizzazione grafica
# Dataset: iris — Iris setosa, Petal.Length
# Dipendenze: ggplot2
# ============================================================

# Installa ggplot2 se non presente
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

# Crea cartella output se non esiste
if (!dir.exists("img")) dir.create("img")

# --- Dati ---
data(iris)
setosa_pl <- iris[iris$Species == "setosa", "Petal.Length"]
n      <- length(setosa_pl)
media  <- mean(setosa_pl)
sd_val <- sd(setosa_pl)
se_val <- sd_val / sqrt(n)
t_cr   <- qt(0.975, df = n - 1)
ic_low <- media - t_cr * se_val
ic_hi  <- media + t_cr * se_val
df     <- data.frame(x = setosa_pl)

# Colori coerenti con il tema della serie
col_sd  <- "#C0623C"   # ruggine — SD
col_se  <- "#2d6a4f"   # verde   — SE
col_med <- "#1A1A2E"   # quasi nero — media
col_ic  <- "#1565c0"   # blu — IC 95%

tema_base <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    plot.caption  = element_text(size = 8,  color = "grey55")
  )

# ============================================================
# GRAFICO 1 — Distribuzione individuale + SD
# ============================================================
p1 <- ggplot(df, aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = 0.1, fill = "#a8d8b0", color = "white", alpha = 0.85) +
  geom_density(color = col_sd, linewidth = 1.2) +
  geom_vline(xintercept = media,
             color = col_med, linewidth = 1.2) +
  geom_vline(xintercept = c(media - sd_val, media + sd_val),
             color = col_sd, linetype = "dashed", linewidth = 1) +
  annotate("segment",
           x = media - sd_val, xend = media + sd_val,
           y = 2.5, yend = 2.5,
           color = col_sd, linewidth = 1.2,
           arrow = arrow(ends = "both", length = unit(0.2, "cm"))) +
  annotate("text", x = media, y = 2.78,
           label = paste0("SD = \u00b1", round(sd_val, 3), " cm"),
           fontface = "bold", color = col_sd, size = 3.8) +
  annotate("text", x = media + 0.01, y = 3.3,
           label = paste0("\u0078\u0305 = ", round(media, 3), " cm"),
           fontface = "bold", color = col_med, size = 3.8, hjust = 0) +
  labs(
    title    = "Distribuzione dei valori individuali — Iris setosa, Petal.Length",
    subtitle = "La SD descrive la dispersione delle singole osservazioni intorno alla media",
    x = "Lunghezza petalo (cm)", y = "Densita'",
    caption  = "Dati: iris (Fisher 1936)"
  ) +
  tema_base

# ============================================================
# GRAFICO 2 — SD vs SE a confronto (barre d'errore)
# ============================================================
df_err <- data.frame(
  indice = factor(c("Media \u00b1 SD", "Media \u00b1 SE", "IC 95%"),
                  levels = c("Media \u00b1 SD", "Media \u00b1 SE", "IC 95%")),
  media  = rep(media, 3),
  lower  = c(media - sd_val, media - se_val, ic_low),
  upper  = c(media + sd_val, media + se_val, ic_hi)
)
p2 <- ggplot(df_err, aes(x = indice, y = media, color = indice,
                          ymin = lower, ymax = upper)) +
  geom_point(size = 4.5) +
  geom_errorbar(width = 0.18, linewidth = 1.4) +
  scale_color_manual(values = c(
    "Media \u00b1 SD" = col_sd,
    "Media \u00b1 SE" = col_se,
    "IC 95%"         = col_ic
  )) +
  labs(
    title    = "Confronto visivo: SD, SE e IC 95%",
    subtitle = "Tutti e tre si misurano in cm, ma descrivono cose diverse",
    x = NULL, y = "Lunghezza petalo (cm)",
    caption  = "Dati: iris (Fisher 1936)"
  ) +
  tema_base +
  theme(legend.position = "none")

# ============================================================
# GRAFICO 3 — Distribuzione campionaria (bootstrap)
# ============================================================
set.seed(42)
medie_sim <- replicate(5000, mean(sample(setosa_pl, n, replace = TRUE)))
se_boot   <- sd(medie_sim)

p3 <- ggplot(data.frame(m = medie_sim), aes(x = m)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = "#a8d8c0", color = "white", alpha = 0.85) +
  geom_density(color = col_se, linewidth = 1.3) +
  geom_vline(xintercept = media,
             color = col_med, linewidth = 1.2) +
  geom_vline(xintercept = c(media - se_val, media + se_val),
             color = col_se, linetype = "dashed", linewidth = 1) +
  annotate("text", x = media + 0.002, y = 19,
           label = paste0("SE teorica = ", round(se_val, 4), " cm\n",
                          "SD bootstrap = ", round(se_boot, 4), " cm"),
           color = col_se, size = 3.5, hjust = 0, fontface = "bold") +
  labs(
    title    = "Distribuzione campionaria delle medie (bootstrap, 5000 campioni)",
    subtitle = "La SE e' la SD di questa distribuzione: misura l'incertezza della media",
    x = "Media campionaria (cm)", y = "Densita'",
    caption  = "Campionamento con rimpiazzo da Iris setosa, n = 50"
  ) +
  tema_base

# ============================================================
# GRAFICO 4 — Confronto tre specie con barre SE (uso inferenziale)
# ============================================================
df_sp <- do.call(data.frame,
  aggregate(Petal.Length ~ Species, data = iris,
    FUN = function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x)))))
names(df_sp) <- c("Species", "mean", "se")

p4 <- ggplot(df_sp, aes(x = Species, y = mean, fill = Species)) +
  geom_col(alpha = 0.78, width = 0.55) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.22, linewidth = 1.2, color = "grey25") +
  geom_point(size = 3.5, color = "grey15") +
  scale_fill_manual(values = c(
    "setosa"     = "#2d6a4f",
    "versicolor" = "#C0623C",
    "virginica"  = "#6b8cba"
  )) +
  labs(
    title    = "Media \u00b1 SE della lunghezza del petalo — tre specie di Iris",
    subtitle = "Uso corretto della SE nei grafici comparativi tra gruppi",
    x = NULL, y = "Lunghezza petalo (cm)",
    caption  = "Barre d'errore = \u00b11 SE  |  Dati: iris (Fisher 1936)"
  ) +
  tema_base +
  theme(legend.position = "none")

# ============================================================
# SALVATAGGIO
# ============================================================
ggsave("img/grafico1_distribuzione_sd.png", p1, width = 8, height = 5, dpi = 150, bg = "white")
ggsave("img/grafico2_sd_vs_se.png",         p2, width = 6, height = 5, dpi = 150, bg = "white")
ggsave("img/grafico3_bootstrap_se.png",     p3, width = 8, height = 5, dpi = 150, bg = "white")
ggsave("img/grafico4_confronto_specie.png", p4, width = 8, height = 5, dpi = 150, bg = "white")

message("\n\u2714 Grafici salvati nella cartella img/")
message("  - img/grafico1_distribuzione_sd.png")
message("  - img/grafico2_sd_vs_se.png")
message("  - img/grafico3_bootstrap_se.png")
message("  - img/grafico4_confronto_specie.png")

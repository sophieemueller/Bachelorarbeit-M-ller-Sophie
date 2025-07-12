rm(list = ls())
dev.off()
gc()
# Umfrage einlesen
df <- read.csv("df.csv", sep = ";")
names(df)
head(df)

# Umkodierung von Items, bei denen Skala von 6–10 auf 1–5 zurückgesetzt werden muss
df$v_22 <- df$v_22 - 5
df$v_73 <- df$v_73 - 5
df$v_97 <- df$v_97 - 5
# Kontrolle nach Umkodierung
# Skalen-Items definieren
rses_items <- c("v_12", "v_85", "v_86", "v_87", "v_88", "v_89", "v_90", "v_91", "v_92", "v_93")
rses_invert <- c("v_85", "v_88", "v_89", "v_91", "v_92")

sseit_items <- c("v_13", "v_14", "v_15", "v_16", "v_17", "v_18", "v_19", "v_20",
                 "v_21", "v_22", "v_23", "v_24", "v_25", "v_26", "v_27", "v_28",
                 "v_29", "v_30", "v_31", "v_32", "v_33", "v_34", "v_35", "v_36",
                 "v_37", "v_38", "v_39", "v_40", "v_41", "v_42", "v_43", "v_44", "v_45")
sseit_invert <- c("v_17", "v_40", "v_45")

ffqm_items <- c("v_46", "v_47", "v_48", "v_49", "v_50", "v_51", "v_52", "v_53",
                "v_54", "v_55", "v_56", "v_57", "v_58", "v_59", "v_60", "v_61",
                "v_62", "v_63", "v_64", "v_65", "v_66", "v_67", "v_68", "v_69",
                "v_70", "v_71", "v_72", "v_73", "v_74", "v_75", "v_76", "v_77",
                "v_78", "v_79", "v_80", "v_81", "v_82", "v_83", "v_84")
ffqm_invert <- c("v_48", "v_50", "v_53", "v_55", "v_58", "v_59",
                 "v_62", "v_63", "v_67", "v_68", "v_70", "v_73", "v_75", "v_79",
                 "v_80", "v_83", "v_84")
# Umkodieren
reverse_items <- function(df, vars, max_scale) {
  df[vars] <- lapply(df[vars], function(x) max_scale + 1 - x)
  return(df)
}

df <- reverse_items(df, rses_invert, 4)
df <- reverse_items(df, sseit_invert, 5)
df <- reverse_items(df, ffqm_invert, 5)
# Daten bereinigen
# Alter zwischen 18 und 60
df_clean <- df[df$v_112 >= 18 & df$v_112 <= 60, ]

# Nur gute Deutschkenntnisse (1–3)
df_clean <- df_clean[df_clean$v_111 <= 3, ]

# Unvollständige Antworten ausschließen
df_clean <- df_clean[complete.cases(df_clean), ]
sum(!complete.cases(df_clean))  # Kontrolle
# Qualitätsprüfung 1: SD < 0.3
df_clean$sd_rses <- apply(df_clean[rses_items], 1, sd)
df_clean$sd_sseit <- apply(df_clean[sseit_items], 1, sd)
df_clean$sd_ffmq <- apply(df_clean[ffqm_items], 1, sd)

df_clean <- df_clean[df_clean$sd_rses >= 0.3 &
                       df_clean$sd_sseit >= 0.3 &
                       df_clean$sd_ffmq >= 0.3, ]

# Qualitätsprüfung 2: >90 % gleiche Antwort
mode_prop <- function(x) {
  freq <- table(x)
  max(freq) / length(x)
}

df_clean$mode_rses  <- apply(df_clean[rses_items], 1, mode_prop)
df_clean$mode_sseit <- apply(df_clean[sseit_items], 1, mode_prop)
df_clean$mode_ffmq  <- apply(df_clean[ffqm_items], 1, mode_prop)

df_clean <- df_clean[df_clean$mode_rses <= 0.9 &
                       df_clean$mode_sseit <= 0.9 &
                       df_clean$mode_ffmq <= 0.9, ]

# Hypothese 1: Es besteht ein positiver Zusammenhang 
# zwischen emotionaler Intelligenz (SSEIT) und Selbstwert (RSES).
#
# Ziel: Pearson-Korrelation (sofern Normalverteilung gegeben ist)
# - Skalenmittelwerte: score_sseit & score_rses
# - Voraussetzungen:
#     - Beide Variablen metrisch
#     - Beide normalverteilt (→ Shapiro-Wilk-Test)
#     - Lineare Beziehung (→ Streudiagramm)
#
# Tests:
# - Pearson-Korrelation: wenn Normalverteilung gegeben
# - Spearman-Korrelation: wenn keine Normalverteilung
# --------------------------------------------------
# Schritt 1: Skalenmittelwerte berechnen
df_clean$score_sseit <- rowMeans(df_clean[sseit_items], na.rm = TRUE)
df_clean$score_rses  <- rowMeans(df_clean[rses_items], na.rm = TRUE)
head(df_clean$score_sseit)
head(df_clean$score_rses)
# Z-Scores berechnen
z_sseit <- scale(df_clean$score_sseit)
z_rses  <- scale(df_clean$score_rses)
# Ausreißer anzeigen (Z > 3 oder < -3)
which(abs(z_sseit) > 3)
which(abs(z_rses) > 3)
# Schritt 2: Voraussetzungen prüfen
shapiro.test(df_clean$score_sseit)
shapiro.test(df_clean$score_rses)
# QQ-Plot für Selbstwert erstellen
qqnorm(df_clean$score_rses, main = "QQ-Plot: Selbstwert (RSES)")
qqline(df_clean$score_rses, col = "blue")
# Q-Q-Plot für emotionale Intelligenz (SSEIT)
qqnorm(df_clean$score_sseit, main = "Q-Q-Plot: Emotionale Intelligenz (SSEIT)")
qqline(df_clean$score_sseit, col = "red")
# --------------------------------------------------
# -----------------------------------------------------------
# NORMALVERTEILUNGSPRÜFUNG – Hypothese 1 (SSEIT & RSES)
# -----------------------------------------------------------
# Ziel: Überprüfung der Normalverteilung für die beiden Skalen 
#       - SSEIT (Emotionale Intelligenz)
#       - RSES  (Selbstwert)
#
# Methode: Shapiro-Wilk-Test und QQ-Plots
#
# Ergebnis:
# - SSEIT (p = 0.136): Normalverteilung gegeben 
# - RSES  (p = 2.801e-08): Keine Normalverteilung 
#
# Ergänzend bestätigen auch die QQ-Plots:
# - SSEIT: Punkte liegen relativ gleichmäßig auf der Diagonale
# - RSES: Deutliche Abweichungen, besonders im rechten Bereich
#
# Fazit:
# → Für Hypothese 1 ist **keine Pearson-Korrelation** geeignet.
# → Stattdessen wurde korrekt die **Spearman-Korrelation** verwendet.
# -----------------------------------------------------------
# Schritt 2: Korrelation berechnen
# Falls KEINE Normalverteilung (→ Spearman-Rangkorrelation)
cor.test(df_clean$score_sseit, df_clean$score_rses, method = "spearman")
# Hypothese 1: Es besteht ein signifikanter Zusammenhang zwischen emotionaler Intelligenz und Selbstwert
# Test: Spearman-Rangkorrelation, da keine Normalverteilung (Shapiro-Wilk-Test p < .05)
# Ergebnis: rho = 0.48, p < .001
# Interpretation: Es besteht ein mittlerer bis starker positiver Zusammenhang zwischen EI und Selbstwert.
# Z-Scores berechnen
z_sseit <- scale(df_clean$score_sseit)
z_rses  <- scale(df_clean$score_rses)

# Ausreißer anzeigen (Z > 3 oder < -3)
which(abs(z_sseit) > 3)
which(abs(z_rses) > 3)
# Z-Scores für die beiden Variablen
df_clean$z_sseit <- scale(df_clean$score_sseit)
df_clean$z_rses  <- scale(df_clean$score_rses)

# Neue Version ohne Ausreißer (Z > |3|)
df_no_outliers <- df_clean[abs(df_clean$z_sseit) <= 3 & abs(df_clean$z_rses) <= 3, ]
cor.test(df_clean$score_sseit, df_clean$score_rses, method = "spearman")
cor.test(df_no_outliers$score_sseit, df_no_outliers$score_rses, method = "spearman")
# -----------------------------------------
# Sensitivitätsanalyse für Hypothese 1
# -----------------------------------------
# Zur Prüfung der Robustheit des Zusammenhangs zwischen Emotionaler Intelligenz (SSEIT)
# und Selbstwert (RSES) wurde eine Sensitivitätsanalyse durchgeführt, bei der Ausreißer
# (Z > |3|) entfernt wurden. Der Rangkorrelationskoeffizient veränderte sich dabei
# nur minimal (rho = .48), und die statistische Signifikanz blieb bestehen (p < .001).
# Die Hypothese ist somit robust gegenüber Ausreißern.
# -----------------------------------------
# Visualisierung Hypothese 1: EI vs. Selbstwert
# ----------------------------------------
library(ggplot2)

ggplot(df_clean, aes(x = score_sseit, y = score_rses)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Zusammenhang zwischen Emotionaler Intelligenz und Selbstwert",
       x = "Emotionale Intelligenz (SSEIT)",
       y = "Selbstwert (RSES)") +
  theme_minimal() 
# -----------------------------------------
# HYPOTHESE 2: Frauen vs. Männer in EI (SSEIT)
# -----------------------------------------
---------------------------------------------
  # SCHRITT 1: Normalverteilung pro Gruppe prüfen
  # --------------------------------------------
shapiro.test(df_clean$score_sseit[df_clean$Geschlecht == 1])  # weiblich
shapiro.test(df_clean$score_sseit[df_clean$Geschlecht == 2])  # männlich
# NORMALVERTEILUNGSPRÜFUNG – Hypothese 2 (EI ~ Geschlecht)
# Für größere Stichproben: Visuelle Überprüfung
# ----------------------------------------------
# QQ-Plot: Frauen
qqnorm(df_clean$score_sseit[df_clean$Geschlecht == 1],
       main = "QQ-Plot: Emotionale Intelligenz (Frauen)")
qqline(df_clean$score_sseit[df_clean$Geschlecht == 1], col = "red", lwd = 2)

# QQ-Plot: Männer
qqnorm(df_clean$score_sseit[df_clean$Geschlecht == 2],
       main = "QQ-Plot: Emotionale Intelligenz (Männer)")
qqline(df_clean$score_sseit[df_clean$Geschlecht == 2], col = "blue", lwd = 2)
# ---------------------------------------------
# Normalverteilungsprüfung – Hypothese 2 (EI ~ Geschlecht)
# Für größere Stichproben: Visuelle Überprüfung via QQ-Plot
# Interpretation:
# Die Punkte in beiden QQ-Plots (Frauen & Männer) liegen überwiegend entlang der Diagonalen.
# → Visuell ist die Normalverteilung in beiden Gruppen als gegeben anzunehmen.
# → Vor allem bei größeren Stichproben ist diese Methode zulässig.
# Die Annahme der Normalverteilung wird daher beibehalten.
# ---------------------------------------------
# ---------------------------------------------
# SCHRITT 2: Varianzhomogenität prüfen (nur wenn normalverteilt)
# --------------------------------------------
library(car)
leveneTest(score_sseit ~ as.factor(Geschlecht), data = df_clean)
sum(!complete.cases(df_clean[, c("score_sseit", "Geschlecht")]))
table(df_clean$Geschlecht, useNA = "always")
# Voraussetzungen: Normalverteilung + Varianzhomogenität erfüllt
# → Daher: Unabhängiger t-Test mit "var.equal = TRUE"
# -------------------------------------------------------
# Mittelwert und Standardabweichung getrennt für Männer und Frauen
tapply(df_clean$score_sseit, df_clean$Geschlecht, sd)
# Mittelwerte vergleichen
t.test(score_sseit ~ Geschlecht, data = df_clean, var.equal = TRUE)
# -----------------------------------------
# HYPOTHESE 2: Ergebnisinterpretation
# -----------------------------------------

# Der p-Wert beträgt 0.046 → also p < 0.05
# → Damit liegt ein statistisch signifikanter Unterschied vor.

# Die Mittelwerte zeigen:
# Frauen (Gruppe 1): M = 3.69
# Männer (Gruppe 2): M = 3.59

# Interpretation des T-Tests: Emotionalen Intelligenz nach Geschlecht
# Der Unterschied in der emotionalen Intelligenz zwischen den Gruppen ist signifikant (t(277) = 2.01, p = 0.046).
# Gruppe 1 (z.B. Frauen) zeigt im Mittel höhere Werte (M = 3.69) als Gruppe 2 (Männer, M = 3.59)
# → Die Hypothese eines Geschlechtsunterschieds in emotionaler Intelligenz wird unterstützt.
# Visualisierung
#    → Boxplot für emotionale Intelligenz nach Geschlecht:
library(ggplot2)

ggplot(df_clean, aes(x = factor(Geschlecht, labels = c("weiblich", "männlich")), 
                     y = score_sseit, fill = factor(Geschlecht))) +
  geom_boxplot(width = 0.5, alpha = 0.7, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black") +
  labs(title = "Emotionale Intelligenz nach Geschlecht",
       x = "Geschlecht",
       y = "SSEIT-Score") +
  scale_fill_manual(values = c("#FFB6C1", "#ADD8E6")) +
  theme_minimal() +
  theme(legend.position = "none")

# -----------------------------------------------------------
# HYPOTHESE 3: Zusammenhang Fachsemester & Emotionale Intelligenz
# -----------------------------------------------------------
# Ziel: Prüfung, ob Studierende mit höherem Fachsemester höhere emotionale Intelligenz (SSEIT) zeigen.
# Datengrundlage: Nur Personen mit Abitur (Bildungsgrad == 4) UND aktueller Student (Beruf == 3)
# Zusätzlich: Fachsemester muss gültig sein (also > 0)
table(df_clean$Bildungsgrad)   
table(df_clean$Beruf)         
summary(df_clean$V8)     
names(df_clean)
# Q-Q-Plot für emotionale Intelligenz (SSEIT) bei Bachelor-Studierenden
df_bachelor <- df_clean[
  df_clean$Bildungsgrad == 4 &
    df_clean$Beruf == 3 &
    df_clean$v_8 > 0,
]
df_bachelor$fachsemester <- df_bachelor$v_8
library(ggplot2)

ggplot(df_bachelor, aes(sample = score_sseit)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(
    title = "Q-Q-Plot: Emotionale Intelligenz bei Bachelor-Studierenden",
    x = "Theoretische Quantile",
    y = "Beobachtete Quantile"
  ) +
  theme_minimal()
library(ggplot2)

ggplot(df_bachelor, aes(x = fachsemester, y = score_sseit)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(
    title = "Monotoner Zusammenhang: Fachsemester und emotionale Intelligenz",
    x = "Fachsemester",
    y = "Emotionale Intelligenz (SSEIT)"
  ) +
  theme_minimal()
#Ausreißer H3
# 1. Z-Werte berechnen
df_bachelor$z_sseit <- scale(df_bachelor$score_sseit)
df_bachelor$fachsemester <- as.numeric(df_bachelor$v_8)  
df_bachelor$z_fachsemester <- scale(df_bachelor$fachsemester)

# 2. Ausreißer anzeigen (Z > ±3)
which(abs(df_bachelor$z_sseit) > 3)
which(abs(df_bachelor$z_fachsemester) > 3)
# Schritt 1: Stichprobe filtern
# Schritt 2: Variablen für Analyse vorbereiten
score_sseit_bachelor <- df_bachelor$score_sseit
fachsemester <- df_bachelor$v_8
nrow(df_bachelor)
# Schritt 3: Voraussetzungen prüfen
# → Beide Variablen ordinal/metrisch → Korrelationsanalyse möglich
# → Normalverteilung bei SSEIT wurde bereits geprüft (OK)
# → Fachsemester ist ordinal → daher sicherheitshalber Spearman-Korrelation
# Schritt 4: Spearman-Korrelation berechnen
#Ausreißer H3
# 1. Z-Werte berechnen
df_bachelor$z_sseit <- scale(df_bachelor$score_sseit)
df_bachelor$fachsemester <- as.numeric(df_bachelor$v_8)  # Umwandlung, falls nötig
df_bachelor$z_fachsemester <- scale(df_bachelor$fachsemester)

# 2. Ausreißer anzeigen (Z > ±3)
which(abs(df_bachelor$z_sseit) > 3)
which(abs(df_bachelor$z_fachsemester) > 3)
cor.test(score_sseit_bachelor, fachsemester, method = "spearman")
# -----------------------------------------
# HYPOTHESE 3: Fachsemester & Emotionale Intelligenz (SSEIT)
# -----------------------------------------
# Ergebnis: rho = -0.022, p = 0.8191
# → Kein signifikanter Zusammenhang zwischen Fachsemester und emotionaler Intelligenz.
# → Der Zusammenhang ist sehr schwach negativ, aber statistisch nicht bedeutsam.
# -----------------------------------------
# Visualisierung Hypothese 3: Fachsemester vs. Emotionale Intelligenz
# -----------------------------------------
# QQ-Plot
ggplot(df_bachelor, aes(sample = fachsemester)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(title = "Q-Q-Plot: Fachsemester",
       x = "Theoretische Quantile",
       y = "Beobachtete Quantile") +
  theme_minimal()
library(ggplot2)
library(ggplot2)

ggplot(df_bachelor, aes(x = fachsemester, y = score_sseit)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Zusammenhang zwischen Fachsemester und Emotionaler Intelligenz",
       x = "Fachsemester",
       y = "Emotionale Intelligenz (SSEIT)") +
  theme_minimal()
# Q-Q-Plot für emotionale Intelligenz (SSEIT) bei Bachelor-Studierenden
library(ggplot2)

ggplot(df_bachelor, aes(sample = score_sseit)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(
    title = "Q-Q-Plot: Emotionale Intelligenz bei Bachelor-Studierenden",
    x = "Theoretische Quantile",
    y = "Beobachtete Quantile"
  ) +
  theme_minimal()
library(ggplot2)

ggplot(df_bachelor, aes(x = fachsemester, y = score_sseit)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(
    title = "Monotoner Zusammenhang: Fachsemester und emotionale Intelligenz",
    x = "Fachsemester",
    y = "Emotionale Intelligenz (SSEIT)"
  ) +
  theme_minimal()
# Interpretation des Scatterplots (Hypothese 3)
# -----------------------------------------
# Der Plot zeigt die Beziehung zwischen Fachsemester und emotionaler Intelligenz (SSEIT).
# Die Punkte sind gleichmäßig verteilt und die Regressionslinie ist nahezu flach.
# → Es liegt kein erkennbarer Zusammenhang vor.
# → Die visuelle Darstellung unterstützt das Ergebnis des Spearman-Tests (nicht signifikant).
# -----------------------------------------
# HYPOTHESE 4: Mediationsanalyse
# Emotionale Intelligenz mediiert den Zusammenhang zwischen Achtsamkeit und Selbstwert
# -----------------------------------------
# -----------------------------------------
# Schritt 1: Mittelwerte berechnen (falls noch nicht vorhanden)
# -----------------------------------------
df_clean$score_ffmq  <- rowMeans(df_clean[ffqm_items],  na.rm = TRUE)
df_clean$score_sseit <- rowMeans(df_clean[sseit_items], na.rm = TRUE)
df_clean$score_rses  <- rowMeans(df_clean[rses_items],  na.rm = TRUE)
# Paket installieren (nur einmal nötig)
install.packages("lavaan")
# Paket laden
library(lavaan)
# Mittelwerte berechnen – korrekt im Datensatz df_clean
ffmq_items <- ffqm_items
df_clean$score_ffmq   <- rowMeans(df_clean[ffmq_items], na.rm = TRUE)  # Achtsamkeit
df_clean$score_sseit  <- rowMeans(df_clean[sseit_items], na.rm = TRUE) # Emotionale Intelligenz
df_clean$score_rses   <- rowMeans(df_clean[rses_items], na.rm = TRUE)  # Selbstwert

# -----------------------------------------
# QQ-Plot für Achtsamkeit
# -----------------------------------------
qqnorm(df_clean$score_ffmq, col = "red")
qqline(df_clean$score_ffmq, col = "red", lwd = 2)
# ➤ Visuelle Prüfung der Normalverteilung für Achtsamkeit (score_ffmq)
# Die Punkte im QQ-Plot liegen relativ nahe an der Linie.
# → Das spricht für eine annähernde Normalverteilung.
# → Kleine Abweichungen am Rand sind bei größeren Stichproben (N > 30) tolerierbar.
# Korrelation Achtsamkeit (X) → Emotionale Intelligenz (M)
cor.test(df_clean$score_ffmq, df_clean$score_sseit, method = "spearman")
# Korrelation Emotionale Intelligenz (M) → Selbstwert (Y)
cor.test(df_clean$score_sseit, df_clean$score_rses, method = "spearman")
# Korrelation Achtsamkeit (X) → Selbstwert (Y)
cor.test(df_clean$score_ffmq, df_clean$score_rses, method = "spearman")
# Wenn der p-Wert **unter 0.05** liegt → Ergebnis ist **signifikant** → es besteht ein Zusammenhang.
# Wenn der p-Wert **über 0.05** liegt → Ergebnis ist **nicht signifikant** → kein Zusammenhang.
# Korrelationen haben p-Werte **kleiner als 0.05**:
# → Das bedeutet: Alle drei Zusammenhänge (Achtsamkeit ↔ EI, EI ↔ Selbstwert, Achtsamkeit ↔ Selbstwert) sind **signifikant**
# ============================
# ============================
# Ausreißerprüfung mit Z-Werten
# ============================
# Ziel: Extremwerte (> ±3.29) identifizieren, da sie Analyseergebnisse verzerren können

z_rses <- scale(df_clean$score_rses)        # Selbstwert (Y)
sum(abs(z_rses) > 3.29)                     # Anzahl extremer Ausreißer in Selbstwert

z_ffmq <- scale(df_clean$score_ffmq)        # Achtsamkeit (X)
sum(abs(z_ffmq) > 3.29)                     # Anzahl extremer Ausreißer in Achtsamkeit

z_sseit <- scale(df_clean$score_sseit)      # Emotionale Intelligenz (M)
sum(abs(z_sseit) > 3.29)                    # Anzahl extremer Ausreißer in EI
#keine extremen Ausreißer
# Regressionsmodell für die Mediationsanalyse
# Y = score_rses (Selbstwert)
# M = score_sseit (emotionale Intelligenz)
# X = score_ffmq (Achtsamkeit)

model_mediation <- lm(score_rses ~ score_ffmq + score_sseit, data = df_clean)
# Streudiagramm der Residuen zur visuellen Prüfung auf Homoskedastizität
plot(model_mediation$fitted.values, resid(model_mediation),
     xlab = "Vorhergesagte Werte",
     ylab = "Residuen",
     main = "Homoskedastizität (Streuungskonstanz)")
abline(h = 0, col = "red", lwd = 2)
# Breusch-Pagan-Test zur formalen Prüfung auf Homoskedastizität
install.packages("lmtest")   # nur beim ersten Mal nötig
library(lmtest)
bptest(model_mediation)
# --------------------------------------------
# FAZIT: Der Breusch-Pagan-Test zeigt eine signifikante Heteroskedastizität (BP = 7.88, p = 0.02). 
# Die Annahme konstanter Varianz der Residuen ist damit verletzt. 
# Die Ergebnisse sollten mit Vorsicht interpretiert oder mit robusten Standardfehlern ergänzt werden.-> Bootstrapping
# Lineare Zusammenhänge prüfen (Scatterplots mit Regressionslinie)
library(ggplot2)
# Achtsamkeit (X) → Emotionale Intelligenz (M)
ggplot(df_clean, aes(x = score_ffmq, y = score_sseit)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  labs(title = "Zusammenhang: Achtsamkeit → Emotionale Intelligenz",
       x = "Achtsamkeit (FFMQ)",
       y = "Emotionale Intelligenz (SSEIT)") +
  theme_minimal()
# Emotionale Intelligenz (M) → Selbstwert (Y)
ggplot(df_clean, aes(x = score_sseit, y = score_rses)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
  labs(title = "Zusammenhang: Emotionale Intelligenz → Selbstwert",
       x = "Emotionale Intelligenz (SSEIT)",
       y = "Selbstwert (RSES)") +
  theme_minimal()
# Achtsamkeit (X) → Selbstwert (Y)
ggplot(df_clean, aes(x = score_ffmq, y = score_rses)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(title = "Zusammenhang: Achtsamkeit → Selbstwert",
       x = "Achtsamkeit (FFMQ)",
       y = "Selbstwert (RSES)") +
  theme_minimal()
# Fazit: Lineare Zusammenhänge überprüft
# In allen drei Scatterplots zeigt sich ein klar erkennbarer linearer Zusammenhang 
# zwischen den Variablen (Achtsamkeit → EI, EI → Selbstwert, Achtsamkeit → Selbstwert).
# Dies spricht dafür, dass eine lineare Regression für die Mediationsanalyse angemessen ist.
# ---------------------------------------------
# PRÜFUNG DER MULTIKOLLINEARITÄT
# Ziel: Überprüfung, ob Prädiktoren (Achtsamkeit & Emotionale Intelligenz) zu stark korrelieren
# → Wichtig, um Verzerrungen in der Mediationsanalyse zu vermeiden
# ---------------------------------------------
# Paket installieren (nur beim ersten Mal nötig)
install.packages("car")  
# Paket laden
library(car)
# Regressionsmodell mit beiden Prädiktoren
model_vif <- lm(score_rses ~ score_ffmq + score_sseit, data = df_clean)
# VIF-Werte berechnen
vif(model_vif)
#FAZIT: Multikollinearität (VIF)
# Die VIF-Werte liegen deutlich unter dem kritischen Schwellenwert von 5.
# → Es besteht **keine problematische Multikollinearität** zwischen Achtsamkeit und Emotionaler Intelligenz.
# → Die Prädiktoren sind für eine Mediationsanalyse geeignet.
# -----------------------------------------
# MEDIATIONSANALYSE
# Emotionale Intelligenz (M) mediiert den Zusammenhang zwischen Achtsamkeit (X) und Selbstwert (Y)
# Schritt 1: Pfadmodell definieren
model_mediation <- '
  score_sseit ~ a*score_ffmq         # Pfad a
  score_rses  ~ b*score_sseit        # Pfad b
  score_rses  ~ c_prime*score_ffmq   # direkter Effekt c´

  # Indirekter Effekt
  ind_effect := a * b

  # Totaler Effekt
  total_effect := c_prime + (a * b)
'
# Schritt 2: Modell schätzen
library(lavaan)
fit_mediation <- sem(model_mediation, data = df_clean, se = "bootstrap", bootstrap = 5000)
#  Schritt 3: Ergebnisse anzeigen
summary(fit_mediation, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
# Fazit der Mediationsanalyse

# Es wurde untersucht, ob emotionale Intelligenz (EI) den Zusammenhang zwischen Achtsamkeit und Selbstwert vermittelt.
# Interpretation der Mediationsanalyse
# Der indirekte Effekt über Emotionale Intelligenz (a * b = 0.134) ist signifikant (p = 0.005).
# Das bedeutet: Achtsamkeit erhöht die emotionale Intelligenz, und diese wiederum erhöht den Selbstwert.
# Auch der direkte Effekt bleibt signifikant (c′ = 0.765, p < 0.001) – es liegt also eine partielle Mediation vor.
# Der totale Effekt (c = 0.899, p < 0.001) zeigt, dass Achtsamkeit insgesamt stark mit Selbstwert zusammenhängt.
# Insgesamt erklärt das Modell 42,2% der Varianz im Selbstwert (R² = 0.422) und 35,0% in emotionaler Intelligenz (R² = 0.350).
# → Die Mediation ist sowohl theoretisch sinnvoll als auch empirisch belegt.
#deskriptive Statistik
# Skalenmittelwerte 
df_clean$score_rses   <- rowMeans(df_clean[rses_items], na.rm = TRUE)   # Selbstwert
df_clean$score_sseit  <- rowMeans(df_clean[sseit_items], na.rm = TRUE)  # Emotionale Intelligenz
df_clean$score_ffmq   <- rowMeans(df_clean[ffmq_items], na.rm = TRUE)   # Achtsamkeit
# Mittelwert, Standardabweichung, Min, Max berechnen
summary_stats <- data.frame(
  Skala = c("Selbstwert (RSES)", "Emotionale Intelligenz (SSEIT)", "Achtsamkeit (FFMQ)"),
  Mittelwert = c(mean(df_clean$score_rses), mean(df_clean$score_sseit), mean(df_clean$score_ffmq)),
  SD         = c(sd(df_clean$score_rses), sd(df_clean$score_sseit), sd(df_clean$score_ffmq)),
  Min        = c(min(df_clean$score_rses), min(df_clean$score_sseit), min(df_clean$score_ffmq)),
  Max        = c(max(df_clean$score_rses), max(df_clean$score_sseit), max(df_clean$score_ffmq)),
  N          = c(length(df_clean$score_rses), length(df_clean$score_sseit), length(df_clean$score_ffmq))
)
# Tabelle anzeigen
print(summary_stats)
# Skalensummenwerte berechnen
df_clean$sum_rses  <- rowSums(df_clean[rses_items], na.rm = TRUE)   # Selbstwert
df_clean$sum_sseit <- rowSums(df_clean[sseit_items], na.rm = TRUE)  # Emotionale Intelligenz
df_clean$sum_ffmq  <- rowSums(df_clean[ffmq_items], na.rm = TRUE)   # Achtsamkeit
summary_sums <- data.frame(
  Skala = c("Selbstwert (RSES)", "Emotionale Intelligenz (SSEIT)", "Achtsamkeit (FFMQ)"),
  Mittelwert = c(mean(df_clean$sum_rses), mean(df_clean$sum_sseit), mean(df_clean$sum_ffmq)),
  SD = c(sd(df_clean$sum_rses), sd(df_clean$sum_sseit), sd(df_clean$sum_ffmq)),
  Min = c(min(df_clean$sum_rses), min(df_clean$sum_sseit), min(df_clean$sum_ffmq)),
  Max = c(max(df_clean$sum_rses), max(df_clean$sum_sseit), max(df_clean$sum_ffmq)),
  N = c(length(df_clean$sum_rses), length(df_clean$sum_sseit), length(df_clean$sum_ffmq))
)
print(summary_sums)
# -----------------------------------------------
# Deskriptive Statistik: Mittelwerte & Summenwerte
# -----------------------------------------------

# Zur Beschreibung der zentralen Variablen wurden sowohl Skalenmittelwerte als auch Skalensummenwerte berechnet.
# Der Skalenmittelwert zeigt die durchschnittliche Itembewertung pro Person, der Summenwert ergibt sich aus der Addition aller Itemwerte je Skala.
# N= 279

# Selbstwert (RSES):
# Skalenmittelwert: M = 3.03, SD = 0.6, Min = 1.1, Max = 3.9
# Skalensummenwert: M = 30.34, SD = 6.00, Min = 11, Max = 39

# Emotionale Intelligenz (SSEIT):
# Skalenmittelwert: M = 3.66, SD = 0.37, Min = 2.55, Max = 4.76
# Skalensummenwert: M = 120.65, SD = 12.36, Min = 84, Max = 157

# Achtsamkeit (FFMQ):
# Skalenmittelwert: M = 3.27, SD = 0.42, Min = 2.28, Max = 4.36
# Skalensummenwert: M = 127.51, SD = 16,57 Min = 89, Max = 170

# → Die Skalen weisen insgesamt mittlere bis hohe Ausprägungen mit ausreichender Streuung auf.
# → Die Datenbasis eignet sich gut für die anschließenden Analysen.
sum(complete.cases(df_clean[, c("score_ffmq", "score_sseit", "score_rses")]))
#Demografische Daten analysieren

#Altersspanne
range(df_clean$v_112, na.rm = TRUE)
# Mittelwert des Alters
mean(df_clean$v_112, na.rm = TRUE)

# Standardabweichung des Alters
sd(df_clean$v_112, na.rm = TRUE)

#Anzahl Männer und Frauen
table(factor(df_clean$Geschlecht, levels = c(1, 2), labels = c("weiblich", "männlich")))

#Bildungsgrad
bildungsgrad_labels <- c(
  "1" = "Kein Schulabschluss",
  "2" = "Hauptschule",
  "3" = "Mittlere Reife",
  "4" = "(Fach-)Abitur",
  "5" = "Bachelor",
  "6" = "Master",
  "7" = "Promotion"
)
table(factor(df_clean$Bildungsgrad, levels = 1:7, labels = bildungsgrad_labels))

#Beruflicher Status
beruf_labels <- c(
  "1" = "Erwerbstätig",
  "2" = "In Ausbildung",
  "3" = "Student*in",
  "4" = "Rentner*in",
  "5" = "Sonstiges")
table(factor(df_clean$Beruf, levels = 1:5, labels = beruf_labels))

#Familienstand
familienstand_labels <- c(
  "1" = "Ledig",
  "2" = "Verheiratet",
  "3" = "Geschieden",
  "4" = "Sonstiges"
)
table(factor(df_clean$Familienstand, levels = 1:4, labels = familienstand_labels))

#Psychische Erkrankungen
psy_labels <- c(
  "1" = "Ja, aktuell",
  "2" = "Ja, in der Vergangenheit",
  "3" = "Nein"
)
table(factor(df_clean$v_11, levels = 1:3, labels = psy_labels))
table(factor(df_clean$Beruf, levels = 1:5, labels = beruf_labels))
sum(is.na(df_clean$Beruf))





# Direkte Berechnung Cohen's d
mean_f <- mean(df_clean$score_sseit[df_clean$Geschlecht == 1])
mean_m <- mean(df_clean$score_sseit[df_clean$Geschlecht == 2])
sd_f <- sd(df_clean$score_sseit[df_clean$Geschlecht == 1])
sd_m <- sd(df_clean$score_sseit[df_clean$Geschlecht == 2])
n_f <- length(df_clean$score_sseit[df_clean$Geschlecht == 1])
n_m <- length(df_clean$score_sseit[df_clean$Geschlecht == 2])

# Pooled SD
sd_pooled <- sqrt(((n_f - 1)*sd_f^2 + (n_m - 1)*sd_m^2) / (n_f + n_m - 2))

# Cohen's d
d <- (mean_f - mean_m) / sd_pooled
d
#Konfidenzintervall H1
install.packages("boot")
library(boot)

# Funktion für Spearman-Korrelation
spearman_cor <- function(data, indices) {
  d <- data[indices, ]
  return(cor(d$score_sseit, d$score_rses, method = "spearman"))
}

# Datensatz für Bootstrapping definieren
mydata <- data.frame(score_sseit = df_clean$score_sseit,
                     score_rses = df_clean$score_rses)

# Bootstrapping mit 5000 Resamples
set.seed(123)  # für Reproduzierbarkeit
results <- boot(data = mydata, statistic = spearman_cor, R = 5000)

# Konfidenzintervall berechnen
boot.ci(results, type = "perc")
#Konfidenzintervalle H2
# t-Test mit Speicherung in Objekt
t_test_h2 <- t.test(score_sseit ~ Geschlecht, data = df_clean, var.equal = TRUE)

# Konfidenzintervall anzeigen
t_test_h2$conf.int
#Konfidenzintervall H3
# Paket laden
library(boot)

# Funktion definieren
spearman_ci <- function(data, indices) {
  d <- data[indices, ]
  return(cor(d$score_sseit, d$v_8, method = "spearman"))
}

# Bootstrapping durchführen
set.seed(123)
boot_spearman <- boot(data = df_bachelor, statistic = spearman_ci, R = 5000)

# 95%-Konfidenzintervall berechnen
boot.ci(boot_spearman, type = "perc")
#Konfidenzintervall mediation 


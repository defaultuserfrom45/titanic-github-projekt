# Ziel: Bereinigten Datensatz laden und deskriptive Auswertungen erstellen
# Output: Tabellen unter report/tables, Grafiken unter report/figures

if (!dir.exists("report")) dir.create("report", recursive = TRUE)
if (!dir.exists("report/figures")) dir.create("report/figures", recursive = TRUE)
if (!dir.exists("report/tables")) dir.create("report/tables", recursive = TRUE)

# ----------------------------
# 1) Daten + Funktionen laden
# ----------------------------
data <- read.csv("data/processed/titanic_clean.csv", stringsAsFactors = FALSE)

if ("Survived" %in% names(data)) data$Survived <- factor(data$Survived)
if ("Sex" %in% names(data)) data$Sex <- factor(data$Sex)
if ("Embarked" %in% names(data)) data$Embarked <- factor(data$Embarked)
if ("Pclass" %in% names(data)) data$Pclass <- factor(data$Pclass, ordered = TRUE)
if ("Anrede" %in% names(data)) data$Anrede <- factor(data$Anrede)
if ("Deck" %in% names(data)) data$Deck <- factor(data$Deck)
if ("Seite" %in% names(data)) data$Seite <- factor(data$Seite)

source("R/03_functions.R")

save_plot_png <- function(filename, expr, w = 1200, h = 800, res = 120) {
  png(filename, width = w, height = h, res = res)
  expr
  dev.off()
}

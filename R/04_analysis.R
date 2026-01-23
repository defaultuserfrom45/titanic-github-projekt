# Ziel: Bereinigten Datensatz laden und deskriptive Auswertungen erstellen
# Output: Tabellen unter report/tables, Grafiken unter report/figures

if (!dir.exists("report")) dir.create("report", recursive = TRUE)
if (!dir.exists("report/figures")) dir.create("report/figures", recursive = TRUE)
if (!dir.exists("report/tables")) dir.create("report/tables", recursive = TRUE)

# 1) Daten + Funktionen laden
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

# 2) Deskriptiv: metrische Variablen

res_age <- NULL
if ("Age" %in% names(data)) {
  res_age <- desc_metrisch(data, "Age")
  print(res_age)
  write.csv(as.data.frame(res_age), "report/tables/desc_age.csv", row.names = FALSE)
}

res_fare <- NULL
if ("Fare" %in% names(data)) {
  res_fare <- desc_metrisch(data, "Fare")
  print(res_fare)
  write.csv(as.data.frame(res_fare), "report/tables/desc_fare.csv", row.names = FALSE)
}

# 3) Deskriptiv: kategoriale Variablen

tab_survived <- NULL
if ("Survived" %in% names(data)) {
  tab_survived <- desc_kategorial(data, "Survived")
  print(tab_survived)
  write.csv(tab_survived, "report/tables/tab_survived.csv", row.names = FALSE)
}

tab_sex <- NULL
if ("Sex" %in% names(data)) {
  tab_sex <- desc_kategorial(data, "Sex")
  print(tab_sex)
  write.csv(tab_sex, "report/tables/tab_sex.csv", row.names = FALSE)
}

tab_pclass <- NULL
if ("Pclass" %in% names(data)) {
  tab_pclass <- desc_kategorial(data, "Pclass")
  print(tab_pclass)
  write.csv(tab_pclass, "report/tables/tab_pclass.csv", row.names = FALSE)
}

tab_embarked <- NULL
if ("Embarked" %in% names(data)) {
  tab_embarked <- desc_kategorial(data, "Embarked")
  print(tab_embarked)
  write.csv(tab_embarked, "report/tables/tab_embarked.csv", row.names = FALSE)
}

if ("Anrede" %in% names(data)) {
  tab_anrede <- desc_kategorial(data, "Anrede")
  write.csv(tab_anrede, "report/tables/tab_anrede.csv", row.names = FALSE)
}

if ("Deck" %in% names(data)) {
  tab_deck <- desc_kategorial(data, "Deck")
  write.csv(tab_deck, "report/tables/tab_deck.csv", row.names = FALSE)
}

if ("Seite" %in% names(data)) {
  tab_seite <- desc_kategorial(data, "Seite")
  write.csv(tab_seite, "report/tables/tab_seite.csv", row.names = FALSE)
}


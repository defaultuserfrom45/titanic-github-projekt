# Titanic – Datenaufbereitung (Schrittweise Umsetzung)
# Ziel: Rohdaten einlesen und Variablen für Analyse sinnvoll aufbereiten

data_raw <- read.csv("data/raw/titanic.csv", stringsAsFactors = FALSE)


# 1) Anrede aus Name extrahieren und vereinheitlichen

data_raw$Anrede <- sub("^.*,(.*)\\..*$", "\\1", data_raw$Name)
data_raw$Anrede <- trimws(data_raw$Anrede)

# Vereinheitlichungen (inhaltlich gleiche Titel zusammenfassen)
data_raw$Anrede[data_raw$Anrede %in% c("Ms", "Mlle")] <- "Miss"
data_raw$Anrede[data_raw$Anrede %in% c("Mme")] <- "Mrs"

# seltene/adelige Titel zu einer Sammelkategorie
selten <- c("Lady","Countess","Capt","Col","Don","Dr","Major","Rev","Sir","Jonkheer","Dona")
data_raw$Anrede[data_raw$Anrede %in% selten] <- "Selten"

# 2) Variablen sinnvoll kodieren

data_raw$Survived <- factor(data_raw$Survived, levels = c(0, 1), labels = c("Nein", "Ja"))
data_raw$Sex <- factor(data_raw$Sex, levels = c("male", "female"), labels = c("männlich", "weiblich"))
data_raw$Embarked <- factor(data_raw$Embarked, levels = c("C","Q","S"), labels = c("Cherbourg","Queenstown","Southampton"))

data_raw$Pclass <- factor(data_raw$Pclass, levels = c(1,2,3), ordered = TRUE)

# Kurzer Check
str(data_raw)
summary(data_raw)


# Fehlende Alterswerte imputieren (Median nach Anrede)

median_age <- tapply(data_raw$Age, data_raw$Anrede, median, na.rm = TRUE)

idx_na <- is.na(data_raw$Age)
data_raw$Age[idx_na] <- median_age[data_raw$Anrede[idx_na]]


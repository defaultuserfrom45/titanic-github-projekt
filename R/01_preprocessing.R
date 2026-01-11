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
data_raw$Embarked <- factor(
  data_raw$Embarked,
  levels = c("C","Q","S"),
  labels = c("Cherbourg","Queenstown","Southampton")
)

data_raw$Pclass <- factor(data_raw$Pclass, levels = c(1,2,3), ordered = TRUE)

# Kurzer Kontrollblick
str(data_raw)
summary(data_raw)


# 3) Fehlende Alterswerte imputieren (Median nach Anrede) – mit Fallback

median_age <- tapply(data_raw$Age, data_raw$Anrede, median, na.rm = TRUE)
overall_med <- median(data_raw$Age, na.rm = TRUE)

idx_na <- is.na(data_raw$Age)
imputed <- median_age[data_raw$Anrede[idx_na]]
imputed[is.na(imputed)] <- overall_med
data_raw$Age[idx_na] <- imputed


# 4) Cabin: unbekannt -> NA, Deck und Seite extrahieren

if ("Cabin" %in% names(data_raw)) {
  # Leere Cabin-Einträge auf NA setzen (falls als "" vorhanden)
  data_raw$Cabin[data_raw$Cabin == ""] <- NA

  idx_cabin <- !is.na(data_raw$Cabin)

  # Nur die erste Kabine verwenden (z.B. "C23" aus "C23 C25 C27")
  first_cabin <- rep(NA_character_, nrow(data_raw))
  first_cabin[idx_cabin] <- sub(" .*", "", data_raw$Cabin[idx_cabin])

  # Deck: erster Buchstabe der ersten Cabin
  data_raw$Deck <- NA
  data_raw$Deck[idx_cabin] <- substr(first_cabin[idx_cabin], 1, 1)
  data_raw$Deck <- factor(data_raw$Deck)

  # Cabin-Nummer: Ziffern aus der ersten Cabin
  cabin_num <- rep(NA_integer_, nrow(data_raw))
  cabin_num[idx_cabin] <- suppressWarnings(as.integer(gsub("[^0-9]", "", first_cabin[idx_cabin])))

  # Seite: ungerade = Steuerbord, gerade = Backbord
  data_raw$Seite <- NA
  idx_num <- !is.na(cabin_num)
  data_raw$Seite[idx_num] <- ifelse(cabin_num[idx_num] %% 2 == 1, "Steuerbord", "Backbord")
  data_raw$Seite <- factor(data_raw$Seite, levels = c("Backbord", "Steuerbord"))
}


# 5) Unnötige Variablen entfernen (robust, je nach Spaltennamen)

drop_cols <- c("PassengerID", "PassengerId", "Name", "Ticket", "Cabin")
drop_cols <- drop_cols[drop_cols %in% names(data_raw)]
data_clean <- data_raw[, !(names(data_raw) %in% drop_cols), drop = FALSE]


# 6) Bereinigten Datensatz speichern

if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)
write.csv(data_clean, "data/processed/titanic_clean.csv", row.names = FALSE)


# Abschluss-Check
str(data_clean)
summary(data_clean)

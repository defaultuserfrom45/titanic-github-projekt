# Laden des bereinigten Datensatzes
# Laden der Funktionsdatei 03_functions.R
data <- read.csv("data/processed/titanic_clean.csv", stringsAsFactors = FALSE)

if ("Survived" %in% names(data)) data$Survived <- factor(data$Survived)
if ("Sex" %in% names(data)) data$Sex <- factor(data$Sex)
if ("Embarked" %in% names(data)) data$Embarked <- factor(data$Embarked)
if ("Pclass" %in% names(data)) data$Pclass <- factor(data$Pclass, ordered = TRUE)
if ("Anrede" %in% names(data)) data$Anrede <- factor(data$Anrede)
if ("Deck" %in% names(data)) data$Deck <- factor(data$Deck)
if ("Seite" %in% names(data)) data$Seite <- factor(data$Seite)

source("R/03_functions.R")

if (!dir.exists("report/figures")) dir.create("report/figures", recursive = TRUE)
if (!dir.exists("report/tables")) dir.create("report/tables", recursive = TRUE)

save_plot_png <- function(filename, expr, w = 1200, h = 800, res = 120) {
png(filename, width = w, height = h, res = res)
expr
dev.off()
}





# Zusätzliche bivariate Auswertungen
# Embarked vs Survived
if (all(c("Embarked","Survived") %in% names(data))) {
res_emb_surv <- biv_kat_kat(data, "Embarked", "Survived")
print(res_emb_surv)

write.csv(as.data.frame.matrix(res_emb_surv$Kreuztabelle),
"report/tables/kreuz_embarked_survived.csv")
}

# Deck vs Survived
if (all(c("Deck","Survived") %in% names(data))) {
res_deck_surv <- biv_kat_kat(data, "Deck", "Survived")
print(res_deck_surv)

write.csv(as.data.frame.matrix(res_deck_surv$Kreuztabelle),
"report/tables/kreuz_deck_survived.csv")
}

# Seite vs Survived
if (all(c("Seite","Survived") %in% names(data))) {
if (sum(!is.na(data$Seite)) > 0) {
res_seite_surv <- biv_kat_kat(data, "Seite", "Survived")
print(res_seite_surv)

write.csv(as.data.frame.matrix(res_seite_surv$Kreuztabelle),
"report/tables/kreuz_seite_survived.csv")
}
}





# Zusätzliche Grafiken für Bericht
# Barplot: Überleben nach Einschiffungshafen
if (all(c("Embarked","Survived") %in% names(data))) {
save_plot_png("report/figures/bar_survived_by_embarked.png", {
tab <- table(data$Embarked, data$Survived)
barplot(tab, beside = TRUE, legend = TRUE,
main = "Überleben nach Einschiffungshafen",
xlab = "Embarked", ylab = "Häufigkeit")
})
}

# Barplot: Überleben nach Geschlecht
if (all(c("Sex","Survived") %in% names(data))) {
save_plot_png("report/figures/bar_survived_by_sex_extra.png", {
tab <- table(data$Sex, data$Survived)
barplot(tab, beside = TRUE, legend = TRUE,
main = "Überleben nach Geschlecht",
xlab = "Sex", ylab = "Häufigkeit")
})
}

# Visualisierung über die Funktion
if (all(c("Survived","Sex","Pclass") %in% names(data))) {
save_plot_png("report/figures/viz_survived_sex_pclass_extra.png", {
viz_kat_3(data, "Survived", "Sex", "Pclass")
})
}

cat("Zusatzanalyse (Person B) abgeschlossen. Tabellen: report/tables, Grafiken: report/figures\n")




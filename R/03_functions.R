# Funktionen zur Deskription und Visualisierung

source("R/02_helpers.R")

# Deskriptive Statistik für metrische Variablen
desc_metrisch <- function(data, var) {
  check_var(data, var)
  x <- data[[var]]
  x <- x[!is.na(x)]
  list(
    n = length(x),
    Mittelwert = mean(x),
    Median = median(x),
    Standardabweichung = sd(x),
    Minimum = min(x),
    Maximum = max(x)
  )
}

# Deskriptive Statistik für kategoriale Variablen
desc_kategorial <- function(data, var) {
  check_var(data, var)
  tab <- table(data[[var]], useNA = "ifany")
  data.frame(
    Ausprägung = names(tab),
    Häufigkeit = as.integer(tab),
    Anteil = round(prop.table(tab), 3)
  )
}

# Bivariate Statistik: zwei kategoriale Variablen
biv_kat_kat <- function(data, v1, v2) {
  d <- drop_na_2(data, v1, v2)
  tab <- table(d[[v1]], d[[v2]])
  list(
    Kreuztabelle = tab,
    Zeilenanteile = round(prop.table(tab, 1), 3)
  )
}

# Bivariate Statistik: metrisch + dichotom
biv_metr_dicho <- function(data, metr, dich) {
  d <- drop_na_2(data, metr, dich)
  gruppen <- split(d[[metr]], d[[dich]])
  lapply(gruppen, function(x) {
    c(n = length(x),
      Mittelwert = mean(x),
      Median = median(x),
      SD = sd(x))
  })
}

# Visualisierung für 3 kategoriale Variablen
viz_kat_3 <- function(data, v1, v2, v3) {
  d <- drop_na_2(data, v1, v2)
  d <- d[!is.na(d[[v3]]), ]
  tab <- table(d[[v1]], d[[v2]], d[[v3]])
  m <- apply(tab, c(1,2), sum)
  barplot(m, beside = TRUE, legend = TRUE,
          main = paste(v1, v2, v3, sep=" / "),
          ylab = "Häufigkeit")
}

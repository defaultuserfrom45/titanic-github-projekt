# Hilfsfunktionen (intern)
# Werden von 03_functions.R genutzt

check_var <- function(data, var) {
  if (!is.data.frame(data)) stop("data muss ein data.frame sein.")
  if (!is.character(var) || length(var) != 1) stop("var muss ein einzelner String sein.")
  if (!(var %in% names(data))) stop(paste("Variable nicht gefunden:", var))
  TRUE
}

drop_na_2 <- function(data, v1, v2) {
  check_var(data, v1)
  check_var(data, v2)
  data[!is.na(data[[v1]]) & !is.na(data[[v2]]), ]
}

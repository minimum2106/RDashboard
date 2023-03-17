convert_name <- function(name) {
  gsub(" ", "_", name)
}

standardize <- function(x, na.rm = TRUE) {
  if (is.numeric(x)) {
    x_sd <- sd(x, na.rm = na.rm)
    x <- x - mean(x, na.rm = na.rm)
    if (isTRUE(x_sd > 0)) {
      x / x_sd
    } else {
      x
    }
  } else {
    x
  }
}

normalize <- function(x, x_min, x_max) {
  if (is.numeric(x)) (x - x_min) / (x_max - x_min) else x
}

log_transform <- function(x) {
  if (is.numeric(x)) log(x) else x
}

exponential <- function(x) {
  if ( is.numeric(x)) exp(x) else x
}

match_or_not <- function(x, y) {
  match(x, y, 0)
}

non_numeric <- function(x) !is.numeric(x)

as_integer <- function(x) as.integer(x)

as_numeric <- function(x) as.numeric(x)

as_factor_self <- function(x, ordered = FALSE) factor(x, ordered = ordered)

as_character <- function(x) as.character(x)

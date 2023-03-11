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

normalize <- function(x, y) {
  if (is.numeric(x) && is.numeric(y)) x / y else x
}
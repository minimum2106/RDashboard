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

as_integer <- function(x) {
  if (is.factor(x)) {
    int <- sshhr(levels(x) %>% .[x] %>% as.integer())
    if (length(na.omit(int)) == 0) as.integer(x) else int
  } else if (is.character(x)) {
    int <- sshhr(as.integer(x))
    if (length(na.omit(int)) == 0) as_integer(as.factor(x)) else int
  } else {
    as.integer(x)
  }
}

as_numeric <- function(x) {
  if (is.factor(x)) {
    num <- sshhr(levels(x) %>% .[x] %>% as.numeric())
    if (length(na.omit(num)) == 0) as.numeric(x) else num
  } else if (is.character(x)) {
    num <- sshhr(as.numeric(x))
    if (length(na.omit(num)) == 0) as_numeric(as.factor(x)) else num
  } else {
    as.numeric(x)
  }
}

as_factor_self <- function(x, ordered = FALSE) factor(x, ordered = ordered)

as_character <- function(x) as.character(x)

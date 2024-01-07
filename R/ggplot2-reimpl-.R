### INCLUDE BEGIN
#' @include Layer.r
#' @include compat-plyr.R
#' @include aab-other_utils.r
#' @include performance.R
NULL
### INCLUDE END


check_subclass <- function (x, subclass, argname = to_lower_ascii(subclass), env = parent.frame(), call = caller_env())
{
  if (inherits(x, subclass)) {
    x
  }
  else if (is.character(x) && length(x) == 1) {
    name <- paste0(subclass, camelize(x, first = TRUE))
    obj <- find_global(name, env = env)
    if (is.null(obj) || !inherits(obj, subclass)) {
      cli::cli_abort("Can't find {argname} called {.val {x}}", call = call)
    }
    else {
      obj
    }
  }
  else {
    cli::cli_abort("{argname} must be either a string or a {.cls {subclass}} object", call = call)
  }
}

# Check inputs with tibble but allow column vectors (see #2609 and #2374)
as_gg_data_frame <- function(x) {
  x <- lapply(x, validate_column_vec)
  new_data_frame(x)
}

validate_column_vec <- function(x) {
  if (is_column_vec(x)) {
    dim(x) <- NULL
  }
  x
}

is_column_vec <- function(x) {
  dims <- dim(x)
  length(dims) == 2L && dims[[2]] == 1L
}


# Use chartr() for safety since toupper() fails to convert i to I in Turkish locale
lower_ascii <- "abcdefghijklmnopqrstuvwxyz"
upper_ascii <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
to_lower_ascii <- function(x) chartr(upper_ascii, lower_ascii, x)
to_upper_ascii <- function(x) chartr(lower_ascii, upper_ascii, x)

tolower <- function(x) {
  cli::cli_abort("Please use {.fn to_lower_ascii}, which works fine in all locales.")
}

toupper <- function(x) {
  cli::cli_abort("Please use {.fn to_upper_ascii}, which works fine in all locales.")
}

# Convert a snake_case string to camelCase
camelize <- function(x, first = FALSE) {
  x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
  if (first) x <- firstUpper(x)
  x
}

snakeize <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  to_lower_ascii(x)
}


firstUpper <- function(s) {
  paste0(to_upper_ascii(substring(s, 1, 1)), substring(s, 2))
}

snake_class <- function(x) {
  snakeize(class(x)[1])
}

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || is.waive(df)
}

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

is.waive <- function(x) inherits(x, "waiver")

is.formula <- function(x) inherits(x, "formula")

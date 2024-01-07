### INCLUDE BEGIN
#' @include aab-other_utils.r
#' @include performance.R
NULL
### INCLUDE END


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

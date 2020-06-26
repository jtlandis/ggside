#' @import ggplot2
#' @import grid
#' @importFrom grid grobName


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}


empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || is.waive(df)
}




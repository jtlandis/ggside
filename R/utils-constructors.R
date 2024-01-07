### INCLUDE BEGIN
#' @include utils-.R
NULL
### INCLUDE END

aes_to_map <- function(ggproto, side) {
  resolve_arg(side, c("x", "y"), null.ok = FALSE)
  other_side <- switch(side, x = "y", y = "x")
  req_aes <- pull_aes(ggproto$required_aes)
  opt_aes <- pull_aes(ggproto$optional_aes)
  non_mis <- pull_aes(ggproto$non_missing_aes)
  def_aes <- names(ggproto$default_aes)
  all_aes <- unique(c(req_aes, opt_aes, non_mis, def_aes))
  all_aes[all_aes %in% .ggside_global[[sprintf(".%s_aes", other_side)]]]
}

data_unmap <- function(data, side) {
  names(data) <- sub(sprintf("%sside", side), "", names(data))
  data
}

data_map <- function(data, side, map) {
  x <- names(data)
  aes <- x %in% map
  x[aes] <- sprintf("%sside%s", side, x[aes])
  names(data) <- x
  data
}


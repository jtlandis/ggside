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

zap_dots <- function(call, zap = character(), ...) {
  dots <- enexprs(...)
  # remove dots and splice them in
  call <- call_modify(call, ... = zap(), !!!dots)
  if (length(zap) > 0) {
    to_zap <- rep_named(zap, list(zap()))
    call <- call_modify(call, !!!to_zap)
  }
  call
}

call_layer_param_aware <-
  function(expr,
           zap = character(),
           ...,
           env = caller_env()) {
    call <- match.call()$expr
    call <- zap_dots(call, zap = zap, ...)
    layer <- eval(call, envir = env)
    any_zap <- length(zap) > 0
    dot_names <- ...names()
    if (length(intersect(dot_names, zap)) > 0) {
      ind <- which(dot_names %in% zap)
      new_name <- sub("color", "colour", dot_names[ind], fixed = TRUE)
      lst <- vector("list", length(ind))
      for (i in seq_along(lst))
        lst[[i]] <- ...elt(ind[i])
      layer$aes_params[new_name] <- lst
    }
    layer
  }

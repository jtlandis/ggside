### INCLUDE BEGIN
#' @include compat-plyr.R
#' @include ggside.R
NULL
### INCLUDE END

find_global <- function (name, env, mode = "any") {
  if (exists(name, envir = env, mode = mode)) {
    return(get(name, envir = env, mode = mode))
  }
  nsenv <- asNamespace("ggplot2")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }
  nsenv <- asNamespace("ggside")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }
  NULL
}


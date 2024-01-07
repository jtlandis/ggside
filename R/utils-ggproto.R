### INCLUDE BEGIN
#' @include compat-plyr.R
NULL
### INCLUDE END



# injects the body with enexpr.
# call_parent_method --> ggproto_parent_method(!!!args)
# and then the proper formals of the function are made
new_ggproto_fun <- function(ggproto_method,
                            body) {
  body <- enexpr(body)
  inj <- list(call_parent_method = quote(ggproto_parent_method(!!!formals_)))
  body <- do.call(substitute, list(body, inj))
  ggproto_parent_method <- environment(ggproto_method)$f
  formals_ <- ggproto_formals0(ggproto_method)
  body <- inject(expr(!!body))
  fun <- new_function(
    args = formals(ggproto_parent_method),
    body = body
  )
  fun

}

# grabs the formals of an inner function from
# a ggproto method accessed via `$` or `[[`
ggproto_formals <- function(x) formals(environment(x)$f)

# uses formals input and renames the values to
# match their own names
formals_as_defaults <- function(formals_) {
  names_ <- names(formals_)
  for (i in seq_along(formals_)) {
    formals_[[i]] <- as.name(names_[i])
  }
  if ("..." %in% names_)
    names(formals_)[names_ %in% "..."] <- ""
  formals_
}

# like ggproto_formals except they values
# get renamed
ggproto_formals0 <- function(ggproto_method) {
  formals_ <- ggproto_formals(ggproto_method)
  formals_as_defaults(formals_ = formals_)
}


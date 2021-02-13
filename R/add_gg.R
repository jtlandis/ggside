

#' @export
`+.gg` <- function(e1, e2) {
  if (missing(e2)) {
    abort("Cannot use `+.gg()` with a single argument. Did you accidentally put + on a new line?")
  }
  e2name <- deparse(substitute(e2))
  add_gg(e1, e2, e2name)
}

add_gg <- function(e1, e2, e2name) {
  UseMethod("add_gg")
}

add_gg.default <- function(e1, e2, e2name) {
  abort(glue("No method defined for class {paste(class(e1),collapse = ', ')}."))
}

add_gg.ggplot <- function(e1, e2, e2name){
  if (is.null(e2)) return(e1)

  p <- plot_clone(e1)
  p <- ggplot_add(e2, p, e2name)
  set_last_plot(p)
  p
}

add_gg.theme <- function(e1, e2, e2name) {
  if (!is.list(e2)) { # in various places in the code base, simple lists are used as themes
    abort(glue("Can't add `{e2name}` to a theme object."))
  }

  # If e2 is a complete theme or e1 is NULL, just return e2
  if (is_theme_complete(e2) || is.null(e1))
    return(e2)

  # Iterate over the elements that are to be updated
  for (item in names(e2)) {
    x <- merge_element(e2[[item]], e1[[item]])

    # Assign it back to e1
    # This is like doing e1[[item]] <- x, except that it preserves NULLs.
    # The other form will simply drop NULL values
    e1[item] <- list(x)
  }

  # make sure the "complete" attribute is set; this can be missing
  # when e1 is an empty list
  attr(e1, "complete") <- is_theme_complete(e1)

  # Only validate if both themes should be validated
  attr(e1, "validate") <-
    is_theme_validate(e1) && is_theme_validate(e2)

  e1
}

add_gg.ggproto <- function(e1, e2, e2name) {
  abort("Cannot add ggproto objects together. Did you forget to add this object to a ggplot object?")
}

add_gg.ggside <- function(e1, e2, e2name) {
  p <- NextMethod("add_gg")
  if(!is.ggside_layer(e2)){
    p <- as_ggside(p, e2)
  }
  p
}

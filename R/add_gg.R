### INCLUDE BEGIN
#' @include side-coord-cartesian.R
#' @include side-facet_.R
NULL
### INCLUDE END



#' @keywords internal
validate_ggside <- function(e2, plot) UseMethod("validate_ggside")
#' @keywords internal
validate_ggside.default <- function(e2, plot) plot
#' @keywords internal
validate_ggside.Facet <- function(e2, plot) {
  plot[["facet"]] <- ggside_facet(plot[["facet"]], plot[["ggside_opt"]])
  plot
}
#' @keywords internal
validate_ggside.Coord <- function(e2, plot) {
  plot[["coordinates"]] <- ggside_coord(plot[["coordinates"]])
  plot
}

plot_clone <- function(plot) {
  p <- plot
  p$scales <- plot$scales$clone()

  p
}

# # @export
# `+.gg` <- function(e1, e2) {
#   if (missing(e2)) {
#     abort("Cannot use `+.gg()` with a single argument. Did you accidentally put + on a new line?")
#   }
#   e2name <- deparse(substitute(e2))
#   add_gg(e1 = e1, e2 = e2, e2name = e2name)
# }

# # @keywords internal
# add_gg <- function(e1, e2, e2name) {
#   UseMethod("add_gg")
# }

# # @keywords internal
# add_gg.default <- function(e1, e2, e2name) {
#   abort(glue("No method defined for class {paste(class(e1),collapse = ', ')}."))
# }

# # @keywords internal
# add_gg.ggplot <- function(e1, e2, e2name){
#   if (is.null(e2)) return(e1)

#   p <- plot_clone(e1)
#   p <- ggplot_add(object = e2, plot = p, object_name = e2name)
#   set_last_plot(p)
#   p
# }

# # @importFrom ggplot2 merge_element
# # @keywords internal
# add_gg.theme <- function(e1, e2, e2name) {
#   if (!is.list(e2)) { # in various places in the code base, simple lists are used as themes
#     abort(glue("Can't add `{e2name}` to a theme object."))
#   }

#   # If e2 is a complete theme or e1 is NULL, just return e2
#   if (is_theme_complete(e2) || is.null(e1))
#     return(e2)

#   # Iterate over the elements that are to be updated
#   for (item in names(e2)) {
#     x <- merge_element(e2[[item]], e1[[item]])

#     # Assign it back to e1
#     # This is like doing e1[[item]] <- x, except that it preserves NULLs.
#     # The other form will simply drop NULL values
#     e1[item] <- list(x)
#   }

#   # make sure the "complete" attribute is set; this can be missing
#   # when e1 is an empty list
#   attr(e1, "complete") <- is_theme_complete(e1)

#   # Only validate if both themes should be validated
#   attr(e1, "validate") <-
#     is_theme_validate(e1) && is_theme_validate(e2)

#   e1
# }

# #' @keywords internal
# add_gg.ggproto <- function(e1, e2, e2name) {
#   abort("Cannot add ggproto objects together. Did you forget to add this object to a ggplot object?")
# }

# #' @keywords internal
# add_gg.ggside <- function(e1, e2, e2name) {
#   p <- NextMethod("add_gg")
#   p <- clone_ggside_plot(p)
#   validate_ggside(e2, plot = p)
# }

# is_theme_complete <- function(x) isTRUE(attr(x, "complete", exact = TRUE))

# is_theme_validate <- function(x) {
#   validate <- attr(x, "validate", exact = TRUE)
#   if (is.null(validate))
#     TRUE # we validate by default
#   else
#     isTRUE(validate)
# }




# # @export
# merge_element.default <- function(new, old) {
#   if (is.null(old) || inherits(old, "element_blank")) {
#     return(new)
#   }
#   else if (is.null(new) || is.character(new) || is.numeric(new) ||
#            is.unit(new) || is.logical(new)) {
#     return(new)
#   }
#   cli::cli_abort("No method for merging {.cls {class(new)[1]}} into {.cls {class(old)[1]}}.")
# }
#
# merge_element.element <- function(new, old) {
#   if (is.null(old) || inherits(old, "element_blank")) {
#     return(new)
#   }
#   if (!inherits(new, class(old)[1])) {
#     cli::cli_abort("Only elements of the same class can be merged.")
#   }
#   idx <- vapply(new, is.null, logical(1))
#   idx <- names(idx[idx])
#   new[idx] <- old[idx]
#   new
# }
#
# merge_element.element_blank <- function(new, old) {
#   # If new is element_blank, just return it
#   new
# }

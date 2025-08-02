### INCLUDE BEGIN
#' @include side-coord-cartesian.R
#' @include ggside.R
#' @include side-facet_.R
#' @include side-layout-.r
#' @include all_classes.r
NULL
### INCLUDE END

#' @title Explicit conversion to ggside object
#' @name as_ggside
#' @description
#' Function is only exported for possible extensions to ggside. ggplot2 objects
#' are implicitly converted to ggside objects by 'adding' a ggside object
#' such as a `ggside_layer` object.
#'
#' @param x an object to convert
#' @param ... unused argument
#' @export
as_ggside <- function(x, ...) UseMethod("as_ggside")

#' @rdname as_ggside
#' @export
as_ggside.default <- function(x, ...) cli::cli_abort("No as_ggside() method for class {.cls {class(x)}}")

#' @rdname as_ggside
#' @param ggside new ggside object to add
#' @export
as_ggside.ggplot <- function(x, ggside = NULL, ...) {
  if (inherits(x[["coordinates"]], "CoordFlip") || inherits(x[["coordinates"]], "CoordPolar")) {
    abort("ggside is not currently compatable with CoordFlip or CoordPolar")
  }
  ggside <- ggside %||% ggside()
  if (!is_ggside_options(ggside)) stop("argument ggside must be of class `ggside_options` or NULL")
  class_ggside(ggplot = x, ggside)
}

#' @rdname as_ggside
#' @export
`as_ggside.ggside::ggside` <- function(x, ggside = NULL, ...) {
  ggside <- ggside %||% x[["ggside_opt"]] %||% ggside()
  if (!is_ggside_options(ggside)) stop("argument ggside must be of class `ggside_options` or NULL")
  update_ggside(x, ggside)
}

#' @rdname as_ggside
#' @export
as_ggside.ggside <- function(x, ggside = NULL, ...) {
  ggside <- ggside %||% x[["ggside_opt"]] %||% ggside()
  if (!is_ggside_options(ggside)) stop("argument ggside must be of class `ggside_options` or NULL")
  update_ggside(x, ggside)
}

#' @keywords internal
update_ggside <- function(object, ggside) UseMethod("update_ggside")

#' @keywords internal
update_ggside.default <- function(object, ggside) abort(glue("No update_ggside() method for class <", glue_collapse(class(object), sep = "/"), ">"))

#' @keywords internal
update_ggside.ggplot <- function(object, ggside = NULL) {
  object$ggside_opt$x.pos <- ggside$x.pos %||% object$ggside_opt$x.pos %||% "top"
  if (!object$ggside_opt$x.pos %in% c("top", "bottom")) {
    abort("x.pos may only be \"top\" or \"bottom\".")
  }
  object$ggside_opt$y.pos <- ggside$y.pos %||% object$ggside_opt$y.pos %||% "right"
  if (!object$ggside_opt$y.pos %in% c("right", "left")) {
    abort("y.pos may only be \"right\" or \"left\".")
  }
  object$ggside_opt$scales <- ggside$scales %||% object$ggside_opt$scales %||% "fixed"
  if (!object$ggside_opt$scales %in% c("fixed", "free", "free_x", "free_y")) {
    abort("scales may only be \"fixed\", \"free\", \"free_x\" or \"free_y\".")
  }
  object$ggside_opt$sides_used <- get_sides(object[["layers"]])
  object$ggside_opt$collapse <- ggside$collapse %||% object$ggside_opt$collapse %||% NULL
  object$ggside_opt$xsidey <- ggside$xsidey %||% object$ggside_opt$xsidey %||% NULL
  object$ggside_opt$ysidex <- ggside$ysidex %||% object$ggside_opt$ysidex %||% NULL
  object$ggside_opt$draw_x_on <- ggside$draw_x_on %||% object$ggside_opt$draw_x_on %||% "default"
  object$ggside_opt$draw_y_on <- ggside$draw_y_on %||% object$ggside_opt$draw_y_on %||% "default"
  object$ggside_opt$strip <- ggside$strip %||% object$ggside_opt$strip %||% "default"
  object$ggside_opt$respect_side_labels <- ggside$respect_side_labels %||% object$ggside_opt$respect_side_labels %||% "default"

  object$facet <- ggside_facet(object$facet, object$ggside_opt)
  object$coordinates <- ggside_coord(object$coordinates)
  object$layout <- ggside_layout(object$layout)
  object
}


get_sides <- function(layers) {
  layer_mappings <- lapply(layers, layer_type)
  sides_used <- unlist(layer_mappings)
  sides_used <- unique(sides_used[!sides_used %in% "main"])
  sides_used
}

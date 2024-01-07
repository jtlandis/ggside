### INCLUDE BEGIN
#' @include side-coord-cartesian.R
#' @include ggside.R
#' @include side-facet_.R
#' @include side-layout-.r
NULL
### INCLUDE END


as_ggside <- function(x, ...) UseMethod('as_ggside')
as_ggside.default <- function(x, ...) abort(glue("No as_ggside() method for class <", glue_collapse(class(x), sep = "/"),">"))
as_ggside.ggplot <- function(x, ggside = NULL, ...) {
  if(inherits(x[['coordinates']], "CoordFlip")||inherits(x[['coordinates']], "CoordPolar")){
    abort("ggside is not currently compatable with CoordFlip or CoordPolar")
  }
  ggside <- ggside %||% ggside()
  if(!is.ggside_options(ggside)) stop("argument ggside must be of class `ggside_options` or NULL")
  class(x) <- c("ggside", class(x))
  x[['ggside']] <- ggside
  update_ggside(x)
}
as_ggside.ggside <- function(x, ggside = NULL, ...) {
  ggside <- ggside %||% x[['ggside']] %||% ggside()
  if(!is.ggside_options(ggside)) stop("argument ggside must be of class `ggside_options` or NULL")
  update_ggside(x, ggside)
}

update_ggside <- function(object, ggside) UseMethod('update_ggside')
update_ggside.default <- function(object, ggside) abort(glue("No update_ggside() method for class <", glue_collapse(class(object), sep = "/"),">"))
update_ggside.ggplot <- function(object, ggside = NULL){
  object$ggside$x.pos <- ggside$x.pos %||% object$ggside$x.pos %||% "top"
  if(!object$ggside$x.pos%in%c("top","bottom")) {
    abort("x.pos may only be \"top\" or \"bottom\".")
  }
  object$ggside$y.pos <- ggside$y.pos %||% object$ggside$y.pos %||% "right"
  if(!object$ggside$y.pos%in%c("right","left")) {
    abort("y.pos may only be \"right\" or \"left\".")
  }
  object$ggside$scales <- ggside$scales %||% object$ggside$scales %||% "fixed"
  if(!object$ggside$scales%in%c("fixed","free","free_x","free_y")){
    abort("scales may only be \"fixed\", \"free\", \"free_x\" or \"free_y\".")
  }
  object$ggside$sides_used <- get_sides(object[["layers"]])
  object$ggside$collapse <- ggside$collapse %||% object$ggside$collapse %||% NULL
  object$ggside$xsidey <- ggside$xsidey %||% object$ggside$xsidey %||% NULL
  object$ggside$ysidex <- ggside$ysidex %||% object$ggside$ysidex %||% NULL
  object$ggside$draw_x_on <- ggside$draw_x_on %||% object$ggside$draw_x_on %||% "default"
  object$ggside$draw_y_on <- ggside$draw_y_on %||% object$ggside$draw_y_on %||% "default"
  object$ggside$strip <- ggside$strip %||% object$ggside$strip %||% "default"
  object$ggside$respect_side_labels <- ggside$respect_side_labels %||% object$ggside$respect_side_labels %||% "default"

  object$facet <- ggside_facet(object$facet, object$ggside)
  object$coordinates <- ggside_coord(object$coordinates)
  object$layout <- ggside_layout(object$layout)
  return(object)
}


get_sides <- function(layers){
  layer_mappings <- lapply(layers, layer_type)
  sides_used <- unlist(layer_mappings)
  sides_used <- unique(sides_used[!sides_used %in% "main"])
  return(sides_used)
}





### INCLUDE BEGIN
#' @include compat-plyr.R
#' @include ggside.R
#' @include side-layer.R
#' @include constructor-.R
NULL
### INCLUDE END
#' @aliases geom_*vline
#' @rdname geom_xsideabline
#' @export
geom_xsidevline <- ggside_layer_function(fun = geom_vline, side = "x")

#' @rdname geom_xsideabline
#' @export
geom_ysidevline <- ggside_layer_function(fun = geom_vline, side = "y")

#' @rdname ggside-ggproto-geoms
#' @format NULL
#' @usage NULL
#' @export
GeomXsidevline <- ggside_geom("GeomXsidevline", GeomVline, "x")

#' @rdname ggside-ggproto-geoms
#' @format NULL
#' @usage NULL
#' @export
GeomYsidevline <- ggside_geom("GeomYsidevline", GeomVline, "y")

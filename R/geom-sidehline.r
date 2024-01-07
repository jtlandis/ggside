### INCLUDE BEGIN
#' @include compat-plyr.R
#' @include ggside.R
#' @include side-layer.R
#' @include constructor-.R
NULL
### INCLUDE END
#' @aliases geom_*hline
#' @rdname geom_xsideabline
#' @export
geom_xsidehline <- ggside_layer_function(fun = geom_hline, side = "x")


#' @rdname geom_xsideabline
#' @export
geom_ysidehline <- ggside_layer_function(fun = geom_hline, side = "y")

#' @rdname ggside-ggproto-geoms
#' @format NULL
#' @usage NULL
#' @export
GeomXsidehline <- ggside_geom("GeomXsidehline", GeomHline, "x")

#' @rdname ggside-ggproto-geoms
#' @format NULL
#' @usage NULL
#' @export
GeomYsidehline <- ggside_geom("GeomYsidehline", GeomHline, "y")

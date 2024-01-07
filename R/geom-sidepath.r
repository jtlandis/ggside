### INCLUDE BEGIN
#' @include constructor-2.R
NULL
### INCLUDE END
#' @rdname geom_xsideline
#' @export
geom_xsidepath <- ggside_layer_function(fun = geom_path, side = "x")

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidepath <- ggside_geom("GeomXsidepath", GeomPath, "x")


#' @rdname geom_xsideline
#' @export
geom_ysidepath <- ggside_layer_function(fun = geom_path, side = "y")

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidepath <- ggside_geom("GeomYsidepath", GeomPath, "y")

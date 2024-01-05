#' @rdname geom_xsidebar
#' @export
geom_xsidecol <- ggside_layer_function(fun = geom_col, side = "x")

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidecol <- ggside_geom("GeomXsidecol", GeomCol, "x")

#' @rdname geom_xsidebar
#' @export
geom_ysidecol <- ggside_layer_function(fun = geom_col, side = "y", orientation = "y")

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidecol <- ggside_geom("GeomYsidecol", GeomCol, "y")

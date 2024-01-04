#' @title Side text
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{geom_text}.
#' @inheritParams ggplot2::geom_text
#' @aliases geom_*sidetext
#' @return XLayer or YLayer object to be added to a ggplot object
#' @export
geom_xsidetext <- ggside_layer_function(fun = geom_text, side = "x")

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidetext <- ggside_geom("GeomXsidetext", GeomText, "x")


#' @rdname geom_xsidetext
#' @export
geom_ysidetext <- ggside_layer_function(fun = geom_text, side = "y")

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidetext <- ggside_geom("GeomYsidetext", GeomText, "y")

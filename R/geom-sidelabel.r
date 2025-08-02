### INCLUDE BEGIN
#' @include constructor-.R
NULL
### INCLUDE END
#' @title Side label
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{geom_label}.
#' @inheritParams ggplot2::geom_label
#' @aliases geom_*sidelabel
#' @return XLayer or YLayer object to be added to a ggplot object
#' @export
geom_xsidelabel <- ggside_layer_function(fun = geom_label, side = "x", force_missing = c("nudge_x", "nudge_y", "position"), label.size = quote(lifecycle::deprecated()))

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidelabel <- ggside_geom("GeomXsidelabel", GeomLabel, "x")


#' @rdname geom_xsidelabel
#' @export
geom_ysidelabel <- ggside_layer_function(fun = geom_label, side = "y", force_missing = c("nudge_x", "nudge_y", "position"), label.size = quote(lifecycle::deprecated()))

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidelabel <- ggside_geom("GeomYsidelabel", GeomLabel, "y")

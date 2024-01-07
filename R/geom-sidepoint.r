### INCLUDE BEGIN
#' @include constructor-2.R
NULL
### INCLUDE END
#' @title  Side Points
#'
#' @description
#' The ggside variants of \link[ggplot2]{geom_point} is [geom_xsidepoint()] and
#' [geom_ysidepoint()]. Both variants inherit from \link[ggplot2]{geom_point},
#' thus the only difference is where the data is plotted. The `xside` variant will
#' plot data along the x-axis, while the `yside` variant will plot data along the
#' y-axis.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#'
#' @aliases geom_*sidepoint
#' @return XLayer or YLayer object to be added to a ggplot object
#' @examples
#' ggplot(diamonds, aes(depth, table, alpha = .2)) +
#'   geom_point() +
#'   geom_ysidepoint(aes(x = price)) +
#'   geom_xsidepoint(aes(y = price)) +
#'   theme(
#'         ggside.panel.scale = .3
#'     )
#' @export
geom_xsidepoint <- ggside_layer_function(fun = geom_point, side = "x")

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidepoint <- ggside_geom("GeomXsidepoint", GeomPoint, "x")

#' @rdname geom_xsidepoint
#' @export
geom_ysidepoint <- ggside_layer_function(fun = geom_point, side = "y")

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidepoint <- ggside_geom("GeomYsidepoint", GeomPoint, "y")


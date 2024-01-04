#' @title Side density distributions
#'
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{geom_density} is
#' [geom_xsidedensity] and [geom_ysidedensity].
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_bar
#' @inheritParams ggplot2::geom_ribbon
#' @param stat Use to override the default connection between
#'   `geom_density()` and `stat_density()`.
#' @aliases geom_*sidedensity
#' @return XLayer or YLayer object to be added to a ggplot object
#' @examples
#'
#' ggplot(mpg, aes(displ, hwy, colour = class)) +
#'  geom_point(size = 2) +
#'  geom_xsidedensity() +
#'  geom_ysidedensity() +
#'  theme(axis.text.x = element_text(angle = 90, vjust = .5))
#'
#' ggplot(mpg, aes(displ, hwy, colour = class)) +
#'  geom_point(size = 2) +
#'  geom_xsidedensity(aes(y = after_stat(count)),position = "stack") +
#'  geom_ysidedensity(aes(x = after_stat(scaled))) +
#'  theme(axis.text.x = element_text(angle = 90, vjust = .5))
#'
#' @export
geom_xsidedensity <- ggside_layer_function(fun = geom_density, side = "x")

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidedensity <- ggside_geom("GeomXsidedensity", GeomDensity, "x")

#' @rdname geom_xsidedensity
#' @export
geom_ysidedensity <- ggside_layer_function(fun = geom_density, side = "y")

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidedensity <- ggside_geom("GeomYsidedensity", GeomDensity, "y")

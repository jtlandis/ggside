### INCLUDE BEGIN
#' @include compat-plyr.R
#' @include ggside.R
#' @include side-layer.R
#' @include constructor-.R
NULL
### INCLUDE END
#' @title Side line plot
#' @description
#' The [xside] and [yside] of \link[ggplot2]{geom_line}.
#' The [xside] and [yside] variants of \link[ggplot2]{geom_path}
#' @inheritParams ggplot2::geom_line
#'
#' @aliases geom_*sideline
#' @return XLayer or YLayer object to be added to a ggplot object
#' @examples
#' #sideline
#' ggplot(economics, aes(date, pop)) +
#'   geom_xsideline(aes(y = unemploy)) +
#'   geom_col()
#' @export
geom_xsideline <- ggside_layer_function(fun = geom_line, side = "x")

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsideline <- ggside_geom("GeomXsideline", GeomLine, "x")


#' @rdname geom_xsideline
#' @export
geom_ysideline <- ggside_layer_function(fun = geom_line, side = "y")


#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsideline <- ggside_geom("GeomYsideline", GeomLine, "y")


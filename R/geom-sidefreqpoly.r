### INCLUDE BEGIN
#' @include constructor-.R
NULL
### INCLUDE END
#' @title Side Frequency Polygons
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{geom_freqpoly} is
#' [geom_xsidefreqpoly] and [geom_ysidefreqpoly].
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_freqpoly
#'
#' @aliases geom_*freqpoly
#' @return XLayer or YLayer object to be added to a ggplot object
#' @examples
#'  ggplot(diamonds, aes(price, carat, colour = cut)) +
#'    geom_point() +
#'    geom_xsidefreqpoly(aes(y=after_stat(count)),binwidth = 500) +
#'    geom_ysidefreqpoly(aes(x=after_stat(count)),binwidth = .2)
#' @export
geom_xsidefreqpoly <- ggside_layer_function(fun = geom_freqpoly, side = "x", stat_orientation = "x")

#' @rdname geom_xsidefreqpoly
#' @export
geom_ysidefreqpoly <- ggside_layer_function(fun = geom_freqpoly, side = "y", stat_orientation = "y")

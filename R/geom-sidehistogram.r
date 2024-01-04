
#' Side Histograms
#'
#' The [xside] and [yside] variants of \link[ggplot2]{geom_histogram} is
#' [geom_xsidehistogram] and [geom_ysidehistogram]. These variants both inherit
#' from \link[ggplot2]{geom_histogram} and only differ on where they plot
#' data relative to main panels.
#'
#' @section Aesthetics:
#' `geom_*sidehistogram` uses the same aesthetics as [geom_*sidebar()]
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_histogram
#'
#' @aliases geom_*sidehistogram
#' @return XLayer or YLayer object to be added to a ggplot object
#' @examples
#'
#' p <-ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species, fill = Species)) +
#' geom_point()
#'
#' #sidehistogram
#' p +
#' geom_xsidehistogram(binwidth = 0.1) +
#' geom_ysidehistogram(binwidth = 0.1)
#' p +
#' geom_xsidehistogram(aes(y = after_stat(density)), binwidth = 0.1) +
#' geom_ysidehistogram(aes(x = after_stat(density)), binwidth = 0.1)
#' @export
geom_xsidehistogram <- ggside_layer_function(fun = geom_histogram, side = "x")

#' @rdname geom_xsidehistogram
#' @aliases geom_ysidehistogram
#' @export
geom_ysidehistogram <- ggside_layer_function(fun = geom_histogram, side = "y")

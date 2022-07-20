
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
geom_xsidehistogram <- function(mapping = NULL, data = NULL,
                                stat = "bin", position = "stack",
                                ...,
                                binwidth = NULL,
                                bins = NULL,
                                na.rm = FALSE,
                                orientation = "x",
                                show.legend = NA,
                                inherit.aes = TRUE) {
  mapping <- default_stat_aes(mapping, stat, orientation)
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsidebar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      binwidth = binwidth,
      bins = bins,
      na.rm = na.rm,
      orientation = orientation,
      pad = FALSE,
      ...
    ),
    layer_class = XLayer
  )
  structure(l, class = c("ggside_layer",class(l)))
}

#' @rdname geom_xsidehistogram
#' @aliases geom_ysidehistogram
#' @export
geom_ysidehistogram <- function(mapping = NULL, data = NULL,
                                stat = "bin", position = "stack",
                                ...,
                                binwidth = NULL,
                                bins = NULL,
                                na.rm = FALSE,
                                orientation = "y",
                                show.legend = NA,
                                inherit.aes = TRUE) {
  mapping <- default_stat_aes(mapping, stat, orientation)
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsidebar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      binwidth = binwidth,
      bins = bins,
      na.rm = na.rm,
      orientation = orientation,
      pad = FALSE,
      ...
    ),
    layer_class = YLayer
  )
  structure(l, class = c("ggside_layer",class(l)))
}

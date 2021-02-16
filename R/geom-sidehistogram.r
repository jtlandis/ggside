
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
#' @inheritParams ggplot2::geom_histogram
#'
#' @aliases geom_*sidehistogram
#' @export
geom_xsidehistogram <- function(mapping = NULL, data = NULL,
                                stat = "bin", position = "stack",
                                ...,
                                binwidth = NULL,
                                bins = NULL,
                                na.rm = FALSE,
                                orientation = NA,
                                show.legend = NA,
                                inherit.aes = TRUE) {

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
  structure(list(layer=l), class = c("ggside_layer",class(l)))
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
                                orientation = NA,
                                show.legend = NA,
                                inherit.aes = TRUE) {

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

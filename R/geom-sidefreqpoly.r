#' @title Side Frequency Polygons
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{geom_freqpoly} is
#' [geom_xsidefreqpoly] and [geom_ysidefreqpoly].
#'
#' @inheritParams ggplot2::geom_freqpoly
#'
#' @aliases geom_*freqpoly
#' @return XLayer or YLayer object to be added to a ggplot object
#' @export
geom_xsidefreqpoly <- function(mapping = NULL, data = NULL,
                               stat = "xsidebin", position = "identity",
                               ...,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {

  params <- list(na.rm = na.rm, ...)
  if (identical(stat, "bin")) {
    params$pad <- TRUE
  }

  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsidepath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params,
    layer_class = XLayer
  )
  structure(l, class = c("ggside_layer",class(l)))

}

#' @rdname geom_xsidefreqpoly
#' @export
geom_ysidefreqpoly <- function(mapping = NULL, data = NULL,
                               stat = "ysidebin", position = "identity",
                               ...,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {

  params <- list(na.rm = na.rm, ...)
  if (identical(stat, "bin")) {
    params$pad <- TRUE
  }

  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsidepath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params,
    layer_class = YLayer
  )
  structure(l, class=c("ggside_layer", class(l)))
}

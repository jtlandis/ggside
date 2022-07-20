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
geom_xsidefreqpoly <- function(mapping = NULL, data = NULL,
                               stat = "bin", position = "identity",
                               ...,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {
  mapping <- default_stat_aes(mapping, stat, "x")
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
                               stat = "bin", position = "identity",
                               ...,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {
  mapping <- default_stat_aes(mapping, stat, "y")
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

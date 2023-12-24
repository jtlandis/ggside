#' @title Side Violin plots
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{geom_violin}
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_bar
#' @param draw_quantiles If `not(NULL)` (default), draw horizontal lines
#'   at the given quantiles of the density estimate.
#' @param trim If `TRUE` (default), trim the tails of the violins
#'   to the range of the data. If `FALSE`, don't trim the tails.
#' @param scale if "area" (default), all violins have the same area
#' (before trimming the tails). If "count", areas are scaled proportionally
#'  to the number of observations. If "width", all violins have the same
#'  maximum width.
#' @param stat Use to override the default connection between
#'   `geom_violin()` and `stat_ydensity()`.
#' @aliases geom_*sideviolin
#' @seealso [geom_*sideboxplot]
#' @examples
#' df <- expand.grid(UpperCase = LETTERS, LowerCase = letters)
#' df$Combo_Index <- as.integer(df$UpperCase)*as.integer(df$LowerCase)
#'
#' p1 <- ggplot(df, aes(UpperCase, LowerCase)) +
#' geom_tile(aes(fill = Combo_Index))
#'
#' #sideviolins
#' #Note - Mixing discrete and continuous axis scales
#' #using xsideviolins when the y aesthetic was previously
#' #mapped with a continuous varialbe will prevent
#' #any labels from being plotted. This is a feature that
#' #will hopefully be added to ggside in the future.
#'
#' p1 + geom_xsideviolin(aes(y = Combo_Index)) +
#'    geom_ysideviolin(aes(x = Combo_Index))
#'
#' #sideviolins with swapped orientation
#' #Note - Discrete before Continuous
#' #If you are to mix Discrete and Continuous variables on
#' #one axis, ggplot2 prefers the discrete variable to be mapped
#' #BEFORE the continuous.
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'     geom_xsideviolin(aes(y = Species), orientation = "y") +
#'     geom_point()
#'
#' #Alternatively, you can recast the value as a factor and then
#' # a numeric
#'
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species))+
#'     geom_point() +
#'     geom_xsideviolin(aes(y = as.numeric(Species)), orientation = "y") +
#'     geom_ysideviolin(aes(x = as.numeric(Species)), orientation = "x")
#'
#' @return XLayer or YLayer object to be added to a ggplot object
#' @export
geom_xsideviolin <- function(mapping = NULL, data = NULL,
                              stat = "ydensity", position = "dodge",
                              ...,
                              draw_quantiles = NULL,
                              trim = TRUE,
                              scale = "area",
                              na.rm = FALSE,
                              orientation = NA,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  ggside_layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsideviolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      draw_quantiles = draw_quantiles,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsideviolin <- ggside_geom("GeomXsideviolin", GeomViolin, "x")

#' @rdname geom_xsideviolin
#' @export
geom_ysideviolin <- function(mapping = NULL, data = NULL,
                             stat = "ydensity", position = "dodge",
                             ...,
                             draw_quantiles = NULL,
                             trim = TRUE,
                             scale = "area",
                             na.rm = FALSE,
                             orientation = "y",
                             show.legend = NA,
                             inherit.aes = TRUE) {
  ggside_layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsideviolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      draw_quantiles = draw_quantiles,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsideviolin <- ggside_geom("GeomYsideviolin", GeomViolin, "y")

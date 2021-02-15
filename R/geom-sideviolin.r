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
  layer(
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
    ),
    layer_class = XLayer
  )
}


GeomXsideviolin <- ggplot2::ggproto("GeomXsideviolin",
                                    ggplot2::GeomViolin,
                                    default_aes = aes(weight = 1, colour = "grey20", xcolour = NA,
                                                      fill = "white", xfill = NA, size = 0.5,
                                                      alhpa = NA, linetype = "solid"),
                                    setup_data = function(data, params){
                                      data <- parse_side_aes(data, params)
                                      ggplot2::GeomViolin$setup_data(data, params)
                                    },
                                    draw_panel = function(self, data, panel_params, coord, ...){
                                      data <- use_xside_aes(data)
                                      ggplot2::GeomViolin$draw_panel(data = data, panel_params, coord = coord, ...)
                                    },
                                    draw_key = function(data, params, size){
                                      data <- use_xside_aes(data)
                                      ggplot2::GeomViolin$draw_key(data, params, size)
                                    })


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
  layer(
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
    ),
    layer_class = YLayer
  )
}


GeomYsideviolin <- ggplot2::ggproto("GeomYsideviolin",
                                    ggplot2::GeomViolin,
                                    default_aes = aes(weight = 1, colour = "grey20", ycolour = NA,
                                                      fill = "white", yfill = NA, size = 0.5,
                                                      alhpa = NA, linetype = "solid"),
                                    setup_data = function(data, params){
                                      data <- parse_side_aes(data, params)
                                      ggplot2::GeomViolin$setup_data(data, params)
                                    },
                                    draw_panel = function(self, data, panel_params, coord, ...){
                                      data <- use_yside_aes(data)
                                      ggplot2::GeomViolin$draw_panel(data = data, panel_params, coord = coord, ...)
                                    },
                                    draw_key = function(data, params, size){
                                      data <- use_yside_aes(data)
                                      ggplot2::GeomViolin$draw_key(data, params, size)
                                    })

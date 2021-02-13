
#' @export
geom_xsidepoint <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsidepoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    ),
    layer_class = XLayer
  )
  structure(list(layer = l), class = c("ggside_layer",class(l)))
}

GeomXsidepoint <- ggplot2::ggproto("GeomXsidepoint",
                                   ggplot2::GeomPoint,
                                   default_aes = aes(shape = 19, colour = "black", xcolour = NA,
                                                     size = 1.5, fill = NA, xfill = NA,
                                                     alpha = NA, stroke = 0.5),
                                   setup_data = function(data, params){
                                     data <- parse_side_aes(data, params)
                                     ggplot2::GeomPoint$setup_data(data, params)
                                   },
                                   draw_panel = function(data, panel_params, coord, na.rm = FALSE){
                                     data <- use_xside_aes(data)
                                     ggplot2::GeomPoint$draw_panel(data = data, panel_params = panel_params,
                                                                   coord = coord, na.rm = na.rm)
                                   },
                                   draw_key = function(data, params, size){
                                     data <- use_xside_aes(data)
                                     ggplot2::GeomPoint$draw_key(data, prams, size)
                                   }
                                   )

#' @export
geom_ysidepoint <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsidepoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    ),
    layer_class = YLayer
  )
  structure(list(layer = l), class = c("ggside_layer",class(l)))
}

GeomYsidepoint <- ggplot2::ggproto("GeomYsidepoint",
                                   ggplot2::GeomPoint,
                                   default_aes = aes(shape = 19, colour = "black", ycolour = NA,
                                                     size = 1.5, fill = NA, yfill = NA,
                                                     alpha = NA, stroke = 0.5),
                                   setup_data = function(data, params){
                                     data <- parse_side_aes(data, params)
                                     ggplot2::GeomPoint$setup_data(data, params)
                                   },
                                   draw_panel = function(data, panel_params, coord, na.rm = FALSE){
                                     data <- use_yside_aes(data)
                                     ggplot2::GeomPoint$draw_panel(data = data, panel_params = panel_params,
                                                                   coord = coord, na.rm = na.rm)
                                   },
                                   draw_key = function(data, params, size){
                                     data <- use_yside_aes(data)
                                     ggplot2::GeomPoint$draw_key(data, prams, size)
                                   }
)



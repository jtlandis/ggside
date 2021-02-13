
#' @export
geom_xsideline <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE, orientation = NA,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsideline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      ...
    ),
    layer_class = XLayer
  )
  structure(l, class = c("ggside_layer",class(l)))
}


GeomXsideline <- ggplot2::ggproto("GeomXsideline",
                                  ggplot2::GeomLine,
                                  default_aes = aes(colour = "black", xcolour = NA, size = 0.5,
                                                    linetype = 1, alpha = NA),
                                  setup_data = function(data, params){
                                    data <- parse_side_aes(data, params)
                                    ggplot2::GeomLine$setup_data(data, params)
                                  },
                                  draw_panel = function(data, panel_params, coord, arrow = NULL,
                                                        lineend = "butt", linejoin = "round",
                                                        linemitre = 10, na.rm = FALSE){
                                    data <- use_xside_aes(data)
                                    ggplot2::GeomLine$draw_panel(data = data, panel_params = panel_params,
                                                                 coord = coord, arrow = arrow, lineend = lineend,
                                                                 linejoin = linejoin, linemitre = linemitre, na.rm = na.rm)
                                  },
                                  draw_key = function(data, params, size){
                                    data <- use_xside_aes(data)
                                    ggplot2::GeomLine$draw_key(data, params, size)
                                  })



#' @export
geom_ysideline <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE, orientation = NA,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsideline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      ...
    ),
    layer_class = YLayer
  )
  structure(l, class = c("ggside_layer",class(l)))
}


GeomYsideline <- ggplot2::ggproto("GeomYsideline",
                                  ggplot2::GeomLine,
                                  default_aes = aes(colour = "black", ycolour = NA, size = 0.5,
                                                    linetype = 1, alpha = NA),
                                  setup_data = function(data, params){
                                    data <- parse_side_aes(data, params)
                                    ggplot2::GeomLine$setup_data(data, params)
                                  },
                                  draw_panel = function(data, panel_params, coord, arrow = NULL,
                                                        lineend = "butt", linejoin = "round",
                                                        linemitre = 10, na.rm = FALSE){
                                    data <- use_yside_aes(data)
                                    ggplot2::GeomLine$draw_panel(data = data, panel_params = panel_params,
                                                                 coord = coord, arrow = arrow, lineend = lineend,
                                                                 linejoin = linejoin, linemitre = linemitre, na.rm = na.rm)
                                  },
                                  draw_key = function(data, params, size){
                                    data <- use_yside_aes(data)
                                    ggplot2::GeomLine$draw_key(data, params, size)
                                  })



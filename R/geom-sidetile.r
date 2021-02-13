
#' @export
geom_xsidetile <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          linejoin = "mitre",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsidetile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    ),
    layer_class = XLayer
  )
  structure(list(layer = l), class = class("ggside_layer",l))
}

GeomXsidetile <- ggplot2::ggproto("GeomXsidetile",
                                  ggplot2::GeomTile,
                                  default_aes = aes(fill = "grey20", xfill = NA,
                                                    colour = NA, xcolour = NA,
                                                    size = 0.1, linetype = 1, alpha = NA,
                                                    width = NA, height = NA),
                                  setup_data = function(data, params) {
                                    data <- parse_side_aes(data, params)
                                    ggplot2::GeomTile$setup_data(data, params)
                                  },
                                  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
                                    data <- use_xside_aes(data)
                                    ggplot2::GeomTile$draw_panel(data = data, panel_params = panel_params, coord = coord, linejoin = linejoin)
                                  },
                                  draw_key = function(data, params, size){
                                    data <- use_xside_aes(data)
                                    ggplot2::GeomTile$draw_key(data, params, size)
                                  })


#' @export
geom_ysidetile <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           linejoin = "mitre",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsidetile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    ),
    layer_class = YLayer
  )
  structure(list(layer = l), class = class("ggside_layer",l))
}

GeomYsidetile <- ggplot2::ggproto("GeomYsidetile",
                                  ggplot2::GeomTile,
                                  default_aes = aes(fill = "grey20", yfill = NA,
                                                    colour = NA, ycolour = NA,
                                                    size = 0.1, linetype = 1, alpha = NA,
                                                    width = NA, height = NA),
                                  setup_data = function(data, params) {
                                    data <- parse_side_aes(data, params)
                                    ggplot2::GeomTile$setup_data(data, params)
                                  },
                                  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
                                    #browser()
                                    data <- use_yside_aes(data)
                                    ggplot2::GeomTile$draw_panel(data = data, panel_params = panel_params, coord = coord, linejoin = linejoin)
                                  },
                                  draw_key = function(data, params, size){
                                    data <- use_yside_aes(data)
                                    ggplot2::GeomTile$draw_key(data, params, size)
                                  })

#' @rdname geom_xsideline
#' @export
geom_xsidepath <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           lineend = "butt",
                           linejoin = "round",
                           linemitre = 10,
                           arrow = NULL,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  mapping <- force_panel_type_mapping(mapping, "x")
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsidepath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      na.rm = na.rm,
      ...
    ),
    layer_class = XLayer
  )
  structure(l, class = c("ggside_layer",class(l)))
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidepath <- ggplot2::ggproto("GeomXsidepath",
                                  ggplot2::GeomPath,
                                  default_aes = aes(
                                    !!!ggplot2::GeomPath$default_aes,
                                    xcolour = NA, xfill = NA, PANEL_TYPE = "x"
                                  ),
                                  setup_data = function(data, params){
                                    data <- parse_side_aes(data, params)
                                    ggplot2::GeomPath$setup_data(data, params)
                                  },
                                  draw_panel = function(data, panel_params, coord, arrow = NULL,
                                                        lineend = "butt", linejoin = "round", linemitre = 10,
                                                        na.rm = FALSE){
                                    data <- use_xside_aes(data)
                                    ggplot2::GeomPath$draw_panel(data = data, panel_params, coord = coord, arrow = arrow,
                                                                 lineend = lineend, linejoin = linejoin, linemitre = linemitre)
                                  },
                                  draw_key = function(data, params, size){
                                    data <- use_xside_aes(data)
                                    ggplot2::GeomPath$draw_key(data, params, size)
                                  })



#' @rdname geom_xsideline
#' @export
geom_ysidepath <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           lineend = "butt",
                           linejoin = "round",
                           linemitre = 10,
                           arrow = NULL,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  mapping <- force_panel_type_mapping(mapping, "y")
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsidepath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      na.rm = na.rm,
      ...
    ),
    layer_class = YLayer
  )
  structure(l, class = c("ggside_layer",class(l)))
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidepath <- ggplot2::ggproto("GeomYsidepath",
                                  ggplot2::GeomPath,
                                  default_aes = aes(
                                    !!!ggplot2::GeomPath$default_aes,
                                    ycolour = NA, yfill = NA, PANEL_TYPE = "y"
                                  ),
                                  setup_data = function(data, params){
                                    data <- parse_side_aes(data, params)
                                    ggplot2::GeomPath$setup_data(data, params)
                                  },
                                  draw_panel = function(data, panel_params, coord, arrow = NULL,
                                                        lineend = "butt", linejoin = "round", linemitre = 10,
                                                        na.rm = FALSE){
                                    data <- use_yside_aes(data)
                                    ggplot2::GeomPath$draw_panel(data = data, panel_params, coord = coord, arrow = arrow,
                                                                 lineend = lineend, linejoin = linejoin, linemitre = linemitre)
                                  },
                                  draw_key = function(data, params, size){
                                    data <- use_yside_aes(data)
                                    ggplot2::GeomPath$draw_key(data, params, size)
                                  })





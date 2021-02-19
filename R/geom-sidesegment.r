#' @title Side line Segments
#' @description
#'  The [xside] and [yside] of \link[ggplot2]{geom_segment}.
#' @inheritParams ggplot2::geom_segment
#'
#' @aliases geom_*sidesegment
#' @export
geom_xsidesegment <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                               ..., arrow = NULL, arrow.fill = NULL, lineend = "butt", linejoin = "round",
                               na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
{
  l <- layer(data = data, mapping = mapping,
             stat = stat, geom = GeomXsidesegment,
             position = position, show.legend = show.legend,
             inherit.aes = inherit.aes,
             params = list(arrow = arrow,
                           arrow.fill = arrow.fill,
                           lineend = lineend,
                           linejoin = linejoin,
                           na.rm = na.rm,
                      ...),
             layer_class = XLayer)
  structure(l, class = c("ggside_layer",class(l)))
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidesegment <- ggplot2::ggproto("GeomXsidesegment",
                                  ggplot2::GeomSegment,
                                  default_aes = aes(colour = "black",
                                                    xcolour = NA, size = 0.5,
                                                    linetype = 1, alpha = NA),
                                  setup_data = function(data, params){
                                    data <- parse_side_aes(data, params)
                                    ggplot2::GeomSegment$setup_data(data, params)
                                  },
                                  draw_panel = function(data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                                                        lineend = "butt", linejoin = "round", na.rm = FALSE){
                                    data <- use_xside_aes(data)
                                    ggplot2::GeomSegment$draw_panel(data = data, panel_params = panel_params,
                                                                 coord = coord, arrow = arrow, arrow.fill = arrow.fill,
                                                                 lineend = lineend, linejoin = linejoin,  na.rm = na.rm)
                                  },
                                  draw_key = function(data, params, size){
                                    data <- use_xside_aes(data)
                                    ggplot2::GeomSegment$draw_key(data, params, size)
                                  })

#' @rdname geom_xsidesegment
#' @export
geom_ysidesegment <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                               ..., arrow = NULL, arrow.fill = NULL, lineend = "butt", linejoin = "round",
                               na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
{
  l <- layer(data = data, mapping = mapping,
             stat = stat, geom = GeomYsidesegment,
             position = position, show.legend = show.legend,
             inherit.aes = inherit.aes,
             params = list(arrow = arrow,
                           arrow.fill = arrow.fill,
                           lineend = lineend,
                           linejoin = linejoin,
                           na.rm = na.rm,
                           ...),
             layer_class = YLayer)
  structure(l, class=c("ggside_layer",class(l)))
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidesegment <- ggplot2::ggproto("GeomYsidesegment",
                                     ggplot2::GeomSegment,
                                     default_aes = aes(colour = "black",
                                                       ycolour = NA, size = 0.5,
                                                       linetype = 1, alpha = NA),
                                     setup_data = function(data, params){
                                       data <- parse_side_aes(data, params)
                                       ggplot2::GeomSegment$setup_data(data, params)
                                     },
                                     draw_panel = function(data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                                                           lineend = "butt", linejoin = "round", na.rm = FALSE){
                                       data <- use_yside_aes(data)
                                       ggplot2::GeomSegment$draw_panel(data = data, panel_params = panel_params,
                                                                       coord = coord, arrow = arrow, arrow.fill = arrow.fill,
                                                                       lineend = lineend, linejoin = linejoin,  na.rm = na.rm)
                                     },
                                     draw_key = function(data, params, size){
                                       data <- use_yside_aes(data)
                                       ggplot2::GeomSegment$draw_key(data, params, size)
                                     })


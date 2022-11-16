#' @title Side line plot
#' @description
#' The [xside] and [yside] of \link[ggplot2]{geom_line}.
#' The [xside] and [yside] variants of \link[ggplot2]{geom_path}
#' @inheritParams ggplot2::geom_line
#'
#' @aliases geom_*sideline
#' @return XLayer or YLayer object to be added to a ggplot object
#' @examples
#' #sideline
#' ggplot(economics, aes(date, pop)) +
#'   geom_xsideline(aes(y = unemploy)) +
#'   geom_col()
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

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsideline <- ggplot2::ggproto("GeomXsideline",
                                  ggplot2::GeomLine,
                                  default_aes = new_default_aes(
                                    aes(xcolour = NA, xfill = NA),
                                    ggplot2::GeomLine$default_aes
                                  ),
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


#' @rdname geom_xsideline
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


#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsideline <- ggplot2::ggproto("GeomYsideline",
                                  ggplot2::GeomLine,
                                  default_aes = new_default_aes(
                                    aes(ycolour = NA, yfill = NA),
                                    ggplot2::GeomLine$default_aes
                                  ),
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



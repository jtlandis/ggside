#' @title Side function plot
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{geom_function}
#' @inheritParams ggplot2::geom_function
#' @aliases geom_*sidefunction
#' @return XLayer or YLayer object to be added to a ggplot object
#' @examples
#' x<- rweibull(100, 2.6, 3)
#' y<- rweibull(100, 1.8, 3)
#' xy.df<- data.frame(cbind(x,y))
#' p <- ggplot(xy.df, aes(x, y)) +
#'   geom_point(colour = "blue", size = 0.25) +
#'   geom_density2d() +
#'   geom_xsidedensity(fill = "blue", alpha = .3) +
#'   geom_ysidedensity(fill = "blue", alpha = .3) +
#'   stat_xsidefunction(fun = dweibull, args = list(shape = 1.8, scale = 3), colour = "red") +
#'   stat_ysidefunction(fun = dweibull, args = list(shape = 2.6, scale = 3), colour = "red") +
#'   theme_classic()
#' p
#' @export
geom_xsidefunction <- function(mapping = NULL, data = NULL,
                           stat = "function", position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsidefunction,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    ),
    layer_class = XLayer
  )
  structure(l, class = c("ggside_layer",class(l)))
}

#' @rdname geom_xsidefunction
#' @export
stat_xsidefunction <- function(mapping = NULL, data = NULL, geom = "xsidefunction", position = "identity",
                               ..., fun, xlim = NULL, n = 101, args = list(), na.rm = FALSE,
                               show.legend = NA, inherit.aes = TRUE) {

  l <- layer(data = data, mapping = mapping, stat = StatFunction,
        geom = geom, position = position, show.legend = show.legend,
        inherit.aes = inherit.aes, params = list(fun = fun, n = n,
                                                 args = args, na.rm = na.rm, xlim = xlim, ...),
        layer_class = XLayer)
  structure(l, class = c("ggside_layer", class(l)))
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidefunction <- ggplot2::ggproto("GeomXsidefunction",
                                  ggplot2::GeomFunction,
                                  default_aes = aes(colour = "black", xcolour = NA, size = 0.5,
                                                    linetype = 1, alpha = NA),
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



#' @rdname geom_xsidefunction
#' @export
geom_ysidefunction <- function(mapping = NULL, data = NULL,
                           stat = "ysidefunction", position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsidefunction,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    ),
    layer_class = YLayer
  )
  structure(l, class = c("ggside_layer",class(l)))
}

#' @rdname geom_xsidefunction
#' @param ylim Optionally, reestrict the range of the function to this range (y-axis)
#' @export
stat_ysidefunction <- function(mapping = NULL, data = NULL, geom = "ysidefunction", position = "identity",
                               ..., fun, ylim = NULL, n = 101, args = list(), na.rm = FALSE,
                               show.legend = NA, inherit.aes = TRUE) {

  l <- layer(data = data, mapping = mapping, stat = StatYsidefunction,
             geom = geom, position = position, show.legend = show.legend,
             inherit.aes = inherit.aes, params = list(fun = fun, n = n,
                                                      args = args, na.rm = na.rm, ylim = ylim, ...),
             layer_class = YLayer)
  structure(l, class = c("ggside_layer", class(l)))
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidefunction <- ggplot2::ggproto("GeomYsidefunction",
                                  ggplot2::GeomFunction,
                                  default_aes = aes(colour = "black", ycolour = NA, size = 0.5,
                                                    linetype = 1, alpha = NA),
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



StatYsidefunction <- ggplot2::ggproto("StatYsidefunction",
                                      ggplot2::StatFunction,
                                      compute_group = function (data, scales, fun, ylim = NULL, n = 101, args = list()) {
                                        if (is.null(scales$y)) {
                                          range <- ylim %||% c(0, 1)
                                          yseq <- seq(range[1], range[2], length.out = n)
                                          y_trans <- yseq
                                        }
                                        else {
                                          range <- ylim %||% scales$y$dimension()
                                          yseq <- seq(range[1], range[2], length.out = n)
                                          if (scales$y$is_discrete()) {
                                            y_trans <- yseq
                                          }
                                          else {
                                            y_trans <- scales$y$trans$inverse(yseq)
                                          }
                                        }
                                        if (is.formula(fun))
                                          fun <- as_function(fun)
                                        x_out <- do.call(fun, c(list(quote(y_trans)), args))
                                        if (!is.null(scales$y) && !scales$y$is_discrete()) {
                                          x_out <- scales$y$trans$transform(x_out)
                                        }
                                        new_data_frame(list(x = x_out, y = yseq))
                                      })


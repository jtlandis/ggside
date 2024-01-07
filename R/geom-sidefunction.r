### INCLUDE BEGIN
#' @include ggplot2-reimpl-.R
#' @include constructor-.R
#' @include performance.R
#' @include side-layer.R
NULL
### INCLUDE END
#' @title Side function plot
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{geom_function}
#' @inheritParams ggplot2::geom_function
#' @param ylim Optionally, restrict the range of the function to this range (y-axis)
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
geom_xsidefunction <- ggside_layer_function(fun = geom_function, side = "x")

#' @rdname geom_xsidefunction
#' @export
stat_xsidefunction <- ggside_layer_function(fun = stat_function, side = "x")

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidefunction <- ggside_geom("GeomXsidefunction", GeomFunction, "x")

#' @rdname geom_xsidefunction
#' @export
geom_ysidefunction2 <- ggside_layer_function(fun = geom_function, side = "y")

#' @rdname geom_xsidefunction
#' @export
stat_ysidefunction2 <- ggside_layer_function(fun = stat_function, side = "y")

#' @rdname geom_xsidefunction
#' @export
geom_ysidefunction <- function(mapping = NULL, data = NULL,
                           stat = "ysidefunction", position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {

  ggside_layer(
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
    )
  )
}

#' @rdname geom_xsidefunction
#' @export
stat_ysidefunction <- function(mapping = NULL, data = NULL, geom = "ysidefunction", position = "identity",
                               ..., fun, ylim = NULL, n = 101, args = list(), na.rm = FALSE,
                               show.legend = NA, inherit.aes = TRUE) {

  ggside_layer(data = data, mapping = mapping, stat = StatYsidefunction,
             geom = geom, position = position, show.legend = show.legend,
             inherit.aes = inherit.aes,
             params = list(fun = fun, n = n,
                           args = args, na.rm = na.rm, ylim = ylim, ...))
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidefunction <- ggside_geom("GeomYsidefunction", GeomFunction, "y")



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


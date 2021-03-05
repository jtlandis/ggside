#' @title Side density distributions
#'
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{geom_density} is
#' [geom_xsidedensity] and [geom_ysidedensity].
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_bar
#' @inheritParams ggplot2::geom_ribbon
#' @param stat Use to override the default connection between
#'   `geom_density()` and `stat_density()`.
#' @aliases geom_*sidedensity
#' @return XLayer or YLayer object to be added to a ggplot object
#' @examples
#'
#' ggplot(mpg, aes(displ, hwy, colour = class)) +
#'  geom_point(size = 2) +
#'  geom_xsidedensity(aes(y = after_stat(density)), position = "stack") +
#'  geom_ysidedensity(aes(x = after_stat(scaled))) +
#'  theme(axis.text.x = element_text(angle = 90, vjust = .5))
#'
#' @export
geom_xsidedensity <- function(mapping = NULL, data = NULL,
         stat = "density", position = "identity",
         ...,
         na.rm = FALSE,
         orientation = "x",
         show.legend = NA,
         inherit.aes = TRUE,
         outline.type = "upper") {
  outline.type <- match.arg(outline.type, c("both", "upper", "lower", "full"))
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsidedensity,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      outline.type = outline.type,
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
GeomXsidedensity <- ggplot2::ggproto("GeomXsidedensity",
                                     ggplot2::GeomDensity,
                                     default_aes = aes(fill = NA, xfill = NA, weight = 1,
                                                       colour = "black", xcolour = NA, alpha = NA,
                                                       size = 0.5, linetype = 1),
                                     setup_data = function(data, params) {
                                       data <- parse_side_aes(data, params)
                                       ggplot2::GeomDensity$setup_data(data, params)
                                     },
                                     draw_group = function(data, panel_params, coord, na.rm = FALSE, flipped_aes = FALSE, outline.type = "both") {
                                       #browser()
                                       data <- use_xside_aes(data)
                                       ggplot2::GeomDensity$draw_group(data = data, panel_params = panel_params, coord = coord, na.rm = na.rm,
                                                                       flipped_aes = flipped_aes, outline.type = outline.type)},
                                     draw_key = function(data, params, size) {
                                       data <- use_xside_aes(data)
                                       ggplot2::GeomDensity$draw_key(data, params, size)
                                       })

#' @rdname geom_xsidedensity
#' @export
geom_ysidedensity <- function(mapping = NULL, data = NULL,
                              stat = "density", position = "identity",
                              ...,
                              na.rm = FALSE,
                              orientation = "y",
                              show.legend = NA,
                              inherit.aes = TRUE,
                              outline.type = "upper") {
  outline.type <- match.arg(outline.type, c("both", "upper", "lower", "full"))
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsidedensity,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      outline.type = outline.type,
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
GeomYsidedensity <- ggplot2::ggproto("GeomYsidedensity",
                                     ggplot2::GeomDensity,
                                     default_aes = aes(fill = NA, yfill = NA, weight = 1,
                                                       colour = "black", ycolour = NA, alpha = NA,
                                                       size = 0.5, linetype = 1),
                                     setup_data = function(data, params) {
                                       data <- parse_side_aes(data, params)
                                       ggplot2::GeomDensity$setup_data(data, params)
                                     },
                                     draw_group = function(data, panel_params, coord, na.rm = FALSE, flipped_aes = FALSE, outline.type = "both") {
                                       data <- use_yside_aes(data)
                                       ggplot2::GeomDensity$draw_group(data = data, panel_params = panel_params, coord = coord, na.rm = na.rm,
                                                                       flipped_aes = flipped_aes, outline.type = outline.type)
                                     },
                                     draw_key = function(data, params, size) {
                                       data <- use_yside_aes(data)
                                       ggplot2::GeomDensity$draw_key(data, params, size)
                                     })

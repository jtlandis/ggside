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
    )
  )
  structure(list(layer = l), class = "ggside")
}

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
                                       ggplot2::GeomDensity$draw_group(data, panel_params, coord,na.rm = FALSE,
                                                                       flipped_aes = FALSE, outline.type = "both")},
                                     draw_key = function(data, params, size) {
                                       data <- use_xside_aes(data)
                                       ggplot2::GeomDensity$draw_key(data, params, size)
                                       })


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
    )
  )
  structure(list(layer = l), class = "ggside")
}

GeomYsidedensity <- ggplot2::ggproto("GeomYsidedensity",
                                     ggplot2::GeomDensity,
                                     default_aes = aes(fill = NA, yfill = NA, weight = 1,
                                                       colour = "black", xcolour = NA, alpha = NA,
                                                       size = 0.5, linetype = 1),
                                     setup_data = function(data, params) {
                                       data <- parse_side_aes(data, params)
                                       ggplot2::GeomDensity$setup_data(data, params)
                                     },
                                     draw_group = function(data, panel_params, coord, na.rm = FALSE, flipped_aes = FALSE, outline.type = "both") {
                                       #browser()
                                       data <- use_yside_aes(data)
                                       ggplot2::GeomDensity$draw_group(data, panel_params, coord, na.rm = FALSE,
                                                                       flipped_aes = FALSE, outline.type = "both")
                                     },
                                     draw_key = function(data, params, size) {
                                       data <- use_yside_aes(data)
                                       ggplot2::GeomDensity$draw_key(data, params, size)
                                     })

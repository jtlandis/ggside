
#' @title Side boxplots
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{geom_boxplot}
#' is [geom_xsideboxplot] and [geom_ysideboxplot].
#'
#' @inheritParams ggplot2::geom_boxplot
#'
#' @seealso [geom_*sideviolin]
#' @aliases geom_*sideboxplot
#'
#' @export
geom_xsideboxplot <- function(mapping = NULL, data = NULL,
                              stat = "boxplot", position = "dodge2",
                              ...,
                              outlier.colour = NULL,
                              outlier.color = NULL,
                              outlier.fill = NULL,
                              outlier.shape = 19,
                              outlier.size = 1.5,
                              outlier.stroke = 0.5,
                              outlier.alpha = NULL,
                              notch = FALSE,
                              notchwidth = 0.5,
                              varwidth = FALSE,
                              na.rm = FALSE,
                              orientation = "x",
                              show.legend = NA,
                              inherit.aes = TRUE) {

  # varwidth = TRUE is not compatible with preserve = "total"
  if (is.character(position)) {
    if (varwidth == TRUE) position <- position_dodge2(preserve = "single")
  } else {
    if (identical(position$preserve, "total") & varwidth == TRUE) {
      warn("Can't preserve total widths when varwidth = TRUE.")
      position$preserve <- "single"
    }
  }

  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsideboxplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.fill = outlier.fill,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      outlier.alpha = outlier.alpha,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
      na.rm = na.rm,
      orientation = orientation,
      ...
    ),
    layer_class = XLayer
  )
  structure(l, class = c("ggside_layer",class(l)))
}


GeomXsideboxplot <- ggplot2::ggproto("GeomXsideboxplot",
                                     ggplot2::GeomBoxplot,
                                     default_aes = aes(weight = 1, colour = "grey20", xcolour = NA,
                                                       fill = "white", xfill = NA, size = 0.5,
                                                       alpha = NA, shape = 19, linetype = "solid"),
                                     setup_data = function(data, params){
                                       data <- parse_side_aes(data, params)
                                       ggplot2::GeomBoxplot$setup_data(data, params)
                                     },
                                     draw_panel = function(self, data, panel_params, coord, ...){
                                       data <- use_xside_aes(data)
                                       ggplot2::GeomBoxplot$draw_panel(data = data, panel_params = panel_params, coord = coord, ...)
                                     },
                                     draw_key = function(data, params, size){
                                       data <- use_xside_aes(data)
                                       ggplot2::GeomBoxplot$draw_key(data, params, size)
                                     })


#' @rdname geom_xsideboxplot
#' @export
geom_ysideboxplot <- function(mapping = NULL, data = NULL,
                              stat = "boxplot", position = "dodge2",
                              ...,
                              outlier.colour = NULL,
                              outlier.color = NULL,
                              outlier.fill = NULL,
                              outlier.shape = 19,
                              outlier.size = 1.5,
                              outlier.stroke = 0.5,
                              outlier.alpha = NULL,
                              notch = FALSE,
                              notchwidth = 0.5,
                              varwidth = FALSE,
                              na.rm = FALSE,
                              orientation = "y",
                              show.legend = NA,
                              inherit.aes = TRUE) {

  # varwidth = TRUE is not compatible with preserve = "total"
  if (is.character(position)) {
    if (varwidth == TRUE) position <- position_dodge2(preserve = "single")
  } else {
    if (identical(position$preserve, "total") & varwidth == TRUE) {
      warn("Can't preserve total widths when varwidth = TRUE.")
      position$preserve <- "single"
    }
  }

  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsideboxplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.fill = outlier.fill,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      outlier.alpha = outlier.alpha,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
      na.rm = na.rm,
      orientation = orientation,
      ...
    ),
    layer_class = YLayer
  )
  structure(l, class = c("ggside_layer",class(l)))
}

GeomYsideboxplot <- ggplot2::ggproto("GeomYsideboxplot",
                                     ggplot2::GeomBoxplot,
                                     default_aes = aes(weight = 1, colour = "grey20", ycolour = NA,
                                                       fill = "white", yfill = NA, size = 0.5,
                                                       alpha = NA, shape = 19, linetype = "solid"),
                                     setup_data = function(data, params){
                                       data <- parse_side_aes(data, params)
                                       ggplot2::GeomBoxplot$setup_data(data, params)
                                     },
                                     draw_panel = function(self, data, panel_params, coord, ...){
                                       data <- use_yside_aes(data)
                                       ggplot2::GeomBoxplot$draw_panel(data = data, panel_params = panel_params, coord = coord, ...)
                                     },
                                     draw_key = function(data, params, size){
                                       data <- use_yside_aes(data)
                                       ggplot2::GeomBoxplot$draw_key(data, params, size)
                                     })






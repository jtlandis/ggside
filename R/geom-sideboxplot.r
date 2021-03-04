
#' @title Side boxplots
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{geom_boxplot}
#' is [geom_xsideboxplot] and [geom_ysideboxplot].
#'
#' @inheritParams ggplot2::geom_boxplot
#'
#' @seealso [geom_*sideviolin]
#' @aliases geom_*sideboxplot
#' @return XLayer or YLayer object to be added to a ggplot object
#' @examples
#'
#' df <- expand.grid(UpperCase = LETTERS, LowerCase = letters)
#' df$Combo_Index <- as.integer(df$UpperCase)*as.integer(df$LowerCase)
#'
#' p1 <- ggplot(df, aes(UpperCase, LowerCase)) +
#' geom_tile(aes(fill = Combo_Index))
#'
#' #sideboxplots
#' #Note - Mixing discrete and continuous axis scales
#' #using xsideboxplots when the y aesthetic was previously
#' #mapped with a continuous varialbe will prevent
#' #any labels from being plotted. This is a feature that
#' #will hopefully be added to ggside in the future.
#'
#' p1 + geom_xsideboxplot(aes(y = Combo_Index)) +
#'    geom_ysideboxplot(aes(x = Combo_Index))
#'
#' #sideboxplots with swapped orientation
#' #Note - Discrete before Continuous
#' #If you are to mix Discrete and Continuous variables on
#' #one axis, ggplot2 prefers the discrete variable to be mapped
#' #BEFORE the continuous.
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'     geom_xsideboxplot(aes(y = Species), orientation = "y") +
#'     geom_ysideboxplot(aes(x = Species), orientation = "x") +
#'     geom_point()
#'
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

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
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

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
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






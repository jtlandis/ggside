
#' @title Side boxplots
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{geom_boxplot}
#' is [geom_xsideboxplot] and [geom_ysideboxplot].
#'
#' @inheritParams ggplot2::layer
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
#'
#' p1 + geom_xsideboxplot(aes(y = Combo_Index)) +
#'    geom_ysideboxplot(aes(x = Combo_Index)) +
#'    #when mixing continuous/discrete scales
#'    #use the following helper functions
#'    scale_xsidey_continuous() +
#'    scale_ysidex_continuous()
#'
#' #sideboxplots with swapped orientation
#' #Note: They order of the layers are affects the default
#' # scale type. If you were to omit the last two scales, the
#' # data labels may be affected
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'     geom_xsideboxplot(aes(y = Species), orientation = "y") +
#'     geom_point() +
#'     scale_y_continuous() + scale_xsidey_discrete()
#'
#' #If using the scale_(xsidey|ysidex)_* functions are a bit cumbersome,
#' # Take extra care to recast your data types.
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species))+
#'   geom_point() +
#'   geom_xsideboxplot(aes(y = as.numeric(Species)), orientation = "y") +
#'   geom_ysideboxplot(aes(x = as.numeric(Species)), orientation = "x")
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

  new_ggside_layer(
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
    )
  )
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsideboxplot <- ggside_geom("GeomXsideboxplot", GeomBoxplot, "x")


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

  new_ggside_layer(
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
    )
  )
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsideboxplot <- ggside_geom("GeomYsideboxplot", GeomBoxplot, "y")


### INCLUDE BEGIN
#' @include compat-plyr.R
#' @include ggside.R
#' @include side-layer.R
#' @include constructor-.R
NULL
### INCLUDE END

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
geom_xsideboxplot <- ggside_layer_function(fun = geom_boxplot, side = "x")

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsideboxplot <- ggside_geom("GeomXsideboxplot", GeomBoxplot, "x")


#' @rdname geom_xsideboxplot
#' @export
geom_ysideboxplot <- ggside_layer_function(fun = geom_boxplot, side = "y")

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsideboxplot <- ggside_geom("GeomYsideboxplot", GeomBoxplot, "y")


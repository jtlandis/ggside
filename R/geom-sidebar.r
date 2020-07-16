
#' geom_*sidebar: Plot a sidebar along an axis
#'
#' @param mapping Set of aesthetic mappings reated by [`aes()`] or
#' [`aes_()`]. If specified and `inherit.aes = TRUE`, it will be
#' combined with the default mapping at the top level of the plot.
#' `mapping` must be supplied in this layer if ther is no plot mapping.
#' @param data The data to be displayed in this layer. If `NULL`, the default,
#' then the data is inherited from the plot data as specified i nthe call to [`ggplot()`].
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... other arguments passed on to [`layer()`]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `color = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#'
#'
#' @section Aesthetics:
#'
#' Required aesthetics are in bold.
#'
#' \itemize{
#' \item \strong{x}
#' \item \strong{y}
#' \item \emph{xfill} Fill color of the xsidebar
#' \item \emph{yfill} Fill color of the ysidebar
#' \item \emph{width} specifies the width of each bar
#' \item \emph{height} specifies the height of each bar
#' \item \emph{alpha} Transparency level of `xfill` or `yfill` depending on which [`geom_*sidbar`]
#' \item \emph{size} size of the border line. --uneeded??
#' \item \emph{location} Specifies where the sidebar should be placed.
#' geom_xsidebar may specify either "bottom" or "top" and
#' geom_ysidebar may specify either "left" or "right.
#' "bottom" and "left" are defaults of geom_xsidebar and
#' geom_ysidebar respecitively.
#'
#' }
#'
#'
#'
#'
#' @import ggplot2
#' @export
geom_xsidebar <- function(mapping = NULL, data = NULL,
                          stat = "count", position = "stack",
                          ...,
                          width = NULL,
                          na.rm = FALSE,
                          orientation = "x",
                          show.legend = NA,
                          inherit.aes = TRUE) {
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsidebar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
  structure(list(layer = l), class="ggside")
}


GeomXsidebar <- ggplot2::ggproto("GeomXsidebar",
                        ggplot2::GeomBar,
                        default_aes = aes(colour = NA, xcolour = NA,
                                          fill = "grey35", xfill = NA,
                                          size = 0.5, linetype = 1, alpha = NA),
                        setup_data = function(data, params){
                          data <- parse_side_aes(data, params),
                          ggplot2::GeomBar$setup_data(data, params)
                        },
                        draw_panel = function(self, data, panel_params, coord, width = NULL, flipped_aes = FALSE){
                          data <- use_xside_aes(data)
                          ggplot2::GeomBar$setup_data(data = data, panel_params = panel_params,
                                                      coord = coord, widht = width, flipped_aes = flipped_aes)
                        },
                        draw_key = function(data, params, size){
                          data <- use_xside_aes(data)
                          ggplot2::GeomBar$draw_key(data, params, size)
                        })


#' @rdname geom_xsidebar
#' @aliases geom_ysidebar
#' @export
geom_ysidebar <- function(mapping = NULL, data = NULL,
                          stat = "count", position = "stack",
                          ...,
                          width = NULL,
                          na.rm = FALSE,
                          orientation = "y",
                          show.legend = NA,
                          inherit.aes = TRUE) {
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsidebar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
  structure(list(layer = l), class="ggside")
}


GeomYsidebar <- ggplot2::ggproto("GeomYsidebar",
                                 ggplot2::GeomBar,
                                 default_aes = aes(colour = NA, ycolour = NA,
                                                   fill = "grey35", yfill = NA,
                                                   size = 0.5, linetype = 1, alpha = NA),
                                 setup_data = function(data, params){
                                   data <- parse_side_aes(data, params),
                                   ggplot2::GeomBar$setup_data(data, params)
                                 },
                                 draw_panel = function(self, data, panel_params, coord, width = NULL, flipped_aes = FALSE){
                                   data <- use_yside_aes(data)
                                   ggplot2::GeomBar$setup_data(data = data, panel_params = panel_params,
                                                               coord = coord, widht = width, flipped_aes = flipped_aes)
                                 },
                                 draw_key = function(data, params, size){
                                   data <- use_yside_aes(data)
                                   ggplot2::GeomBar$draw_key(data, params, size)
                                 })



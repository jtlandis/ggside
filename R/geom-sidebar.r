
#' @title Side bar Charts
#'
#' @description
#'
#' The [xside] and [yside] variants of \link[ggplot2]{geom_bar} is
#' [geom_xsidebar] and [geom_ysidebar]. These variants both inherit
#' from \link[ggplot2]{geom_bar} and only differ on where they plot
#' data relative to main panels.
#'
#' The [xside] and [yside] variants of \link[ggplot2]{geom_col} is
#' [geom_xsidecol] and [geom_ysidecol]. These variants both inherit
#' from \link[ggplot2]{geom_col} and only differ on where they plot
#' data relative to main panels.
#'
#' @inheritParams ggplot2::geom_bar
#'
#' @section Aesthetics:
#'
#' Required aesthetics are in bold.
#'
#' \itemize{
#' \item \strong{`x`}
#' \item \strong{`y`}
#' \item \emph{`fill` or `xfill`} Fill color of the xsidebar
#' \item \emph{`fill` or `yfill`} Fill color of the ysidebar
#' \item \emph{`width`} specifies the width of each bar
#' \item \emph{`height`} specifies the height of each bar
#' \item \emph{`alpha`} Transparency level of `xfill` or `yfill`
#' \item \emph{`size`} size of the border line.
#' }
#'
#' @seealso [geom_xsidehistogram], [geom_ysidehistogram]
#' @return XLayer or YLayer object to be added to a ggplot object
#' @aliases geom_*sidebar
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
    ),
    layer_class = XLayer
  )
  structure(l, class=c("ggside_layer", class(l)))
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidebar <- ggplot2::ggproto("GeomXsidebar",
                        ggplot2::GeomBar,
                        default_aes = aes(colour = NA, xcolour = NA,
                                          fill = "grey35", xfill = NA,
                                          size = 0.5, linetype = 1, alpha = NA),
                        setup_data = function(data, params){
                          data <- parse_side_aes(data, params)
                          ggplot2::GeomBar$setup_data(data, params)
                        },
                        draw_panel = function(self, data, panel_params, coord, width = NULL, flipped_aes = FALSE){
                          data <- use_xside_aes(data)
                          ggplot2::GeomBar$draw_panel(data = data, panel_params = panel_params,
                                                      coord = coord, width = width, flipped_aes = flipped_aes)
                        },
                        draw_key = function(data, params, size){
                          data <- use_xside_aes(data)
                          ggplot2::GeomBar$draw_key(data, params, size)
                        })


#' @rdname geom_xsidebar
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
    ),
    layer_class = YLayer
  )
  structure(l, class = c("ggside_layer",class(l)))
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidebar <- ggplot2::ggproto("GeomYsidebar",
                                 ggplot2::GeomBar,
                                 default_aes = aes(colour = NA, ycolour = NA,
                                                   fill = "grey35", yfill = NA,
                                                   size = 0.5, linetype = 1, alpha = NA),
                                 setup_data = function(data, params){
                                   data <- parse_side_aes(data, params)
                                   ggplot2::GeomBar$setup_data(data, params)
                                 },
                                 draw_panel = function(self, data, panel_params, coord, width = NULL, flipped_aes = FALSE){
                                   data <- use_yside_aes(data)
                                   ggplot2::GeomBar$draw_panel(data = data, panel_params = panel_params,
                                                               coord = coord, width = width, flipped_aes = flipped_aes)
                                 },
                                 draw_key = function(data, params, size){
                                   data <- use_yside_aes(data)
                                   ggplot2::GeomBar$draw_key(data, params, size)
                                 })



#' @rdname geom_xsidebar
#' @export
geom_xsidecol <- function(mapping = NULL, data = NULL,
                          position = "stack",
                          ...,
                          width = NULL,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  mapping <- force_panel_type_mapping(mapping, "x")
  l <- layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomXsidecol,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      na.rm = na.rm,
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
GeomXsidecol <- ggplot2::ggproto("GeomXsidecol",
                                 ggplot2::GeomCol,
                                 default_aes = aes(
                                   !!!ggplot2::GeomCol$default_aes,
                                   xcolour = NA, xfill = NA, PANEL_TYPE = "x"
                                 ),
                                 setup_data = function(data, params){
                                   data <- parse_side_aes(data, params)
                                   ggplot2::GeomCol$setup_data(data, params)
                                 },
                                 draw_panel = function(self, data, panel_params, coord, width = NULL, flipped_aes = FALSE){
                                   data <- use_xside_aes(data)
                                   ggplot2::GeomCol$draw_panel(data = data, panel_params = panel_params,
                                                               coord = coord, width = width, flipped_aes = flipped_aes)
                                 },
                                 draw_key = function(data, params, size){
                                   data <- use_xside_aes(data)
                                   ggplot2::GeomCol$draw_key(data, params, size)
                                 })

#' @rdname geom_xsidebar
#' @export
geom_ysidecol <- function(mapping = NULL, data = NULL,
                          position = "stack",
                          ...,
                          width = NULL,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          orientation = "y") {

  mapping <- force_panel_type_mapping(mapping, "y")
  l <- layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomYsidecol,
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
GeomYsidecol <- ggplot2::ggproto("GeomYsidecol",
                                 ggplot2::GeomCol,
                                 default_aes = aes(
                                   !!!ggplot2::GeomCol$default_aes,
                                   ycolour = NA, yfill = NA, PANEL_TYPE = "y"
                                 ),
                                 setup_data = function(data, params){
                                   data <- parse_side_aes(data, params)
                                   ggplot2::GeomCol$setup_data(data, params)
                                 },
                                 draw_panel = function(self, data, panel_params, coord, width = NULL, flipped_aes = FALSE){
                                   data <- use_yside_aes(data)
                                   ggplot2::GeomCol$draw_panel(data = data, panel_params = panel_params,
                                                               coord = coord, width = width, flipped_aes = flipped_aes)
                                 },
                                 draw_key = function(data, params, size){
                                   data <- use_yside_aes(data)
                                   ggplot2::GeomCol$draw_key(data, params, size)
                                 })

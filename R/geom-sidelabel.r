#' @title Side label
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{geom_label}.
#' @inheritParams ggplot2::geom_label
#' @aliases geom_*sidelabel
#' @return XLayer or YLayer object to be added to a ggplot object
#' @export
geom_xsidelabel <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            parse = FALSE,
                            nudge_x = 0,
                            nudge_y = 0,
                            check_overlap = FALSE,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsidelabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    ),
    layer_class = ggside:::XLayer
  )
  structure(l, class = c("ggside_layer",class(l)))
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidelabel <- ggplot2::ggproto("GeomXsidelabel",
                                   ggplot2::GeomLabel,
                                   default_aes = aes(colour = "black", fill = "white",
                                                     xcolour = NA, size = 3.88,
                                                     angle = 0, hjust = 0.5, vjust = 0.5, alpha = NA,
                                                     family = "", fontface = 1, lineheight = 1.2),
                                   setup_data = function(data, params){
                                     parse_side_aes(data, params)
                                   },
                                   draw_panel = function(self, data, panel_params, coord, parse = FALSE,
                                                         na.rm = FALSE) {
                                     data <- use_xside_aes(data)
                                     ggplot2::GeomLabel$draw_panel(data = data, panel_params = panel_params,
                                                                   coord = coord, parse = parse, na.rm = na.rm)
                                   },
                                   draw_key = function(data, params, size){
                                     data <- use_xside_aes(data)
                                     ggplot2::GeomLabel$draw_key(data, params, size)
                                   })


#' @rdname geom_xsidelabel
#' @export
geom_ysidelabel <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            parse = FALSE,
                            nudge_x = 0,
                            nudge_y = 0,
                            check_overlap = FALSE,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsidelabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      na.rm = na.rm,
      ...
    ),
    layer_class = ggside:::YLayer
  )
  structure(l, class = c("ggside_layer",class(l)))
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidelabel <- ggplot2::ggproto("GeomYsidelabel",
                                   ggplot2::GeomLabel,
                                   default_aes = aes(colour = "black", fill = "white",
                                                     ycolour = NA, size = 3.88,
                                                     angle = 0, hjust = 0.5, vjust = 0.5, alpha = NA,
                                                     family = "", fontface = 1, lineheight = 1.2),
                                   setup_data = function(data, params){
                                     parse_side_aes(data, params)
                                   },
                                   draw_panel = function(self, data, panel_params, coord, parse = FALSE,
                                                         na.rm = FALSE){
                                     data <- use_yside_aes(data)
                                     ggplot2::GeomLabel$draw_panel(data = data, panel_params = panel_params,
                                                                   coord = coord, parse = parse, na.rm = na.rm)
                                   },
                                   draw_key = function(data, params, size){
                                     data <- use_yside_aes(data)
                                     ggplot2::GeomLabel$draw_key(data, params, size)
                                   })

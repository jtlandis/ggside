#' @title Side text
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{geom_text}.
#' @inheritParams ggplot2::geom_text
#' @aliases geom_*sidetext
#' @export
geom_xsidetext <- function(mapping = NULL, data = NULL,
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
    geom = GeomText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    ),
    layer_class = XLayer
  )
  structure(l, class = c("ggside_layer",class(l)))
}

GeomXsidetext <- ggplot2::ggproto("GeomXsidetext",
                                  ggplot2::GeomText,
                                  default_aes = aes(colour = "black", xcolour = NA, size = 3.88,
                                                    angle = 0, hjust = 0.5, vjust = 0.5, alpha = NA,
                                                    family = "", fontface = 1, lineheight = 1.2),
                                  setup_data = function(data, params){
                                    parse_side_aes(data, params)
                                  },
                                  draw_panel = function(data, panel_params, coord, parse = FALSE,
                                                        na.rm = FALSE, check_overlap = FALSE){
                                    data <- use_xside_aes(data)
                                    ggplot2::GeomText$draw_panel(data = data, panel_params = panel_params,
                                                                 coord = coord, parse = parse, na.rm = na.rm,
                                                                 check_overlap = check_overlap)
                                  },
                                  draw_key = function(data, params, size){
                                    data <- use_xside_aes(data)
                                    ggplot2::GeomText$draw_key(data, params, size)
                                  })


#' @rdname geom_xsidetext
#' @export
geom_ysidetext <- function(mapping = NULL, data = NULL,
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
    geom = GeomYsidetext,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    ),
    layer_class = YLayer
  )
  structure(l, class = c("ggside_layer",class(l)))
}


GeomYsidetext <- ggplot2::ggproto("GeomYsidetext",
                                  ggplot2::GeomText,
                                  default_aes = aes(colour = "black", ycolour = NA, size = 3.88,
                                                    angle = 0, hjust = 0.5, vjust = 0.5, alpha = NA,
                                                    family = "", fontface = 1, lineheight = 1.2),
                                  setup_data = function(data, params){
                                    parse_side_aes(data, params)
                                  },
                                  draw_panel = function(data, panel_params, coord, parse = FALSE,
                                                        na.rm = FALSE, check_overlap = FALSE){
                                    data <- use_yside_aes(data)
                                    ggplot2::GeomText$draw_panel(data = data, panel_params = panel_params,
                                                                 coord = coord, parse = parse, na.rm = na.rm,
                                                                 check_overlap = check_overlap)
                                  },
                                  draw_key = function(data, params, size){
                                    data <- use_yside_aes(data)
                                    ggplot2::GeomText$draw_key(data, params, size)
                                  })

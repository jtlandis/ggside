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
                            label.padding = unit(0.25, "lines"),
                            label.r = unit(0.15, "lines"),
                            label.size = 0.25,
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

  new_ggside_layer(
    "x",
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsidelabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      na.rm = na.rm,
      ...)
  )
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidelabel <- ggside_geom("GeomXsidelabel", GeomLabel, "x")


#' @rdname geom_xsidelabel
#' @export
geom_ysidelabel <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            parse = FALSE,
                            nudge_x = 0,
                            nudge_y = 0,
                            label.padding = unit(0.25, "lines"),
                            label.r = unit(0.15, "lines"),
                            label.size = 0.25,
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

  new_ggside_layer(
    "y",
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsidelabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      na.rm = na.rm,
      ...)
  )
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidelabel <- ggside_geom("GeomYsidelabel", GeomLabel, "y")

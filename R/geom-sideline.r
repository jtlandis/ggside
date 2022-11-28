#' @title Side line plot
#' @description
#' The [xside] and [yside] of \link[ggplot2]{geom_line}.
#' The [xside] and [yside] variants of \link[ggplot2]{geom_path}
#' @inheritParams ggplot2::geom_line
#'
#' @aliases geom_*sideline
#' @return XLayer or YLayer object to be added to a ggplot object
#' @examples
#' #sideline
#' ggplot(economics, aes(date, pop)) +
#'   geom_xsideline(aes(y = unemploy)) +
#'   geom_col()
#' @export
geom_xsideline <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE, orientation = NA,
                           show.legend = NA, inherit.aes = TRUE, ...) {

  new_ggside_layer(
    "x",
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsideline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
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
GeomXsideline <- ggside_geom("GeomXsideline", GeomLine, "x")


#' @rdname geom_xsideline
#' @export
geom_ysideline <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE, orientation = NA,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  new_ggside_layer(
    "y",
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsideline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
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
GeomYsideline <- ggside_geom("GeomYsideline", GeomLine, "y")


#' @title  Side Points
#'
#' @description
#' The ggside variants of \link[ggplot2]{geom_point} is [geom_xsidepoint()] and
#' [geom_ysidepoint()]. Both variants inherit from \link[ggplot2]{geom_point},
#' thus the only difference is where the data is plotted. The `xside` variant will
#' plot data along the x-axis, while the `yside` variant will plot data along the
#' y-axis.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#'
#' @aliases geom_*sidepoint
#' @return XLayer or YLayer object to be added to a ggplot object
#' @examples
#' ggplot(diamonds, aes(depth, table, alpha = .2)) +
#'   geom_point() +
#'   geom_ysidepoint(aes(x = price)) +
#'   geom_xsidepoint(aes(y = price)) +
#'   theme(
#'         ggside.panel.scale = .3
#'     )
#' @export
geom_xsidepoint <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  new_ggside_layer(
    "x",
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsidepoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidepoint <- ggside_geom("GeomXsidepoint", GeomPoint, "x")

#' @rdname geom_xsidepoint
#' @export
geom_ysidepoint <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  new_ggside_layer(
    "y",
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsidepoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidepoint <- ggside_geom("GeomYsidepoint", GeomPoint, "y")


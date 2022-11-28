#' @rdname geom_xsideline
#' @export
geom_xsidepath <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           lineend = "butt",
                           linejoin = "round",
                           linemitre = 10,
                           arrow = NULL,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  new_ggside_layer(
    "x",
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsidepath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidepath <- ggside_geom("GeomXsidepath", GeomPath, "x")


#' @rdname geom_xsideline
#' @export
geom_ysidepath <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           lineend = "butt",
                           linejoin = "round",
                           linemitre = 10,
                           arrow = NULL,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  new_ggside_layer(
    "y",
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsidepath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidepath <- ggside_geom("GeomYsidepath", GeomPath, "y")

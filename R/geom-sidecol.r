#' @rdname geom_xsidebar
#' @export
geom_xsidecol <- function(mapping = NULL, data = NULL,
                          position = "stack",
                          ...,
                          width = NULL,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  new_ggside_layer(
    "x",
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
    )
  )
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomXsidecol <- ggside_geom("GeomXsidecol", GeomCol, "x")

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

  new_ggside_layer(
    "y",
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
    )
  )
}

#' @rdname ggside-ggproto-geoms
#' @usage NULL
#' @format NULL
#' @export
GeomYsidecol <- ggside_geom("GeomYsidecol", GeomCol, "y")

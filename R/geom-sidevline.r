#' @aliases geom_*vline
#' @rdname geom_xsideabline
#' @export
geom_xsidevline <- function(mapping = NULL, data = NULL,
                       ...,
                       xintercept,
                       na.rm = FALSE,
                       show.legend = NA) {

  # Act like an annotation
  if (!missing(xintercept)) {
    # Warn if supplied mapping and/or data is going to be overwritten
    if (!is.null(mapping)) {
      cli::cli_warn("{.fn geom_vline}: Ignoring {.arg mapping} because {.arg xintercept} was provided.")
    }
    if (!is.null(data)) {
      cli::cli_warn("{.fn geom_vline}: Ignoring {.arg data} because {.arg xintercept} was provided.")
    }

    data <- data_frame(xintercept = xintercept)
    mapping <- aes(xintercept = xintercept)
    show.legend <- FALSE
  }

  ggside_layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomXsidevline,
    position = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list2(
      na.rm = na.rm,
      ...
    ))
}

#' @rdname geom_xsideabline
#' @export
geom_ysidevline <- function(mapping = NULL, data = NULL,
                       ...,
                       xintercept,
                       na.rm = FALSE,
                       show.legend = NA) {

  # Act like an annotation
  if (!missing(xintercept)) {
    # Warn if supplied mapping and/or data is going to be overwritten
    if (!is.null(mapping)) {
      cli::cli_warn("{.fn geom_vline}: Ignoring {.arg mapping} because {.arg xintercept} was provided.")
    }
    if (!is.null(data)) {
      cli::cli_warn("{.fn geom_vline}: Ignoring {.arg data} because {.arg xintercept} was provided.")
    }

    data <- data_frame(xintercept = xintercept)
    mapping <- aes(xintercept = xintercept)
    show.legend <- FALSE
  }

  ggside_layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomYsidevline,
    position = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list2(
      na.rm = na.rm,
      ...
    ))
}

#' @rdname ggside-ggproto-geoms
#' @format NULL
#' @usage NULL
#' @export
GeomXsidevline <- ggside_geom("GeomXsidevline", GeomVline, "x")

#' @rdname ggside-ggproto-geoms
#' @format NULL
#' @usage NULL
#' @export
GeomYsidevline <- ggside_geom("GeomYsidevline", GeomVline, "y")

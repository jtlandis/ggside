#' @aliases geom_*hline
#' @rdname geom_xsideabline
#' @export
geom_xsidehline <- function(mapping = NULL, data = NULL,
                       ...,
                       yintercept,
                       na.rm = FALSE,
                       show.legend = NA) {

  # Act like an annotation
  if (!missing(yintercept)) {
    # Warn if supplied mapping and/or data is going to be overwritten
    if (!is.null(mapping)) {
      cli::cli_warn("{.fn geom_hline}: Ignoring {.arg mapping} because {.arg yintercept} was provided.")
    }
    if (!is.null(data)) {
      cli::cli_warn("{.fn geom_hline}: Ignoring {.arg data} because {.arg yintercept} was provided.")
    }

    data <- data_frame(yintercept = yintercept)
    mapping <- aes(yintercept = yintercept)
    show.legend <- FALSE
  }

  ggside_layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomXsidehline,
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
geom_ysidehline <- function(mapping = NULL, data = NULL,
                            ...,
                            yintercept,
                            na.rm = FALSE,
                            show.legend = NA) {

  # Act like an annotation
  if (!missing(yintercept)) {
    # Warn if supplied mapping and/or data is going to be overwritten
    if (!is.null(mapping)) {
      cli::cli_warn("{.fn geom_hline}: Ignoring {.arg mapping} because {.arg yintercept} was provided.")
    }
    if (!is.null(data)) {
      cli::cli_warn("{.fn geom_hline}: Ignoring {.arg data} because {.arg yintercept} was provided.")
    }

    data <- data_frame(yintercept = yintercept)
    mapping <- aes(yintercept = yintercept)
    show.legend <- FALSE
  }

  ggside_layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomYsidehline,
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
GeomXsidehline <- ggside_geom("GeomXsidehline", GeomHline, "x")

#' @rdname ggside-ggproto-geoms
#' @format NULL
#' @usage NULL
#' @export
GeomYsidehline <- ggside_geom("GeomYsidehline", GeomHline, "y")

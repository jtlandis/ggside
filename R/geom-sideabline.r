
#' @title Side Reference lines
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{geom_abline},
#' \link[ggplot2]{geom_hline} and \link[ggplot2]{geom_vline} are
#' [geom_*abline], [geom_*hline], and [geom_*vline].
#'
#' @aliases geom_*abline
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param mapping Set of aesthetic mappings created by [aes()].
#' @param xintercept,yintercept,slope,intercept Parameters that control the
#'   position of the line specifically for the [xside] or [yside] variants.
#'   If these are set, `data`, `mapping` and `show.legend` are overridden.
#' @export
geom_xsideabline <- function(mapping = NULL, data = NULL,
                        ...,
                        slope,
                        intercept,
                        na.rm = FALSE,
                        show.legend = NA) {
  # If nothing set, default to y = x
  if (is.null(mapping) && missing(slope) && missing(intercept)) {
    slope <- 1
    intercept <- 0
  }

  # Act like an annotation
  if (!missing(slope) || !missing(intercept)) {

    # Warn if supplied mapping and/or data is going to be overwritten
    if (!is.null(mapping)) {
      cli::cli_warn("{.fn geom_abline}: Ignoring {.arg mapping} because {.arg slope} and/or {.arg intercept} were provided.")
    }
    if (!is.null(data)) {
      cli::cli_warn("{.fn geom_abline}: Ignoring {.arg data} because {.arg slope} and/or {.arg intercept} were provided.")
    }

    if (missing(slope)) slope <- 1
    if (missing(intercept)) intercept <- 0
    n_slopes <- max(length(slope), length(intercept))

    data <- data_frame(
      intercept = intercept,
      slope = slope,
      .size = n_slopes
    )
    mapping <- aes(intercept = intercept, slope = slope)
    show.legend <- FALSE
  }

  ggside_layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomXsideabline,
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
geom_ysideabline <- function(mapping = NULL, data = NULL,
                             ...,
                             slope,
                             intercept,
                             na.rm = FALSE,
                             show.legend = NA) {
  # If nothing set, default to y = x
  if (is.null(mapping) && missing(slope) && missing(intercept)) {
    slope <- 1
    intercept <- 0
  }

  # Act like an annotation
  if (!missing(slope) || !missing(intercept)) {

    # Warn if supplied mapping and/or data is going to be overwritten
    if (!is.null(mapping)) {
      cli::cli_warn("{.fn geom_abline}: Ignoring {.arg mapping} because {.arg slope} and/or {.arg intercept} were provided.")
    }
    if (!is.null(data)) {
      cli::cli_warn("{.fn geom_abline}: Ignoring {.arg data} because {.arg slope} and/or {.arg intercept} were provided.")
    }

    if (missing(slope)) slope <- 1
    if (missing(intercept)) intercept <- 0
    n_slopes <- max(length(slope), length(intercept))

    data <- data_frame(
      intercept = intercept,
      slope = slope,
      .size = n_slopes
    )
    mapping <- aes(intercept = intercept, slope = slope)
    show.legend <- FALSE
  }

  ggside_layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomYsideabline,
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
GeomXsideabline <- ggside_geom("GeomXsideabline", GeomAbline, "x")

#' @rdname ggside-ggproto-geoms
#' @format NULL
#' @usage NULL
#' @export
GeomYsideabline <- ggside_geom("GeomYsideabline", GeomAbline, "y")

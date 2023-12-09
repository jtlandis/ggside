
#' @title Side Reference lines
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{geom_abline},
#' \link[ggplot2]{geom_hline} and \link[ggplot2]{geom_vline} are
#' [geom_*abline], [geom_*hline], and [geom_*vline].
#'
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

  l <- layer(
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
    ),
    layer_class = XLayer
  )
  structure(l, class = c("ggside_layer", class(l)))

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

  l <- layer(
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
    ),
    layer_class = YLayer
  )
  structure(l, class = c("ggside_layer", class(l)))

}


#' @rdname ggside-ggproto-geoms
#' @format NULL
#' @usage NULL
#' @export
GeomXsideabline <- ggplot2::ggproto(
    "GeomXsideabline",
    ggplot2::GeomAbline,
    default_aes = new_default_aes(
      aes(xcolour = NA, xfill = NA),
      ggplot2::GeomAbline$default_aes
    ),
    setup_data = function(data, params) {
      data <- parse_side_aes(data, params)
      ggplot2::GeomAbline$setup_data(data, params)
    },
    draw_panel = function(data, panel_params, coord, lineend = "butt") {
      data <- use_xside_aes(data)
      ggplot2::GeomAbline$draw_panel(data = data, panel_params = panel_params,
                                     coord = coord, lineend = lineend)

    },
    draw_key = function(data, params, size) {
      data <- use_xside_aes(data)
      ggplot2::GeomAbline$draw_key(data, params, size)
    }
)

#' @rdname ggside-ggproto-geoms
#' @format NULL
#' @usage NULL
#' @export
GeomYsideabline <- ggplot2::ggproto(
  "GeomYsideabline",
  ggplot2::GeomAbline,
  default_aes = new_default_aes(
    aes(ycolour = NA, yfill = NA),
    ggplot2::GeomAbline$default_aes
  ),
  setup_data = function(data, params) {
    data <- parse_side_aes(data, params)
    ggplot2::GeomAbline$setup_data(data, params)
  },
  draw_panel = function(data, panel_params, coord, lineend = "butt") {
    data <- use_yside_aes(data)
    ggplot2::GeomAbline$draw_panel(data = data, panel_params = panel_params,
                                   coord = coord, lineend = lineend)

  },
  draw_key = function(data, params, size) {
    data <- use_yside_aes(data)
    ggplot2::GeomAbline$draw_key(data, params, size)
  }
)

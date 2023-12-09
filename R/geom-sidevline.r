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

  l <- layer(
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
    ),
    layer_class = XLayer
  )
  structure(l, class = c("ggside_layer", class(l)))
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

  l <- layer(
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
    ),
    layer_class = YLayer
  )
  structure(l, class = c("ggside_layer", class(l)))
}

#' @rdname ggside-ggproto-geoms
#' @format NULL
#' @usage NULL
#' @export
GeomXsidevline <- ggplot2::ggproto(
  "GeomXsidevline",
  ggplot2::GeomVline,
  default_aes = new_default_aes(
    aes(xcolour = NA, xfill = NA),
    ggplot2::GeomVline$default_aes
  ),
  setup_data = function(data, params) {
    data <- parse_side_aes(data, params)
    ggplot2::GeomVline$setup_data(data, params)
  },
  draw_panel = function(data, panel_params, coord, lineend = "butt") {
    data <- use_xside_aes(data)
    ggplot2::GeomVline$draw_panel(data = data, panel_params = panel_params,
                                  coord = coord, lineend = lineend)

  },
  draw_key = function(data, params, size) {
    data <- use_xside_aes(data)
    ggplot2::GeomVline$draw_key(data, params, size)
  }
)

#' @rdname ggside-ggproto-geoms
#' @format NULL
#' @usage NULL
#' @export
GeomYsidevline <- ggplot2::ggproto(
  "GeomYsidevline",
  ggplot2::GeomVline,
  default_aes = new_default_aes(
    aes(ycolour = NA, yfill = NA),
    ggplot2::GeomVline$default_aes
  ),
  setup_data = function(data, params) {
    data <- parse_side_aes(data, params)
    ggplot2::GeomVline$setup_data(data, params)
  },
  draw_panel = function(data, panel_params, coord, lineend = "butt") {
    data <- use_yside_aes(data)
    ggplot2::GeomVline$draw_panel(data = data, panel_params = panel_params,
                                  coord = coord, lineend = lineend)

  },
  draw_key = function(data, params, size) {
    data <- use_yside_aes(data)
    ggplot2::GeomVline$draw_key(data, params, size)
  }
)

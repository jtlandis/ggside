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

  l <- layer(
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
    ),
    layer_class = XLayer
  )
  structure(l, class = c("ggside_layer", class(l)))
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

  l <- layer(
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
    ),
    layer_class = YLayer
  )
  structure(l, class = c("ggside_layer", class(l)))
}

#' @rdname ggside-ggproto-geoms
#' @format NULL
#' @usage NULL
#' @export
GeomXsidehline <- ggplot2::ggproto(
  "GeomXsidehline",
  ggplot2::GeomHline,
  default_aes = new_default_aes(
    aes(xcolour = NA, xfill = NA),
    ggplot2::GeomHline$default_aes
  ),
  setup_data = function(data, params) {
    data <- parse_side_aes(data, params)
    ggplot2::GeomHline$setup_data(data, params)
  },
  draw_panel = function(data, panel_params, coord, lineend = "butt") {
    data <- use_xside_aes(data)
    ggplot2::GeomHline$draw_panel(data = data, panel_params = panel_params,
                                   coord = coord, lineend = lineend)

  },
  draw_key = function(data, params, size) {
    data <- use_xside_aes(data)
    ggplot2::GeomHline$draw_key(data, params, size)
  }
)

#' @rdname ggside-ggproto-geoms
#' @format NULL
#' @usage NULL
#' @export
GeomYsidehline <- ggplot2::ggproto(
  "GeomYsidehline",
  ggplot2::GeomHline,
  default_aes = new_default_aes(
    aes(ycolour = NA, yfill = NA),
    ggplot2::GeomHline$default_aes
  ),
  setup_data = function(data, params) {
    data <- parse_side_aes(data, params)
    ggplot2::GeomHline$setup_data(data, params)
  },
  draw_panel = function(data, panel_params, coord, lineend = "butt") {
    data <- use_yside_aes(data)
    ggplot2::GeomHline$draw_panel(data = data, panel_params = panel_params,
                                   coord = coord, lineend = lineend)

  },
  draw_key = function(data, params, size) {
    data <- use_yside_aes(data)
    ggplot2::GeomHline$draw_key(data, params, size)
  }
)

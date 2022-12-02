
#' @title Side bar Charts
#'
#' @description
#'
#' The [xside] and [yside] variants of \link[ggplot2]{geom_bar} is
#' [geom_xsidebar] and [geom_ysidebar]. These variants both inherit
#' from \link[ggplot2]{geom_bar} and only differ on where they plot
#' data relative to main panels.
#'
#' The [xside] and [yside] variants of \link[ggplot2]{geom_col} is
#' [geom_xsidecol] and [geom_ysidecol]. These variants both inherit
#' from \link[ggplot2]{geom_col} and only differ on where they plot
#' data relative to main panels.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_bar
#'
#' @section Aesthetics:
#'
#' Required aesthetics are in bold.
#'
#' \itemize{
#' \item \strong{`x`}
#' \item \strong{`y`}
#' \item \emph{`fill` or `xfill`} Fill color of the xsidebar
#' \item \emph{`fill` or `yfill`} Fill color of the ysidebar
#' \item \emph{`width`} specifies the width of each bar
#' \item \emph{`height`} specifies the height of each bar
#' \item \emph{`alpha`} Transparency level of `xfill` or `yfill`
#' \item \emph{`size`} size of the border line.
#' }
#'
#' @seealso [geom_xsidehistogram], [geom_ysidehistogram]
#' @return XLayer or YLayer object to be added to a ggplot object
#' @aliases geom_*sidebar
#' @examples
#'
#' p <-ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species, fill = Species)) +
#' geom_point()
#'
#' #sidebar - uses StatCount
#' p +
#' geom_xsidebar() +
#' geom_ysidebar()
#'
#' #sidecol - uses Global mapping
#' p +
#'   geom_xsidecol() +
#'   geom_ysidecol()
#'
#' @export
geom_xsidebar <- function(mapping = NULL, data = NULL,
                          stat = "count", position = "stack",
                          ...,
                          width = NULL,
                          na.rm = FALSE,
                          orientation = "x",
                          show.legend = NA,
                          inherit.aes = TRUE) {
  mapping <- default_stat_aes(mapping, stat, orientation)

  ggside_layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXsidebar,
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
GeomXsidebar <- ggside_geom("GeomXsidebar", GeomBar, "x")


#' @rdname geom_xsidebar
#' @export
geom_ysidebar <- function(mapping = NULL, data = NULL,
                          stat = "count", position = "stack",
                          ...,
                          width = NULL,
                          na.rm = FALSE,
                          orientation = "y",
                          show.legend = NA,
                          inherit.aes = TRUE) {
  mapping <- default_stat_aes(mapping, stat, orientation)
  mapping <- force_panel_type_mapping(mapping, "y")
  names(mapping) <- rename_side(names(mapping), "y")
  ggside_layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYsidebar,
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
GeomYsidebar <- ggside_geom("GeomYsidebar", GeomBar, "y")

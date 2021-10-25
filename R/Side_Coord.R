
#'@rdname ggside-ggproto-coord
#'@description
#' S3 class that converts old Coord into one that
#' is compatible with ggside. Can also update
#' ggside on the object. Typically, the new ggproto
#' will inherit from the object being replaced.
#' @param coord coord ggproto Object to replace
#'@export
as_ggsideCoord <- function(coord) UseMethod("as_ggsideCoord")
as_ggsideCoord.default <- function(coord){
  abort(glue("No known method to make {class(coord)[1]} ggside friendly"))
}
as_ggsideCoord.CoordCartesian <- function(coord){
  ggplot2::ggproto(NULL,
                   CoordSideCartesian,
                   limits = coord$limits,
                   expand = coord$expand,
                   default = coord$default,
                   clip = coord$clip)
}

CoordSideCartesian <- ggplot2::ggproto(
  "CoordSideCartesian",
  CoordCartesian,
  render_bg = function(panel_params, theme) {
    panel_type <- eval(quote(self$layout[self$layout$PANEL==i,]$PANEL_TYPE), sys.parent(2))
    if (is.element(panel_type, c("x", "y"))) {
      ggside_guide_grid(
        theme,
        panel_params$x$break_positions_minor(),
        panel_params$x$break_positions(),
        panel_params$y$break_positions_minor(),
        panel_params$y$break_positions()
      )
    } else {
      guide_grid(
        theme,
        panel_params$x$break_positions_minor(),
        panel_params$x$break_positions(),
        panel_params$y$break_positions_minor(),
        panel_params$y$break_positions()
      )
    }
  },
  render_fg = function(panel_params, theme) {
    panel_type <- eval(quote(self$layout[self$layout$PANEL==i,]$PANEL_TYPE), sys.parent(2))
    if (is.element(panel_type, c("x", "y"))) {
      theme[["ggside.panel.border"]] <- theme[["ggside.panel.border"]] %||% theme[["panel.border"]]
      element_render(theme, "ggside.panel.border")
    } else {
      element_render(theme, "panel.border")
    }
  },
  render_axis_h = function (panel_params, theme) {
    panel_type <- panel_params$ggside_panel_type

    if (is.element(panel_type, c("x", "main"))) {
      list(top = panel_guides_grob(panel_params$guides, position = "top", theme = theme),
           bottom = panel_guides_grob(panel_params$guides, position = "bottom", theme = theme))
    } else {
      list(top = ggside_panel_guides_grob(panel_params$guides, position = "top", theme = theme),
           bottom = ggside_panel_guides_grob(panel_params$guides, position = "bottom", theme = theme))
    }
  },
  render_axis_v = function (panel_params, theme) {
    panel_type <- panel_params$ggside_panel_type

    if (is.element(panel_type, c("y", "main"))) {
      list(left = panel_guides_grob(panel_params$guides, position = "left", theme = theme),
           right = panel_guides_grob(panel_params$guides, position = "right", theme = theme))
    } else {
      list(left = ggside_panel_guides_grob(panel_params$guides, position = "left", theme = theme),
           right = ggside_panel_guides_grob(panel_params$guides, position = "right", theme = theme))
    }
  }
)






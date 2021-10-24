
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
  }
)

combine_elements <- function (e1, e2) {
  if (is.null(e2) || inherits(e1, "element_blank")) {
    return(e1)
  }
  if (is.null(e1)) {
    return(e2)
  }
  if (!inherits(e1, "element") && !inherits(e2, "element")) {
    return(e1)
  }
  if (inherits(e2, "element_blank")) {
    if (e1$inherit.blank) {
      return(e2)
    }
    else {
      return(e1)
    }
  }
  n <- names(e1)[vapply(e1, is.null, logical(1))]
  e1[n] <- e2[n]
  if (inherits(e1$size, "rel")) {
    e1$size <- e2$size * unclass(e1$size)
  }
  e1
}
guide_grid <- function (theme, x.minor, x.major, y.minor, y.major) {
  x.minor <- setdiff(x.minor, x.major)
  y.minor <- setdiff(y.minor, y.major)
  ggname("grill",
         grobTree(element_render(theme, "panel.background"),
                  if (length(y.minor) > 0)
                    element_render(theme, "panel.grid.minor.y",
                                   x = rep(0:1,length(y.minor)), y = rep(y.minor, each = 2),
                                   id.lengths = rep(2, length(y.minor))),
                  if (length(x.minor) > 0)
                    element_render(theme, "panel.grid.minor.x",
                                   x = rep(x.minor, each = 2), y = rep(0:1, length(x.minor)),
                                   id.lengths = rep(2,length(x.minor))),
                  if (length(y.major) > 0)
                    element_render(theme, "panel.grid.major.y",
                                   x = rep(0:1, length(y.major)), y = rep(y.major, each = 2),
                                   id.lengths = rep(2, length(y.major))),
                  if (length(x.major) >  0)
                    element_render(theme, "panel.grid.major.x",
                                   x = rep(x.major,  each = 2), y = rep(0:1, length(x.major)),
                                   id.lengths = rep(2, length(x.major)))))
}

setup_ggside_panel_theme <- function(theme) {
  theme[["ggside.panel.background"]] <- theme[["ggside.panel.background"]] %||% theme[["panel.background"]]
  theme[["ggside.panel.grid"]] <- theme[["ggside.panel.grid"]] %||% theme[["panel.grid"]]
  theme[["ggside.panel.grid.major"]] <- theme[["ggside.panel.grid.major"]] %||% theme[["panel.grid.major"]] %||% theme[["ggside.panel.grid"]]
  theme[["ggside.panel.grid.major.x"]] <- theme[["ggside.panel.grid.major.x"]] %||% theme[["panel.grid.major.x"]] %||% theme[["ggside.panel.grid"]]
  theme[["ggside.panel.grid.major.y"]] <- theme[["ggside.panel.grid.major.y"]] %||% theme[["panel.grid.major.y"]] %||% theme[["ggside.panel.grid"]]
  theme[["ggside.panel.grid.minor"]] <- theme[["ggside.panel.grid.minor"]] %||% combine_elements(theme[["ggside.panel.grid"]], theme[["panel.grid.minor"]])
  theme[["ggside.panel.grid.minor.x"]] <- theme[["ggside.panel.grid.minor.x"]] %||% theme[["panel.grid.minor.x"]] %||% theme[["ggside.panel.grid.minor"]]
  theme[["ggside.panel.grid.minor.y"]] <- theme[["ggside.panel.grid.minor.y"]] %||% theme[["panel.grid.minor.y"]] %||% theme[["ggside.panel.grid.minor"]]
  theme
}

ggside_guide_grid <- function(theme, x.minor, x.major, y.minor, y.major) {
    theme <- setup_ggside_panel_theme(theme)
    x.minor <- setdiff(x.minor, x.major)
    y.minor <- setdiff(y.minor, y.major)
    ggname("grill",
           grobTree(element_render(theme, "ggside.panel.background"),
                    if (length(y.minor) > 0)
                      element_render(theme, "ggside.panel.grid.minor.y",
                                     x = rep(0:1,length(y.minor)), y = rep(y.minor, each = 2),
                                     id.lengths = rep(2, length(y.minor))),
                    if (length(x.minor) > 0)
                      element_render(theme, "ggside.panel.grid.minor.x",
                                     x = rep(x.minor, each = 2), y = rep(0:1, length(x.minor)),
                                     id.lengths = rep(2,length(x.minor))),
                    if (length(y.major) > 0)
                      element_render(theme, "ggside.panel.grid.major.y",
                                     x = rep(0:1, length(y.major)), y = rep(y.major, each = 2),
                                     id.lengths = rep(2, length(y.major))),
                    if (length(x.major) >  0)
                      element_render(theme, "ggside.panel.grid.major.x",
                                     x = rep(x.major,  each = 2), y = rep(0:1, length(x.major)),
                                     id.lengths = rep(2, length(x.major)))))

}







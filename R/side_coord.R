
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
  theme[["ggside.panel.grid.minor"]] <- theme[["ggside.panel.grid.minor"]] %||% theme[["panel.grid.minor"]] %||% theme[["ggside.panel.grid"]]
  theme[["ggside.panel.grid.minor.x"]] <- theme[["ggside.panel.grid.minor.x"]] %||% theme[["ggside.panel.grid.minor"]]%||% theme[["panel.grid.minor.x"]]  %||% theme[["ggside.panel.grid"]]
  theme[["ggside.panel.grid.minor.y"]] <- theme[["ggside.panel.grid.minor.y"]] %||% theme[["ggside.panel.grid.minor"]]%||% theme[["panel.grid.minor.y"]]  %||% theme[["ggside.panel.grid"]]
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

#' @export
side_coord <- function(xlim = NULL, ylim = NULL, expand = TRUE,
                       default = FALSE, clip = "on"){
  ggplot2::ggproto(
    NULL,
    CoordCartesian,
    limits = list(x = xlim, y = ylim),
    expand = expand,
    default = default,
    clip = clip,
    render_bg = function(panel_params, theme) {
      panel_type <- eval(quote(self$layout[self$layout$PANEL==i,]$PANEL_TYPE), sys.parent(2))
      if (panel_type == "main") {
        guide_grid(
          theme,
          panel_params$x$break_positions_minor(),
          panel_params$x$break_positions(),
          panel_params$y$break_positions_minor(),
          panel_params$y$break_positions()
        )
      } else if (is.element(panel_type, c("x", "y"))) {
        ggside_guide_grid(
          theme,
          panel_params$x$break_positions_minor(),
          panel_params$x$break_positions(),
          panel_params$y$break_positions_minor(),
          panel_params$y$break_positions()
        )
      } else {
        abort(glue("unexpected value in `ggside` PANEL_TYPE: {panel_type}"))
      }
    }
  )
}

# ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Species)) +
#   geom_point(aes(color = Species)) +
#   geom_xsidedensity(alpha = .3) +
#   side_coord()



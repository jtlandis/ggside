
#' @title ggside custom themes
#' @name ggside-theme
#' @description the following are the theme elements defined in
#' [ggside].
#'
#' @section Elements:
#' \itemize{
#' \item ggside.panel.scale - sets the scaling of side panels relative
#' to the plotting width of the main panels. Default is set to 0.1, i.e. 0.1/1
#' \item ggside.panel.scale.x - same as ggside.panel.scale except only
#' for the xside panel.
#' \item ggside.panel.scale.y - same as ggside.panel.scale except only
#' for the yside panel.
#' \item ggside.panel.spacing - sets how much spacing should be used between
#' the main panels and the side panels. default is unit(2,"pt")
#' \item ggside.panel.spacing.x - same as ggside.panel.spacing except only
#' for the space between the main panel and the yside panel.
#' \item ggside.panel.spacing.y - same as ggside.panel.spacing except only
#' for the space between the main panel and the xside panel.
#' }
#'
NULL



#'
.onLoad <- function(libname, pkgname){
  register_theme_elements(
    ggside.panel.scale = 0.1,
    ggside.panel.scale.x = NULL,
    ggside.panel.scale.y = NULL,
    ggside.panel.spacing = unit(2,"pt"),
    ggside.panel.spacing.x = NULL,
    ggside.panel.spacing.y = NULL,
    # 0.2.0 - panel.*
    ggside.panel.background = NULL,
    ggside.panel.border = NULL,
    ggside.panel.grid = NULL,
    ggside.panel.grid.major = NULL,
    ggside.panel.grid.major.x = NULL,
    ggside.panel.grid.major.y = NULL,
    ggside.panel.grid.minor = NULL,
    ggside.panel.grid.minor.x = NULL,
    ggside.panel.grid.minor.y = NULL,
    # 0.2.0 - axis
    ggside.axis.text = NULL,
    ggside.axis.text.x = NULL,
    ggside.axis.text.x.top = NULL,
    ggside.axis.text.x.bottom = NULL,
    ggside.axis.text.y = NULL,
    ggside.axis.text.y.left = NULL,
    ggside.axis.text.y.right = NULL,
    ggside.axis.line = NULL,
    ggside.axis.line.x = NULL,
    ggside.axis.line.x.top = NULL,
    ggside.axis.line.x.bottom = NULL,
    ggside.axis.line.y = NULL,
    ggside.axis.line.y.left = NULL,
    ggside.axis.line.y.right = NULL,
    ggside.axis.ticks = NULL,
    ggside.axis.ticks.x = NULL,
    ggside.axis.ticks.x.top = NULL,
    ggside.axis.ticks.x.bottom = NULL,
    ggside.axis.ticks.y = NULL,
    ggside.axis.ticks.y.left = NULL,
    ggside.axis.ticks.y.right = NULL,
    ggside.axis.ticks.length = NULL,
    ggside.axis.ticks.length.x = NULL,
    ggside.axis.ticks.length.x.top = NULL,
    ggside.axis.ticks.length.x.bottom = NULL,
    ggside.axis.ticks.length.y = NULL,
    ggside.axis.ticks.length.y.left = NULL,
    ggside.axis.ticks.length.y.right = NULL,
    element_tree = list(ggside.panel.scale = el_def("numeric", "numeric"),
                        ggside.panel.scale.x = el_def("numeric", "ggside.panel.scale"),
                        ggside.panel.scale.y = el_def("numeric", "ggside.panel.scale"),
                        ggside.panel.spacing = el_def("unit", "unit"),
                        ggside.panel.spacing.x = el_def("unit", "ggside.panel.spacing"),
                        ggside.panel.spacing.y = el_def("unit", "ggside.panel.spacing"),
                        ggside.panel.background = el_def("element_rect", "panel.background"),
                        ggside.panel.border = el_def("element_rect", "panel.border"),
                        ggside.panel.grid = el_def("element_line", "panel.grid"),
                        ggside.panel.grid.major = el_def("element_line", "ggside.panel.grid"),
                        ggside.panel.grid.major.x = el_def("element_line", "ggside.panel.grid.major"),
                        ggside.panel.grid.major.y = el_def("element_line", "ggside.panel.grid.major"),
                        ggside.panel.grid.minor = el_def("element_line", "ggside.panel.grid"),
                        ggside.panel.grid.minor.x = el_def("element_line", "ggside.panel.grid.minor"),
                        ggside.panel.grid.minor.y = el_def("element_line", "ggside.panel.grid.minor"),
                        ggside.axis.text = el_def("element_text", "axis.text"),
                        ggside.axis.text.x = el_def("element_text", "ggside.axis.text"),
                        ggside.axis.text.x.top = el_def("element_text", "ggside.axis.text.x"),
                        ggside.axis.text.x.bottom = el_def("element_text", "ggside.axis.text.x"),
                        ggside.axis.text.y = el_def("element_text", "ggside.axis.text"),
                        ggside.axis.text.y.left = el_def("element_text", "ggside.axis.text"),
                        ggside.axis.text.y.right = el_def("element_text", "ggside.axis.text.y"),
                        ggside.axis.line = el_def("element_line", "axis.line"),
                        ggside.axis.line.x = el_def("element_line", "ggside.axis.line"),
                        ggside.axis.line.x.top = el_def("element_line", "ggside.axis.line.x"),
                        ggside.axis.line.x.bottom = el_def("element_line", "ggside.axis.line.x"),
                        ggside.axis.line.y = el_def("element_line", "ggside.axis.line"),
                        ggside.axis.line.y.left = el_def("element_line", "ggside.axis.line.y"),
                        ggside.axis.line.y.right = el_def("element_line", "ggside.axis.line.y"),
                        ggside.axis.ticks = el_def("element_line", "axis.ticks"),
                        ggside.axis.ticks.x = el_def("element_line", "ggside.axis.ticks"),
                        ggside.axis.ticks.x.top = el_def("element_line", "ggside.axis.ticks.x"),
                        ggside.axis.ticks.x.bottom = el_def("element_line", "ggside.axis.ticks.x"),
                        ggside.axis.ticks.y = el_def("element_line", "ggside.axis.ticks"),
                        ggside.axis.ticks.y.left = el_def("element_line", "ggside.axis.ticks.y"),
                        ggside.axis.ticks.y.right = el_def("element_line", "ggside.axis.ticks.y"),
                        ggside.axis.ticks.length = el_def("unit", "axis.ticks.length"),
                        ggside.axis.ticks.length.x = el_def("unit", "ggside.axis.ticks.length"),
                        ggside.axis.ticks.length.x.top = el_def("unit", "ggside.axis.ticks.length.x"),
                        ggside.axis.ticks.length.x.bottom = el_def("unit", "ggside.axis.ticks.length.x"),
                        ggside.axis.ticks.length.y = el_def("unit", "ggside.axis.ticks.length"),
                        ggside.axis.ticks.length.y.left = el_def("unit", "ggside.axis.ticks.length.y"),
                        ggside.axis.ticks.length.y.right = el_def("unit", "ggside.axis.ticks.length.y")
                        )
    )
}

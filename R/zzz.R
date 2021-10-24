
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
    ggside.panel.background = NULL,
    ggside.panel.grid = NULL,
    ggside.panel.grid.major = NULL,
    ggside.panel.grid.major.x = NULL,
    ggside.panel.grid.major.y = NULL,
    ggside.panel.grid.minor = NULL,
    ggside.panel.grid.minor.x = NULL,
    ggside.panel.grid.minor.y = NULL,
    element_tree = list(ggside.panel.scale = el_def("numeric", "numeric"),
                        ggside.panel.scale.x = el_def("numeric", "numeric"),
                        ggside.panel.scale.y = el_def("numeric", "numeric"),
                        ggside.panel.spacing = el_def("unit", "unit"),
                        ggside.panel.spacing.x = el_def("unit", "unit"),
                        ggside.panel.spacing.y = el_def("unit", "unit"),
                        ggside.panel.background = el_def("element_rect", "rect"),
                        ggside.panel.grid = el_def("element_line", "line"),
                        ggside.panel.grid.major = el_def("element_line", "line"),
                        ggside.panel.grid.major.x = el_def("element_line", "line"),
                        ggside.panel.grid.major.y = el_def("element_line", "line"),
                        ggside.panel.grid.minor = el_def("element_line", "line"),
                        ggside.panel.grid.minor.x = el_def("element_line", "line"),
                        ggside.panel.grid.minor.y = el_def("element_line", "line")
                        )
    )
}

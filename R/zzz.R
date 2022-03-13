
#' @title ggside custom themes
#' @name ggside-theme
#' @description Theme elements to help customize the look and feel of
#' [ggside]'s side panels.
#'
#' @inheritParams ggplot2::theme_grey
#'
#' @details Incomplete themes:
#'
#' Unlike the complete themes like \link[ggplot2]{theme_grey}, `ggside`'s variants are not
#' considered "complete". This is because the user may want to specify the side panels
#' separately from the theme of the main panel. This means that `theme_ggside_*()` functions
#' should be called after any of `ggplot2`'s complete themes.
#'
#' @section ggside theme elements:
#'
#' \tabular{ll}{
#' \code{ggside.panel.scale, ggside.panel.scale.x, ggside.panel.scale.y} \tab expects a scalar numeric
#' that sets the scaling of side panels relative to the plotting width/height of the main panels.
#' Default is set to 0.1. i.e. 0.1 indicates side panels are 1/10th the size of the main panel whereas 1
#' indicates side panel are the same size as main panels. `.x` will set the scale for the xside panel
#' and `.y` will set the scale for the yside panel. \cr
#' \tab \cr
#' \code{ggside.panel.spacing, ggside.panel.spacing.x, ggside.panel.spacing.y} \tab expects a scalar unit
#' that sets the spacing between side panels and main panels. Default facet spacing is typically
#' `unit(5.5,"pt")` whereas this element's default is `unit(2,"pt")` to indicate the relationship
#' a side panel has to the main panels. `.x` will set the space between the main panel and the yside panel,
#' where as `.y` will set the space between the main panel and the xside panel. \cr
#' \tab \cr
#' \code{ggside.panel.background} \tab Sets the background of the side panels. If unspecified, side panels
#' inherit from `panel.background` \cr
#' \tab \cr
#' \code{ggside.panel.grid, ggside.panel.grid.major, ggside.panel.grid.minor,
#' ggside.panel.grid.major.x, ggside.panel.grid.major.y, ggside.panel.grid.minor.x, ggside.panel.grid.minor.y}
#' \tab
#' Grid lines for the side panels. These elements inherit from `panel.grid` and will default
#' to the current theme's `panel.grid` inheritance unless specifically set. \cr
#' \tab \cr
#' \code{ggside.axis.text, ggside.axis.text.x, ggside.axis.text.y,
#'  ggside.axis.text.x.top, ggside.axis.text.x.bottom, ggside.axis.text.y.left, ggside.axis.text.y.right} \tab
#' Tick labels along the side panel's axis. Due to the layout of side panels, `ggside.axis.text.x`
#' will only affect the yside panel's x-axis text and `ggside.axis.text.y` will
#' only affect the xside panel's y-axis text. These elements inherit from `axis.text`
#' and will default to the current theme's `axis.text` inheritance scheme unless
#' specifically set. \cr
#' \tab \cr
#' \code{ggside.axis.line, ggside.axis.line.x, ggside.axis.line.y,
#' ggside.axis.line.x.top, ggside.axis.line.x.bottom, ggside.axis.line.y.left, ggside.axis.line.y.right} \tab
#' Lines along the side panel's axis.Due to the layout of side panels, `ggside.axis.line.x`
#' will only affect the yside panel's x-axis text and `ggside.axis.line.y` will
#' only affect the xside panel's y-axis text. Theme elements inherit from `axis.line`
#' and will default to the current theme's `axis.line` inheritance scheme unless
#' specifically set. \cr
#' \tab \cr
#' \code{ggside.axis.ticks, ggside.axis.ticks.x, ggside.axis.ticks.y,
#' ggside.axis.ticks.x.top, ggside.axis.ticks.x.bottom, ggside.axis.ticks.y.left, ggside.axis.ticks.y.right} \tab
#' Tick marks along the side panel's axis. Due to the layout of side panels, `ggside.axis.ticks.x`
#' will only affect the yside panel's x-axis text and `ggside.axis.ticks.y` will
#' only affect the xside panel's y-axis text. Theme elements inherit from `axis.ticks`
#' and will default to the current theme's `axis.ticks` inheritance scheme unless
#' specifically set. \cr
#' \tab \cr
#' \code{ggside.axis.ticks.length, ggside.axis.ticks.length.x,
#' ggside.axis.ticks.length.y, ggside.axis.ticks.length.x.top, ggside.axis.ticks.length.x.bottom,
#' ggside.axis.ticks.length.y.left, ggside.axis.ticks.length.y.right} \tab
#' length of ticks along the side panel's axis.Due to the layout of side panels, `ggside.axis.ticks.length.x`
#' will only affect the yside panel's x-axis text and `ggside.axis.ticks.length.y` will
#' only affect the xside panel's y-axis text. Theme elements inherit from `axis.ticks.length`
#' and will default to the current theme's `axis.ticks.length` inheritance scheme unless
#' specifically set. \cr
#' \tab \cr
#' }
#'
#' @examples
#'
#' library(ggplot2)
#' library(ggside)
#'
#' p <- ggplot(iris, aes(Sepal.Width, Petal.Length, color = Species)) +
#'  geom_point() +
#'  geom_xsidedensity() +
#'  geom_ysidedensity() +
#'  theme_dark()
#'
#' p
#'
#' p + theme_ggside_classic()
#' p + theme_ggside_void()
#' p + theme_ggside_linedraw() +
#' theme(ggside.panel.border = element_rect(colour = "red"))
NULL



#'
.onLoad <- function(libname, pkgname){
  register_theme_elements(
    # base elements,
    ggside.line = NULL,
    ggside.rect = NULL,
    ggside.text = NULL,
    # vanilla
    ggside.panel.scale = 0.1,
    ggside.panel.scale.x = NULL,
    ggside.panel.scale.y = NULL,
    ggside.panel.spacing = unit(2,"pt"),
    ggside.panel.spacing.x = NULL,
    ggside.panel.spacing.y = NULL,
    # 0.2.0 - panel.*
    ggside.panel.background = NULL,
    ggside.panel.border = NULL, #ensure we dont inherit a white background from element_rect default
    ggside.panel.grid = NULL,
    ggside.panel.grid.major = NULL,
    ggside.panel.grid.major.x = NULL,
    ggside.panel.grid.major.y = NULL,
    ggside.panel.grid.minor = NULL,
    ggside.panel.grid.minor.x = NULL,
    ggside.panel.grid.minor.y = NULL,
    ggside.xside.panel.background = NULL,
    ggside.xside.panel.border = NULL,
    ggside.xside.panel.grid = NULL,
    ggside.xside.panel.grid.major = NULL,
    ggside.xside.panel.grid.major.x = NULL,
    ggside.xside.panel.grid.major.y = NULL,
    ggside.xside.panel.grid.minor = NULL,
    ggside.xside.panel.grid.minor.x = NULL,
    ggside.xside.panel.grid.minor.y = NULL,
    ggside.yside.panel.background = NULL,
    ggside.yside.panel.border = NULL,
    ggside.yside.panel.grid = NULL,
    ggside.yside.panel.grid.major = NULL,
    ggside.yside.panel.grid.major.x = NULL,
    ggside.yside.panel.grid.major.y = NULL,
    ggside.yside.panel.grid.minor = NULL,
    ggside.yside.panel.grid.minor.x = NULL,
    ggside.yside.panel.grid.minor.y = NULL,
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
    element_tree = list(ggside.line = el_def("element_line"),
                        ggside.rect = el_def("element_rect"),
                        ggside.text = el_def("element_text"),
                        ggside.panel.scale = el_def("numeric", "numeric"),
                        ggside.panel.scale.x = el_def("numeric", "ggside.panel.scale"),
                        ggside.panel.scale.y = el_def("numeric", "ggside.panel.scale"),
                        ggside.panel.spacing = el_def("unit", "unit"),
                        ggside.panel.spacing.x = el_def("unit", "ggside.panel.spacing"),
                        ggside.panel.spacing.y = el_def("unit", "ggside.panel.spacing"),
                        ggside.panel.background = el_def("element_rect", c("ggside.rect","panel.background")),
                        ggside.panel.border = el_def("element_rect", c("ggside.rect","panel.border")),
                        ggside.panel.grid = el_def("element_line", "ggside.line"),
                        ggside.panel.grid.major = el_def("element_line", "ggside.panel.grid"),
                        ggside.panel.grid.major.x = el_def("element_line", c("ggside.panel.grid.major","panel.grid.major.x")),
                        ggside.panel.grid.major.y = el_def("element_line", c("ggside.panel.grid.major","panel.grid.major.y")),
                        ggside.panel.grid.minor = el_def("element_line", "ggside.panel.grid"),
                        ggside.panel.grid.minor.x = el_def("element_line", c("ggside.panel.grid.minor","panel.grid.minor.x")),
                        ggside.panel.grid.minor.y = el_def("element_line", c("ggside.panel.grid.minor","panel.grid.minor.y")),

                        ggside.xside.panel.background = el_def("element_rect", c("ggside.rect","ggside.panel.background")),
                        ggside.xside.panel.border = el_def("element_rect", c("ggside.rect", "ggside.panel.border")),
                        ggside.xside.panel.grid = el_def("element_line", c("ggside.line")),
                        ggside.xside.panel.grid.major = el_def("element_line", c("ggside.xside.panel.grid")),
                        ggside.xside.panel.grid.major.x = el_def("element_line", c("ggside.xside.panel.grid.major","ggside.panel.grid.major.x")),
                        ggside.xside.panel.grid.major.y = el_def("element_line", c("ggside.xside.panel.grid.major","ggside.panel.grid.major.y")),
                        ggside.xside.panel.grid.minor = el_def("element_line", c("ggside.xside.panel.grid")),
                        ggside.xside.panel.grid.minor.x = el_def("element_line", c("ggside.xside.panel.grid.minor","ggside.panel.grid.minor.x")),
                        ggside.xside.panel.grid.minor.y = el_def("element_line", c("ggside.xside.panel.grid.minor","ggside.panel.grid.minor.y")),

                        ggside.yside.panel.background = el_def("element_rect", c("ggside.rect","ggside.panel.background")),
                        ggside.yside.panel.border = el_def("element_rect", c("ggside.rect", "ggside.panel.border")),
                        ggside.yside.panel.grid = el_def("element_line", c("ggside.line","ggside.panel.grid")),
                        ggside.yside.panel.grid.major = el_def("element_line", c("ggside.yside.panel.grid")),
                        ggside.yside.panel.grid.major.x = el_def("element_line", c("ggside.yside.panel.grid.major","ggside.panel.grid.major.x")),
                        ggside.yside.panel.grid.major.y = el_def("element_line", c("ggside.yside.panel.grid.major","ggside.panel.grid.major.y")),
                        ggside.yside.panel.grid.minor = el_def("element_line", c("ggside.yside.panel.grid")),
                        ggside.yside.panel.grid.minor.x = el_def("element_line", c("ggside.yside.panel.grid.minor","ggside.panel.grid.minor.x")),
                        ggside.yside.panel.grid.minor.y = el_def("element_line", c("ggside.yside.panel.grid.minor","ggside.panel.grid.minor.y")),


                        ggside.axis.text = el_def("element_text", "ggside.text"),
                        ggside.axis.text.x = el_def("element_text", "ggside.axis.text"),
                        ggside.axis.text.x.top = el_def("element_text", c("ggside.axis.text.x","axis.text.x.top")),
                        ggside.axis.text.x.bottom = el_def("element_text", c("ggside.axis.text.x", "axis.text.x.bottom")),
                        ggside.axis.text.y = el_def("element_text", "ggside.axis.text"),
                        ggside.axis.text.y.left = el_def("element_text", c("ggside.axis.text.y", "axis.text.y.left")),
                        ggside.axis.text.y.right = el_def("element_text", c("ggside.axis.text.y", "axis.text.y.right")),
                        ggside.axis.line = el_def("element_line", "ggside.line"),
                        ggside.axis.line.x = el_def("element_line", "ggside.axis.line"),
                        ggside.axis.line.x.top = el_def("element_line", c("ggside.axis.line.x","axis.line.x.top")),
                        ggside.axis.line.x.bottom = el_def("element_line", c("ggside.axis.line.x","axis.line.x.bottom")),
                        ggside.axis.line.y = el_def("element_line", "ggside.axis.line"),
                        ggside.axis.line.y.left = el_def("element_line", c("ggside.axis.line.y","axis.line.y.left")),
                        ggside.axis.line.y.right = el_def("element_line", c("ggside.axis.line.y","axis.line.y.right")),
                        ggside.axis.ticks = el_def("element_line", "ggside.line"),
                        ggside.axis.ticks.x = el_def("element_line", "ggside.axis.ticks"),
                        ggside.axis.ticks.x.top = el_def("element_line", c("ggside.axis.ticks.x","axis.ticks.x.top")),
                        ggside.axis.ticks.x.bottom = el_def("element_line", c("ggside.axis.ticks.x","axis.ticks.x.bottom")),
                        ggside.axis.ticks.y = el_def("element_line", "ggside.axis.ticks"),
                        ggside.axis.ticks.y.left = el_def("element_line", c("ggside.axis.ticks.y","axis.ticks.y.left")),
                        ggside.axis.ticks.y.right = el_def("element_line", c("ggside.axis.ticks.y","axis.ticks.y.right")),
                        ggside.axis.ticks.length = el_def("unit", "axis.ticks.length"),
                        ggside.axis.ticks.length.x = el_def("unit", "ggside.axis.ticks.length"),
                        ggside.axis.ticks.length.x.top = el_def("unit", c("ggside.axis.ticks.length.x","axis.ticks.length.x.top")),
                        ggside.axis.ticks.length.x.bottom = el_def("unit", c("ggside.axis.ticks.length.x","axis.ticks.length.x.bottom")),
                        ggside.axis.ticks.length.y = el_def("unit", "ggside.axis.ticks.length"),
                        ggside.axis.ticks.length.y.left = el_def("unit", c("ggside.axis.ticks.length.y","axis.ticks.length.y.left")),
                        ggside.axis.ticks.length.y.right = el_def("unit", c("ggside.axis.ticks.length.y","axis.ticks.length.y.right"))
                        )
    )
}

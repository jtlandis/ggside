#' Scales for the *fill aesthetics
#'
#' These are the various scales that can be applied to the xsidebar or ysidebar
#' fill aesthetics, such as `xfill` and `yfill`. They have the same usage as
#' existing standard ggplot2 scales.
#'
#' @name scale_*fill
NULL

#' scale_xfill_hue
#' @rdname scale_*fill
#' @usage NULL
#' @export
scale_xfill_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                            direction = 1, na.value = "grey50", aesthetics = "xfill")
{
  ggplot2::discrete_scale(aesthetics, "hue",
                          scales::hue_pal(h, c, l, h.start, direction), na.value = na.value, ...)
}

#' scale_xfill_gradient
#' @rdname scale_*fill
#' @usage NULL
#' @export
scale_xfill_gradient <- function (..., low = "#132B43", high = "#56B1F7",
                                  space = "Lab",na.value = "grey50",
                                  guide = "colourbar", aesthetics = "xfill")
{
  continuous_scale(aesthetics,
                   "gradient",
                   seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...)
}

scale_xfill_gradientn <- function (..., colours, values = NULL,
                                   space = "Lab", na.value = "grey50",
                                   guide = "colourbar", aesthetics = "fill", colors)
{
  colours <- if (missing(colours))
    colors
  else colours
  continuous_scale(aesthetics, "gradientn",
                   gradient_n_pal(colours,values, space),
                   na.value = na.value, guide = guide, ...)
}

#' scale_xfill_discrete
#' @rdname scale_*fill
#' @usage NULL
#' @export
scale_xfill_discrete <- scale_xfill_hue

#' scale_xfill_continuous
#' @rdname scale_*fill
#' @usage NULL
#' @export
scale_xfill_continuous <- scale_xfill_gradient

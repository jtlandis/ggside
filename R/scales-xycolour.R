#' Scales for the *colour aesthetics
#'
#' These are the various scales that can be applied to the xsidebar or ysidebar
#' colour aesthetics, such as xcolour and ycolour. They have the same usage as
#' existing standard ggplot2 scales.
#'
#' @name scale_xcolour
#' @aliases scale_ycolour scale_xcolor scale_ycolor
#'
#' @section Related Functions:
#'
#' \itemize{
#' \item scale_xcolour_hue
#' \item scale_ycolour_hue
#' \item scale_xcolour_discrete
#' \item scale_ycolour_discrete
#' \item scale_xcolour_continuous
#' \item scale_ycolour_continuous
#' \item scale_xcolour_manual
#' \item scale_ycolour_manual
#' \item scale_xcolour_gradient
#' \item scale_ycolour_gradient
#' \item scale_xcolour_gradientn
#' \item scale_ycolour_gradientn
#' }
#' @return returns a ggroto object to be added to a ggplot
NULL

#' scale_xcolour_hue
#' @rdname scale_xcolour
#' @usage NULL
#' @export
scale_xcolour_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                            direction = 1, na.value = "grey50", aesthetics = "xcolour")
{
  ggplot2::discrete_scale(aesthetics, "hue",
                          scales::hue_pal(h, c, l, h.start, direction), na.value = na.value, ...)
}

#' scale_xcolour_manual
#' @rdname scale_xcolour
#' @usage NULL
#' @export
scale_xcolour_manual <- function(..., values, aesthetics = "xcolour", breaks = waiver()) {
  manual_scale(aesthetics, values, breaks, ...)
}

#' @rdname scale_xcolour
#' @usage NULL
#' @export
scale_xcolor_manual <- function(..., values, aesthetics = "xcolour", breaks = waiver()) {
  manual_scale(aesthetics, values, breaks, ...)
}

#' scale_xcolour_gradient
#' @rdname scale_xcolour
#' @usage NULL
#' @export
scale_xcolour_gradient <- function (..., low = "#132B43", high = "#56B1F7",
                                  space = "Lab",na.value = "grey50",
                                  guide = guide_colorbar(available_aes = "xcolour"), aesthetics = "xcolour")
{
  continuous_scale(aesthetics,
                   "gradient",
                   scales::seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...)
}

#' @rdname scale_xcolour
#' @usage NULL
#' @export
scale_xcolor_gradientn <- function (..., colours, values = NULL,
                                   space = "Lab", na.value = "grey50",
                                   guide = guide_colorbar(available_aes = "xcolour"), aesthetics = "xcolour", colors)
{
  colours <- if (missing(colours))
    colors
  else colours
  continuous_scale(aesthetics, "gradientn",
                   scales::gradient_n_pal(colours,values, space),
                   na.value = na.value, guide = guide, ...)
}

#' @rdname scale_xcolour
#' @usage NULL
#' @export
scale_xcolour_gradientn <- function (..., colours, values = NULL,
                                     space = "Lab", na.value = "grey50",
                                     guide = guide_colorbar(available_aes = "xcolour"), aesthetics = "xcolour", colors)
{
  colours <- if (missing(colours))
    colors
  else colours
  continuous_scale(aesthetics, "gradientn",
                   scales::gradient_n_pal(colours,values, space),
                   na.value = na.value, guide = guide, ...)
}

#' scale_xcolour_discrete
#' @rdname scale_xcolour
#' @usage NULL
#' @export
scale_xcolour_discrete <- scale_xcolour_hue

#' scale_xcolor_discrete
#' @rdname scale_xcolour
#' @usage NULL
#' @export
scale_xcolor_discrete <- scale_xcolour_hue

#' scale_xcolour_continuous
#' @rdname scale_xcolour
#' @usage NULL
#' @export
scale_xcolour_continuous <- scale_xcolour_gradient

#' scale_xcolour_continuous
#' @rdname scale_xcolour
#' @usage NULL
#' @export
scale_xcolor_continuous <- scale_xcolour_gradient


#' scale_ycolour_hue
#' @rdname scale_ycolour
#' @usage NULL
#' @export
scale_ycolour_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                            direction = 1, na.value = "grey50", aesthetics = "ycolour")
{
  ggplot2::discrete_scale(aesthetics, "hue",
                          scales::hue_pal(h, c, l, h.start, direction), na.value = na.value, ...)
}

#' scale_ycolour_manual
#' @rdname scale_ycolour
#' @usage NULL
#' @export
scale_ycolour_manual <- function(..., values, aesthetics = "ycolour", breaks = waiver()) {
  manual_scale(aesthetics, values, breaks, ...)
}

#' @rdname scale_ycolour
#' @usage NULL
#' @export
scale_ycolor_manual <- function(..., values, aesthetics = "ycolour", breaks = waiver()) {
  manual_scale(aesthetics, values, breaks, ...)
}

#' scale_ycolour_gradient
#' @rdname scale_ycolour
#' @usage NULL
#' @export
scale_ycolour_gradient <- function (..., low = "#132B43", high = "#56B1F7",
                                  space = "Lab",na.value = "grey50",
                                  guide = guide_colorbar(available_aes = "ycolour"), aesthetics = "ycolour")
{
  continuous_scale(aesthetics,
                   "gradient",
                   scales::seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...)
}

#' @rdname scale_ycolour
#' @usage NULL
#' @export
scale_ycolour_gradientn <- function (..., colours, values = NULL,
                                   space = "Lab", na.value = "grey50",
                                   guide = guide_colorbar(available_aes = "ycolour"), aesthetics = "ycolour", colors)
{
  colours <- if (missing(colours))
    colors
  else colours
  continuous_scale(aesthetics, "gradientn",
                   scales::gradient_n_pal(colours,values, space),
                   na.value = na.value, guide = guide, ...)
}

#' @rdname scale_ycolour
#' @usage NULL
#' @export
scale_ycolor_gradientn <- function (..., colours, values = NULL,
                                     space = "Lab", na.value = "grey50",
                                     guide = guide_colorbar(available_aes = "ycolour"), aesthetics = "ycolour", colors)
{
  colours <- if (missing(colours))
    colors
  else colours
  continuous_scale(aesthetics, "gradientn",
                   scales::gradient_n_pal(colours,values, space),
                   na.value = na.value, guide = guide, ...)
}

#' scale_ycolour_discrete
#' @rdname scale_ycolour
#' @usage NULL
#' @export
scale_ycolour_discrete <- scale_ycolour_hue

#' scale_ycolour_discrete
#' @rdname scale_ycolour
#' @usage NULL
#' @export
scale_ycolor_discrete <- scale_ycolour_hue

#' scale_ycolour_continuous
#' @rdname scale_ycolour
#' @usage NULL
#' @export
scale_ycolour_continuous <- scale_ycolour_gradient

#' scale_ycolour_continuous
#' @rdname scale_ycolour
#' @usage NULL
#' @export
scale_ycolor_continuous <- scale_ycolour_gradient








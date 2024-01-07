### INCLUDE BEGIN
#' @include aab-other_utils.r
NULL
### INCLUDE END
#' Scales for the *fill aesthetics
#'
#' These are the various scales that can be applied to the xsidebar or ysidebar
#' fill aesthetics, such as xfill and yfill. They have the same usage as
#' existing standard ggplot2 scales.
#'
#' @name scale_xfill
#' @aliases scale_yfill
#'
#' @section Related Functions:
#'
#' \itemize{
#' \item scale_xfill_hue
#' \item scale_yfill_hue
#' \item scale_xfill_discrete
#' \item scale_yfill_discrete
#' \item scale_xfill_continuous
#' \item scale_yfill_continuous
#' \item scale_xfill_manual
#' \item scale_yfill_manual
#' \item scale_xfill_gradient
#' \item scale_yfill_gradient
#' \item scale_xfill_gradientn
#' \item scale_yfill_gradientn
#' }
#' @return returns a ggproto object to be added to a ggplot
NULL

#' scale_xfill_hue
#' @rdname scale_xfill
#' @usage NULL
#' @export
scale_xfill_hue <- function(name = waiver(), ..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                            direction = 1, na.value = "grey50", aesthetics = "xfill")
{
  ggplot2::discrete_scale(aesthetics = aesthetics,
                          name = name,
                          palette =  scales::hue_pal(h, c, l, h.start, direction),
                          na.value = na.value,
                          ...)
}

#' scale_xfill_manual
#' @rdname scale_xfill
#' @usage NULL
#' @export
scale_xfill_manual <- function(..., values, aesthetics = "xfill", breaks = waiver(), na.value = "grey50") {
  manual_scale(aesthetics, values, breaks, ..., na.value = na.value)
}

#' scale_xfill_gradient
#' @rdname scale_xfill
#' @usage NULL
#' @export
scale_xfill_gradient <- function (name = waiver(), ..., low = "#132B43", high = "#56B1F7",
                                  space = "Lab", na.value = "grey50",
                                  guide = guide_colorbar(available_aes = "xfill"), aesthetics = "xfill")
{
  continuous_scale(aesthetics = aesthetics,
                   name = name,
                   palette = scales::seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...)
}

#' @rdname scale_xfill
#' @usage NULL
#' @export
scale_xfill_gradientn <- function (name = waiver(), ..., colours, values = NULL,
                                   space = "Lab", na.value = "grey50",
                                   guide = guide_colorbar(available_aes = "xfill"), aesthetics = "xfill", colors)
{
  colours <- if (missing(colours))
    colors
  else colours
  continuous_scale(aesthetics = aesthetics, "gradientn", name = name,
                   palette = scales::gradient_n_pal(colours, values, space),
                   na.value = na.value, guide = guide, ...)
}

#' scale_xfill_discrete
#' @rdname scale_xfill
#' @usage NULL
#' @export
scale_xfill_discrete <- scale_xfill_hue

#' scale_xfill_continuous
#' @rdname scale_xfill
#' @usage NULL
#' @export
scale_xfill_continuous <- scale_xfill_gradient


#' scale_yfill_hue
#' @rdname scale_yfill
#' @usage NULL
#' @export
scale_yfill_hue <- function(name = waiver(), ..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                            direction = 1, na.value = "grey50", aesthetics = "yfill")
{
  ggplot2::discrete_scale(aesthetics = aesthetics, name = name,
                          palette = scales::hue_pal(h, c, l, h.start, direction), na.value = na.value, ...)
}

#' scale_yfill_manual
#' @rdname scale_yfill
#' @usage NULL
#' @export
scale_yfill_manual <- function(..., values, aesthetics = "yfill", breaks = waiver()) {
  manual_scale(aesthetics, values, breaks, ...)
}

#' scale_yfill_gradient
#' @rdname scale_yfill
#' @usage NULL
#' @export
scale_yfill_gradient <- function (name = waiver(), ..., low = "#132B43", high = "#56B1F7",
                                  space = "Lab",na.value = "grey50",
                                  guide = guide_colorbar(available_aes = "yfill"), aesthetics = "yfill")
{
  continuous_scale(aesthetics = aesthetics,
                   name = name,
                   palette = scales::seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...)
}

#' @rdname scale_xfill
#' @usage NULL
#' @export
scale_yfill_gradientn <- function (name = waiver(), ..., colours, values = NULL,
                                   space = "Lab", na.value = "grey50",
                                   guide = guide_colorbar(available_aes = "yfill"), aesthetics = "yfill", colors)
{
  colours <- if (missing(colours))
    colors
  else colours
  continuous_scale(aesthetics = aesthetics, name = name,
                   palette = scales::gradient_n_pal(colours,values, space),
                   na.value = na.value, guide = guide, ...)
}

#' scale_yfill_discrete
#' @rdname scale_yfill
#' @usage NULL
#' @export
scale_yfill_discrete <- scale_yfill_hue

#' scale_yfill_continuous
#' @rdname scale_yfill
#' @usage NULL
#' @export
scale_yfill_continuous <- scale_yfill_gradient

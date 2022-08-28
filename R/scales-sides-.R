
#' @title Position scales for continuous data ggside scales
#'
#' @name ggside-scales-continuous
#'
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{scale_x_continuous}/\link[ggplot2]{scale_y_continuous}.
#' [scale_xsidey_continuous] enables better control on how the y-axis is rendered on the xside panel and
#' [scale_ysidex_continuous] enables better control on how the x-axis is rendered on the yside panel.
#'
#' @inheritParams ggplot2::continuous_scale
#' @return ggside_scale object inheriting from ggplot2::ScaleContinuousPosition
#' @examples
#'
#' library(ggside)
#' library(ggplot2)
#' # adding continuous y-scale to the x-side panel, when main panel mapped to discrete data
#' ggplot(mpg, aes(hwy, class, colour = class)) +
#'   geom_boxplot() +
#'   geom_xsidedensity(position = "stack") +
#'   theme(ggside.panel.scale = .3) +
#'   scale_xsidey_continuous(minor_breaks = NULL, limits = c(NA,1))
#'
#' #If you need to specify the main scale, but need to prevent this from
#' #affecting the side scale. Simply add the appropriate `scale_*side*_*()` function.
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   geom_xsidehistogram() +
#'   geom_ysidehistogram()  +
#'   scale_x_continuous(
#'       breaks = seq(1, 6, 1),
#'       #would otherwise remove the histogram
#'       #as they have a lower value of 0.
#'       limits = (c(1, 6))
#'       ) +
#'   scale_ysidex_continuous() #ensures the x-axis of the y-side panel has its own scale.
NULL

#' @rdname ggside-scales-continuous
#' @usage NULL
#' @export
scale_xsidey_continuous <- function(name = waiver(), breaks = waiver(), minor_breaks = waiver(),
                                    n.breaks = NULL, labels = waiver(), limits = NULL, expand = waiver(),
                                    oob = scales::censor, na.value = NA_real_, trans = "identity", guide = waiver(),
                                    position = "left", sec.axis = waiver()){

  new_oob <- muffle_opts_warn(oob)

  sc <- continuous_scale(c("y", "ymin", "ymax", "yend", "yintercept",
                           "ymin_final", "ymax_final", "lower", "middle", "upper",
                           "y0","xsidey"), "position_c", identity, name = name, breaks = breaks,
                         n.breaks = n.breaks, minor_breaks = minor_breaks, labels = labels,
                         limits = limits, expand = expand, oob = new_oob, na.value = na.value,
                         trans = trans, guide = guide, position = position, super = ScaleContinuousPosition)
  sc <- set_sec_axis(sec.axis, sc)
  structure(sc,
            class = c("ggside_scale", class(sc)))

}


#' @rdname ggside-scales-continuous
#' @usage NULL
#' @export
scale_ysidex_continuous <- function(name = waiver(), breaks = waiver(), minor_breaks = waiver(),
                                    n.breaks = NULL, labels = waiver(), limits = NULL, expand = waiver(),
                                    oob = scales::censor, na.value = NA_real_, trans = "identity", guide = waiver(),
                                    position = "bottom", sec.axis = waiver()){
  new_oob <- muffle_opts_warn(oob)

  sc <- continuous_scale(c("x", "xmin", "xmax", "xend", "xintercept",
                           "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper",
                           "x0", "ysidex"), "position_c", identity, name = name, breaks = breaks,
                         n.breaks = n.breaks, minor_breaks = minor_breaks, labels = labels,
                         limits = limits, expand = expand, oob = new_oob, na.value = na.value,
                         trans = trans, guide = guide, position = position, super = ScaleContinuousPosition)
  sc <- set_sec_axis(sec.axis, sc)
  structure(sc,
            class = c("ggside_scale", class(sc)))

}


#' @title Position scales for discrete data ggside scales
#' @name ggside-scales-discrete
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{scale_x_discrete}/\link[ggplot2]{scale_y_discrete}.
#' [scale_xsidey_discrete] enables better control on how the y-axis is rendered on the xside panel and
#' [scale_ysidex_discrete] enables better control on how the x-axis is rendered on the yside panel.
#'
#' @inheritParams ggplot2::discrete_scale
#' @return ggside_scale object inheriting from ggplot2::ScaleDiscretePosition
#' @examples
#'
#' library(ggside)
#' library(ggplot2)
#' # adding discrete y-scale to the x-side panel, when main panel mapped to continuous data
#' ggplot(mpg, aes(displ, hwy, colour = class)) +
#'   geom_point() +
#'   geom_xsideboxplot(aes(y=class), orientation = "y") +
#'   theme(ggside.panel.scale = .3) +
#'   scale_xsidey_discrete(guide = guide_axis(angle = 45))
#'
#' #If you need to specify the main scale, but need to prevent this from
#' #affecting the side scale. Simply add the appropriate `scale_*side*_*()` function.
#' ggplot(mpg, aes(class, displ)) +
#'   geom_boxplot() +
#'   geom_ysideboxplot(aes(x = "all"), orientation = "x") +
#'   scale_x_discrete(guide = guide_axis(angle = 90)) + #rotate the main panel text
#'   scale_ysidex_discrete() #leave side panel as default
NULL

#' @rdname ggside-scales-discrete
#' @usage NULL
#' @export
scale_xsidey_discrete <- function(..., expand = waiver(),
                                  guide = waiver(), position = "left") {

  sc <- discrete_scale(c("y", "ymin", "ymax", "yend", "xsidey"), "position_d",
                       identity, ..., expand = expand, guide = guide, position = position,
                       super = ScaleDiscretePosition)
  sc$range_c <- continuous_range()
  structure(sc,
            class = c("ggside_scale", class(sc)))

}

#' @rdname ggside-scales-discrete
#' @usage NULL
#' @export
scale_ysidex_discrete <- function(..., expand = waiver(),
                                  guide = waiver(), position = "bottom") {

  sc <- discrete_scale(c("x", "xmin", "xmax", "xend", "ysidex"), "position_d",
                       identity, ..., expand = expand, guide = guide, position = position,
                       super = ScaleDiscretePosition)
  sc$range_c <- continuous_range()
  structure(sc,
            class = c("ggside_scale", class(sc)))

}
new_mapped_discrete <- function(x=double()){
  vctrs::vec_assert(x, double())
  obj <- vctrs::new_vctr(x, class = "ggplot2_mapped_discrete")
  class(obj) <- c(class(obj), "numeric")
  obj
}

#' @export
vec_ptype2.logical.ggplot2_mapped_discrete <- function(x, y, ...) if(length(y)==0) new_mapped_discrete() else vctrs::stop_incompatible_type(x,y, details = "something went wrong")
#' @export
vec_ptype2.ggplot2_mapped_discrete.logical <- function(x, y, ...) if(length(x)==0) new_mapped_discrete() else vctrs::stop_incompatible_type(x,y, details = "something went wrong")


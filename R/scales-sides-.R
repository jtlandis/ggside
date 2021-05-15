
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
#'
NULL

#' @rdname ggside-scales-continuous
#' @usage NULL
#' @export
scale_xsidey_continuous <- function(name = waiver(), breaks = waiver(), minor_breaks = waiver(),
                                    n.breaks = NULL, labels = waiver(), limits = NULL, expand = waiver(),
                                    oob = scales::censor, na.value = NA_real_, trans = "identity", guide = waiver(),
                                    position = "left", sec.axis = waiver()){

  sc <- continuous_scale(c("y", "ymin", "ymax", "yend", "yintercept",
                           "ymin_final", "ymax_final", "lower", "middle", "upper",
                           "y0","xsidey"), "position_c", identity, name = name, breaks = breaks,
                         n.breaks = n.breaks, minor_breaks = minor_breaks, labels = labels,
                         limits = limits, expand = expand, oob = oob, na.value = na.value,
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

  sc <- continuous_scale(c("x", "xmin", "xmax", "xend", "xintercept",
                           "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper",
                           "x0", "ysidex"), "position_c", identity, name = name, breaks = breaks,
                         n.breaks = n.breaks, minor_breaks = minor_breaks, labels = labels,
                         limits = limits, expand = expand, oob = oob, na.value = na.value,
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
#'
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

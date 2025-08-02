### INCLUDE BEGIN
#' @include utils-calls.R
NULL
### INCLUDE END

# is_active <- function(f) {
#   class(f) <- "is_active"
#   f
# }
#
# extract_active <- function(x, name) {
#   res <- fetch_ggproto(x, name)
#   if (!is.function(res)) {
#     return(res)
#   }
#   method <- make_proto_method(x, res, name)
#   if (inherits(res, "is_active")) {
#     return(method())
#   }
#   method
# }
#
# replace_active <- function(x, name, value) {
#   res <- fetch_ggproto(x, name)
#   if (inherits(res, "is_active")) {
#     make_proto_method(x, res, name)(value)
#   } else {
#     NextMethod()
#   }
# }

# # #' @export
# #`$.ggside_scale` <- extract_active
#
# #' @export
# `[[.ggside_scale` <- extract_active
#
# #' @export
# `$<-.ggside_scale` <- replace_active
#
# #' @export
# `[[<-.ggside_scale` <- replace_active


# fetch_ggproto <- function (x, name) {
#   res <- NULL
#   val <- .subset2(x, name)
#   if (!is.null(val) || exists(name, envir = x, inherits = FALSE)) {
#     res <- val
#   }
#   else {
#     super <- .subset2(x, "super")
#     if (is.null(super)) {
#     }
#     else if (is.function(super)) {
#       res <- fetch_ggproto(super(), name)
#     }
#     else {
#       cli::cli_abort(c("{class(x)[[1]]} was built with an incompatible version of ggproto.",
#                        i = "Please reinstall the package that provides this extension."))
#     }
#   }
#   res
# }
#
# make_proto_method <- function (self, f, name) {
#   args <- formals(f)
#   has_self <- !is.null(args[["self"]]) || "self" %in% names(args)
#   assign(name, f, envir = environment())
#   args <- list(quote(...))
#   if (has_self) {
#     args$self <- quote(self)
#   }
#   fun <- inject(function(...) !!call2(name, !!!args))
#   class(fun) <- "ggproto_method"
#   fun
# }


mod_scale_map_method <- function(scale) {
  ggproto(
    NULL,
    scale,
    map = mod_ggproto_fun(scale$map) |>
      mod_fun_at(quote(if (length(x)==0) return(x)), 1)
  )
}

new_side_pos_scale <- function(scale, side) {
  side <- match.arg(side, choices = c("x","y"))
  # other <- switch(side, x = "y", y =)
  mod_scale_map_method(
    ggproto(
      "ggside_scale",
      scale,
      aesthetics = sprintf("%sside%s", side, scale$aesthetics)
    )
  )
}




#' @title Position scales for continuous data ggside scales
#'
#' @name ggside-scales-continuous
#'
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{scale_x_continuous}/\link[ggplot2]{scale_y_continuous}.
#' [scale_xsidey_continuous] enables better control on how the y-axis is rendered on the xside panel and
#' [scale_ysidex_continuous] enables better control on how the x-axis is rendered on the yside panel.
#'

#' @param ... Other arguments passed on to scale_(y|x)side(x|y)_continuous()
#' @inheritParams ggplot2::scale_x_continuous
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
#' @export
scale_xsidey_continuous <- function(name = waiver(), breaks = waiver(), minor_breaks = waiver(),
                                    n.breaks = NULL, labels = waiver(), limits = NULL, expand = waiver(),
                                    oob = scales::censor, na.value = NA_real_, transform = "identity", guide = waiver(),
                                    position = "left", sec.axis = waiver()){

  new_side_pos_scale(
    side = "x",
    scale_y_continuous(name = name, breaks = breaks, minor_breaks = minor_breaks,
                       n.breaks = n.breaks, labels = labels, limits = limits,
                       expand = expand, oob = oob, na.value = na.value,
                       transform = transform, guide = guide, position = position, sec.axis = sec.axis)
  )

}

#' @rdname ggside-scales-continuous
#' @export
scale_xsidey_log10 <- function(...) {
  scale_xsidey_continuous(..., transform = scales::log10_trans())
}

#' @rdname ggside-scales-continuous
#' @export
scale_xsidey_reverse <- function(...) {
  scale_xsidey_continuous(..., transform = scales::reverse_trans())
}

#' @rdname ggside-scales-continuous
#' @export
scale_xsidey_sqrt <- function(...) {
  scale_xsidey_continuous(..., transform = scales::sqrt_trans())
}


#' @rdname ggside-scales-continuous
#' @usage NULL
#' @export
scale_ysidex_continuous <- function(name = waiver(), breaks = waiver(), minor_breaks = waiver(),
                                    n.breaks = NULL, labels = waiver(), limits = NULL, expand = waiver(),
                                    oob = scales::censor, na.value = NA_real_, transform = "identity", guide = waiver(),
                                    position = "bottom", sec.axis = waiver()){
  new_side_pos_scale(
    side = "y",
    scale_x_continuous(name = name, breaks = breaks, minor_breaks = minor_breaks,
                       n.breaks = n.breaks, labels = labels, limits = limits,
                       expand = expand, oob = oob, na.value = na.value,
                       transform = transform, guide = guide, position = position, sec.axis = sec.axis)
  )

}


#' @rdname ggside-scales-continuous
#' @export
scale_ysidex_log10 <- function(...) {
  scale_ysidex_continuous(..., transform = scales::log10_trans())
}

#' @rdname ggside-scales-continuous
#' @export
scale_ysidex_reverse <- function(...) {
  scale_ysidex_continuous(..., transform = scales::reverse_trans())
}

#' @rdname ggside-scales-continuous
#' @export
scale_ysidex_sqrt <- function(...) {
  scale_ysidex_continuous(..., transform = scales::sqrt_trans())
}


#' @rdname ggside-scales-continuous
#' @export
scale_ysidex_log10 <- function(...) {
  scale_ysidex_continuous(..., transform = scales::log10_trans())
}

#' @rdname ggside-scales-continuous
#' @export
scale_ysidex_reverse <- function(...) {
  scale_ysidex_continuous(..., transform = scales::reverse_trans())
}

#' @rdname ggside-scales-continuous
#' @export
scale_ysidex_sqrt <- function(...) {
  scale_ysidex_continuous(..., transform = scales::sqrt_trans())
}


#' @title Position scales for discrete data ggside scales
#' @name ggside-scales-discrete
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{scale_x_discrete}/\link[ggplot2]{scale_y_discrete}.
#' [scale_xsidey_discrete] enables better control on how the y-axis is rendered on the xside panel and
#' [scale_ysidex_discrete] enables better control on how the x-axis is rendered on the yside panel.
#'
#' @inheritParams ggplot2::scale_x_discrete
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

  new_side_pos_scale(
    side = "x",
    scale_y_discrete(..., expand = expand, guide = guide, position = position)
  )

}

#' @rdname ggside-scales-discrete
#' @usage NULL
#' @export
scale_ysidex_discrete <- function(..., expand = waiver(),
                                  guide = waiver(), position = "bottom") {

  new_side_pos_scale(
    side = "y",
    scale_x_discrete(..., expand = expand, guide = guide, position = position)
  )

}


#' @title Position scales for binning continuous data ggside scales
#' @name ggside-scales-binned
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{scale_x_binned}/\link[ggplot2]{scale_y_binned}.
#' [scale_xsidey_binned] enables better control on how the y-axis is rendered on the xside panel and
#' [scale_ysidex_binned] enables better control on how the x-axis is rendered on the yside panel.
#'
#' @inheritParams ggplot2::binned_scale
#' @return ggside_scale object inheriting from ggplot2::ScaleBinnedPosition
#' @examples
#'
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   geom_point() + geom_xsidepoint(aes(y = Petal.Width, xcolour = Petal.Length)) +
#'   scale_xsidey_binned(n.breaks = 4) +
#'   scale_colour_steps(aesthetics ="xcolour", guide = guide_colorbar(available_aes = "xcolour")) +
#'   theme(ggside.panel.scale.x = .3)
#'
#' @export
scale_xsidey_binned <- function (name = waiver(), n.breaks = 10, nice.breaks = TRUE,
                                 breaks = waiver(), labels = waiver(), limits = NULL, expand = waiver(),
                                 oob = squish, na.value = NA_real_, right = TRUE, show.limits = FALSE,
                                 transform = "identity", guide = waiver(), position = "left")
{

  new_side_pos_scale(
    side = "x",
    scale_y_binned(name = name, n.breaks = n.breaks, nice.breaks = nice.breaks,
                   breaks = breaks, labels = labels, limits = limits, expand = expand,
                   oob = oob, na.value = na.value, right = right, show.limits = show.limits,
                   transform = transform, guide = guide, position = position)
  )
}

#' @rdname ggside-scales-binned
#' @export
scale_ysidex_binned <- function (name = waiver(), n.breaks = 10, nice.breaks = TRUE,
                                 breaks = waiver(), labels = waiver(), limits = NULL, expand = waiver(),
                                 oob = squish, na.value = NA_real_, right = TRUE, show.limits = FALSE,
                                 transform = "identity", guide = waiver(), position = "bottom")
{

  new_side_pos_scale(
    side = "y",
    scale_x_binned(name = name, n.breaks = n.breaks, nice.breaks = nice.breaks,
                   breaks = breaks, labels = labels, limits = limits, expand = expand,
                   oob = oob, na.value = na.value, right = right, show.limits = show.limits,
                   transform = transform, guide = guide, position = position)
  )
}

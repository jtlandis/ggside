### INCLUDE BEGIN
#' @include constructor-.R
NULL
### INCLUDE END

#' @title Side Reference lines
#' @description
#' The [xside] and [yside] variants of \link[ggplot2]{geom_abline},
#' \link[ggplot2]{geom_hline} and \link[ggplot2]{geom_vline} are
#' [geom_*abline], [geom_*hline], and [geom_*vline].
#'
#' @aliases geom_*abline
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param mapping Set of aesthetic mappings created by
#' \link[ggplot2:aes]{aes()}.
#' @param xintercept,yintercept,slope,intercept Parameters that control the
#'   position of the line specifically for the [xside] or [yside] variants.
#'   If these are set, `data`, `mapping` and `show.legend` are overridden.
#' @export
geom_xsideabline <- ggside_layer_function(fun = geom_abline, side = "x")

#' @rdname geom_xsideabline
#' @export
geom_ysideabline <- ggside_layer_function(fun = geom_abline, side = "y")

#' @rdname ggside-ggproto-geoms
#' @format NULL
#' @usage NULL
#' @export
GeomXsideabline <- ggside_geom("GeomXsideabline", GeomAbline, "x")

#' @rdname ggside-ggproto-geoms
#' @format NULL
#' @usage NULL
#' @export
GeomYsideabline <- ggside_geom("GeomYsideabline", GeomAbline, "y")

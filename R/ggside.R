#' @title ggside options
#' @rdname ggside-options
#' @description Set characteristics of side panels
#' @param x.pos x side panel can either take "top" or "bottom"
#' @param y.pos y side panel can either take "right" or "left"
#' @param scales Determines side panel's unaligned axis
#' scale. Inputs are similar to facet_* scales function. Default
#' is set to "fixed", but "free_x", "free_y" and "free" are
#' acceptable inputs. For example, xside panels are aligned to
#' the x axis of the main panel. Setting "free" or "free_y" will
#' cause all y scales of the x side Panels to be independent.
#' @param collapse Determines if side panels should be collapsed into
#' a single panel. Set "x" to collapse all x side panels, set "y" to
#' collapse all y side panels, set "all" to collapse both x and y
#' side panels.
#' @param draw_x_on,draw_y_on Determines where the axis is rendered.
#' For example:
#' By default, the bottom x-axis is rendered on the bottom most panel
#' per column. If set to "main", then the axis is rendered on the bottom
#' of the bottom most main panel. If set to "side", then the x-axis is rendered
#' on the bottom of the bottom most side panel(s). You may apply this logic
#' to all axis positions.
#' @param strip Determines if the strip should be rendered on the main plot or
#' on their default locations. Only has an effect on `facet_grid`.
#' @param respect_side_labels Valid arguments are "default", "x", "y",
#' "all", and "none" Indicates if panel spacing should respect the axis
#' labels. The default is to respect side panel labels except when xside
#' labels are on the same side as the yside panel. Note: setting this
#' parameter to "x" is to "respect the labels of the xside panel" and
#' consequently the yside labels, if present, are not respected.
#'
#' @seealso
#' For more information regarding the ggside api: see [xside] or [yside]
#' @return a object of class 'ggside_options' or to be added to a ggplot
#' @export
ggside <- function(x.pos = NULL, y.pos = NULL, scales = NULL, collapse = NULL,
                   draw_x_on = NULL,
                   draw_y_on = NULL,
                   strip = NULL,
                   respect_side_labels = NULL){

  x.pos <- resolve_arg(x.pos, c("top", "bottom"))
  y.pos <- resolve_arg(y.pos, c("right", "left"))
  draw_x_on <- resolve_arg(draw_x_on, c("default","main","side"))
  draw_y_on <- resolve_arg(draw_y_on, c("default","main","side"))
  strip <- resolve_arg(strip, c("default", "main"))
  collapse <- resolve_arg(collapse, c("all", "x", "y"))
  respect_side_labels <- resolve_arg(respect_side_labels, c("default","x","y", "all", "none", "independent"))


  ggproto(
    "ggside_options",
    x.pos = x.pos,
    y.pos = y.pos,
    scales = scales,
    collapse = collapse,
    xsidey = NULL,
    ysidex = NULL,
    draw_x_on = draw_x_on,
    draw_y_on = draw_y_on,
    strip = strip,
    sides_used = NULL,
    respect_side_labels = respect_side_labels
  )
}

resolve_arg <- function(arg, opt, several.ok = FALSE) {
  arg_sym <- substitute(arg)
  if (!is.null(arg)) {
    arg <- opt[opt %in% arg]
    len <- length(arg)
    opt_len <- length(opt)
    if (len==0)
      cli::cli_abort("valid {cli::qty(opt_len)} option{?s} for argument {.arg {arg_sym}} {?is/are} {.val {opt}}",
                     call = parent.frame())
    if (length(arg)>1 && !several.ok)
      cli::cli_abort("you specified {length(arg)} arguments to {.arg {arg_sym}}, but only one of {.or {.val {opt}}} are allowed",
                     call = parent.frame())
  }
  arg
}

new_ggside <- function(x.pos = "top", y.pos = "right", scales = "fixed", collapse = NULL,
                   draw_x_on = c("default","main","side"),
                   draw_y_on = c("default","main","side"),
                   strip = c("default", "main"),
                   respect_side_labels = FALSE){
  draw_x_on <- match.arg(draw_x_on, c("default","main","side"))
  draw_y_on <- match.arg(draw_y_on, c("default","main","side"))
  strip <- match.arg(strip, c("default", "main"))
  if (!is.null(collapse)) {
    collapse <- match.arg(collapse, c("all", "x","y"))
  }


  ggproto(
    "ggside_options",
    x.pos = x.pos,
    y.pos = y.pos,
    scales = scales,
    collapse = collapse,
    xsidey = NULL,
    ysidex = NULL,
    draw_x_on = draw_x_on,
    draw_y_on = draw_y_on,
    strip = strip,
    sides_used = NULL,
    respect_side_labels = respect_side_labels
  )
}



#' @title Check ggside objects
#' @param x Object to test
#' @return A logical value
#' @export
is.ggside <- function(x) inherits(x, "ggside")

#' @rdname is.ggside
#' @export
is.ggside_layer <- function(x) inherits(x, "ggside_layer")

#' @rdname is.ggside
#' @export
is.ggside_options <- function(x) inherits(x, "ggside_options")

#' @rdname is.ggside
#' @export
is.ggside_scale <- function(x) inherits(x, "ggside_scale")

#' @name xside
#' @title The xside geometries
#' @rdname xside
#' @description `xside` refers to the api of ggside. Any `geom_` with
#' `xside` will plot its respective geometry along the x-axis
#' per facet panel. By default the xside panel will plot above the main
#' panel. This xside panel will always share the same scale as it's main
#' panel, but is expected to have a separate y-axis scaling.
#'
#' @section New Aesthetics:
#'
#' All `xside` Geometries have `xfill`, `xcolour`/`xcolor` available for
#' aesthetic mappings. These mappings behave exactly like the default
#' counterparts except that they are considered separate scales. All
#' `xside` geometries will use `xfill` over `fill`, but will default
#' to `fill` if `xfill` is not provided. The same goes for `xcolour` in
#' respects to `colour`. This comes in handy if you wish to map both `fill`
#' to one geometry as continuous, you can still map `xfill` for a separate
#' `xside` geometry without conflicts. See more information in
#' `vignette("ggside")`.
#'
#' @section Exported Geometries:
#'
#' The following are the `xside` variants of the \link{ggplot2} Geometries
#'
#' \itemize{
#' \item [geom_xsidebar]
#' \item [geom_xsideboxplot]
#' \item [geom_xsidecol]
#' \item [geom_xsidedensity]
#' \item [geom_xsidefreqpoly]
#' \item [geom_xsidehistogram]
#' \item [geom_xsideline]
#' \item [geom_xsidepath]
#' \item [geom_xsidepoint]
#' \item [geom_xsidetext]
#' \item [geom_xsidetile]
#' \item [geom_xsideviolin]
#' }
#' @return geom_xside* return a XLayer object to be added to a ggplot
#' @seealso [yside]
NULL

#' @title The yside geometries
#' @name yside
#' @rdname yside
#' @description `yside` refers to the api of ggside. Any `geom_` with
#' `yside` will plot its respective geometry along the y-axis per
#' facet panel. The yside panel will plot to the right of the main
#' panel by default. This yside panel will always share the same scale
#' as it's main panel, but is expected to have a separate x-axis scaling.
#'
#' @section New Aesthetics:
#'
#' All `yside` Geometries have `yfill`, `ycolour`/`ycolor` available for
#' aesthetic mappings. These mappings behave exactly like the default
#' counterparts except that they are considered separate scales. All
#' `yside` geometries will use `yfill` over `fill`, but will default
#' to `fill` if `yfill` is not provided. The same goes for `ycolour` in
#' respects to `colour`. This comes in handy if you wish to map both `fill`
#' to one geometry as continuous, you can still map `yfill` for a separate
#' `yside` geometry without conflicts. See more information in
#' `vignette("ggside")`.
#'
#' #' @section Exported Geometries:
#'
#' The following are the `yside` variants of the \link{ggplot2} Geometries
#'
#' \itemize{
#' \item [geom_ysidebar]
#' \item [geom_ysideboxplot]
#' \item [geom_ysidecol]
#' \item [geom_ysidedensity]
#' \item [geom_ysidefreqpoly]
#' \item [geom_ysidehistogram]
#' \item [geom_ysideline]
#' \item [geom_ysidepath]
#' \item [geom_ysidepoint]
#' \item [geom_ysidetext]
#' \item [geom_ysidetile]
#' \item [geom_ysideviolin]
#' }
#' @return geom_yside* return a YLayer object to be added to a ggplot
#' @seealso [xside]
NULL





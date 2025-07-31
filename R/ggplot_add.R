### INCLUDE BEGIN
#' @include add_gg.R
#' @include all_classes.r
#' @include ggside.R
#' @include plot-construction.R
NULL
### INCLUDE END


S7::method(`+`, list(class_ggside, S7::class_any)) <- function(e1, e2) {
  p <- S7::super(e1, class_ggplot) + e2
  p <- clone_ggside_plot(p)
  validate_ggside(e2, plot = p)
}


clone_ggside_plot <- function(plot) {
  # does not clone scales, we should assume they
  # have been already
  plot@ggside_opt <- clone_ggside(plot[["ggside_opt"]])
  plot@facet$params$ggside <- plot@ggside_opt
  plot
}

clone_ggside <- function(ggside) {
  new_ggside <- ggside(
    x.pos = ggside$x.pos,
    y.pos = ggside$y.pos,
    scales = ggside$scales,
    collapse = ggside$collapse,
    draw_x_on = ggside$draw_x_on,
    draw_y_on = ggside$draw_y_on,
    strip = ggside$strip,
    respect_side_labels = ggside$respect_side_labels
  )
  if (!is.null(ggside$xsidey)) {
    new_ggside$xsidey <- ggside$xsidey$clone()
  }
  if (!is.null(ggside$ysidex)) {
    new_ggside$ysidex <- ggside$ysidex$clone()
  }
  if (!is.null(ggside$sides_used)) {
    new_ggside$sides_used <- ggside$sides_used
  }
  new_ggside
}



#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggside_layer <- function(object, plot, object_name) {
  p <- NextMethod("ggplot_add")
  if (S7::S7_inherits(p, class_ggside)) {
    p <- clone_ggside_plot(p)
    as_ggside(p)
  } else {
    class_ggside(ggplot = p, ggside_opt = ggside())
  }
}

#' @export
ggplot_add.ggside_options <- function(object, plot, object_name) {
  as_ggside(plot, ggside = object)
}


#' @export
ggplot_add.ggside_scale <- function(object, plot, object_name) {
  ggside_opt <- if (is_ggside(plot)) {
    plot@ggside_opt
  } else {
    ggside()
  }
  # save scale in appropriate place
  ggside_opt[[intersect(c("xsidey", "ysidex"), object$aesthetics)]] <- object
  new_scale <- object$clone()
  new_scale$guide <- waiver()
  plot$scales$add(new_scale)
  as_ggside(plot, ggside = ggside_opt)
}

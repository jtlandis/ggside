### INCLUDE BEGIN
#' @include ggside.R
#' @include plot-construction.R
NULL
### INCLUDE END

clone_ggside_plot <- function(plot) {
  #does not clone scales, we should assume they
  # have been already
  plot$ggside <- clone_ggside(plot$ggside)
  plot$facet$params$ggside <- plot$ggside
  plot

}

clone_ggside <- function(ggside) {
  new_ggside <- ggside(x.pos = ggside$x.pos,
                       y.pos = ggside$y.pos,
                       scales = ggside$scales,
                       collapse = ggside$collapse,
                       draw_x_on = ggside$draw_x_on,
                       draw_y_on = ggside$draw_y_on,
                       strip = ggside$strip,
                       respect_side_labels = ggside$respect_side_labels)
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
ggplot_add.ggside_layer <- function(object, plot, object_name){

  p <- NextMethod("ggplot_add")
  p <- clone_ggside_plot(p)
  as_ggside(p)
}

#' @export
ggplot_add.ggside_options <- function(object, plot, object_name){
  plot <- clone_ggside_plot(plot)
  as_ggside(plot, object)
}


#' @export
ggplot_add.ggside_scale <- function(object, plot, object_name){
  plot <- clone_ggside_plot(plot)
  plot$ggside[[intersect(c("xsidey","ysidex"), object$aesthetics)]] <- object #save scale in appropriate place
  new_scale <- object$clone()
  new_scale$guide <- waiver()
  plot$scales$add(new_scale)
  as_ggside(plot)
}




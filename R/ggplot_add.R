
clone_ggside_plot <- function(plot) {
  #does not clone scales, we should assume they
  # have been already
  plot$ggside <- clone_ggside(plot$ggside)
  plot$facet$params$ggside <- plot$ggside
  plot

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
  plot$scales$add(object)
  as_ggside(plot)
}


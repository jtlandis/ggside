

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggside_layer <- function(object, plot, object_name){

  p <- NextMethod("ggplot_add")
  p$ggside <- clone_ggside(plot$ggside)
  as_ggside(p)
}

#' @export
ggplot_add.ggside_options <- function(object, plot, object_name){
  plot$ggside <- clone_ggside(plot$ggside)
  as_ggside(plot, object)
}


#' @export
ggplot_add.ggside_scale <- function(object, plot, object_name){
  plot$ggside <- clone_ggside(plot$ggside)
  plot$ggside[[intersect(c("xsidey","ysidex"), object$aesthetics)]] <- object #save scale in appropriate place
  plot$scales$add(object)
  as_ggside(plot)
}



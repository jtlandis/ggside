

#' @export
ggplot_add.ggside <- function(object, plot, object_name){
  plot <- make_ggside(plot)
  plot + object$layer
}

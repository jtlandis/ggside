
#' @export
ggplot_build.ggside <- function(plot) {

  plot$scales <- make_new_scale_list(plot$scales, plot$ggside)
  plot$facet <- as_ggsideFacet(plot$facet, plot$ggside)
  plot$coordinates <- as_ggsideCoord(plot$coordinates)
  NextMethod("ggplot_build")

}

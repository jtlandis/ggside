
#' @export
ggplot_build.ggside <- function(plot) {

  plot$scales <- ggside_scales(plot$scales)
  plot$facet <- as_ggsideFacet(plot$facet, plot$ggside)
  plot$coordinates <- as_ggsideCoord(plot$coordinates)

  built <- NextMethod("ggplot_build")
  standardise_panel_params(built)

}

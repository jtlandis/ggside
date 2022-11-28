
#' @export
ggplot_build.ggside <- function(plot) {

  plot$scales <- ggside_scales(plot$scales)
  plot$facet <- ggside_facet(plot$facet, plot$ggside)
  plot$coordinates <- as_ggsideCoord(plot$coordinates)

  built <- NextMethod("ggplot_build")
  standardise_panel_params(built)

}

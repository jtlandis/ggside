
clone_ggside <- function(ggside) {
  new_ggside <- ggproto(NULL, ggside)
  if (!is.null(ggside$xsidey)) {
    new_ggside$xsidey <- ggside$xsidey$clone()
  }
  if (!is.null(ggside$ysidex)) {
    new_ggside$ysidex <- ggside$ysidex$clone()
  }
  new_ggside
}

ggside_guides <- function(guide) {
  ggproto(
    NULL,
    guide,
    build = mod_ggproto_fun(guide$build) |> browse_fun()
  )
}

#' @export
ggplot_build.ggside <- function(plot) {

  plot$ggside <- clone_ggside(plot$ggside)
  # plot$scales <- ggside_scales(plot$scales, plot$ggside)
  plot$facet <- ggside_facet(plot$facet, plot$ggside)
  plot$coordinates <- as_ggsideCoord(plot$coordinates)
  plot$layout <- new_side_layout(plot$layout)

  built <- NextMethod("ggplot_build")
  standardise_panel_params(built)

}

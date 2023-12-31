
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

# ggside_guides <- function(guide) {
#   ggproto(
#     NULL,
#     guide,
#     build = mod_ggproto_fun(guide$build) |> browse_fun()
#   )
# }

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

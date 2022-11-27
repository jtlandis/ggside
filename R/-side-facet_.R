

#' @export
ggside_facet <- function(facet, ggside) UseMethod("ggside_facet", facet)

ggside_facet.default <- function(facet, ggside) {
  abort(
    sprintf("No %s() method for object of class <%s>", "ggside_facet", class(facet)[1]),
  )
}


ggside_facet.FacetNull <- function(facet, ggside) {
  force(ggside)

  ggproto(
    "ggsideFacetNull",
    facet,
    ggside = ggside,
    draw_panels = sideFacetNull_draw_panels,
    compute_layout = ggside_compute_layout(facet),
    init_scales = ggside_init_scales(facet),

  )

}


ggside_compute_layout <- function(facet) {
  force(facet)
  function(data, params){
    layout <- facet$compute_layout(data, params)
    layout <- check_scales_collapse(layout, params)
    layout <- sidePanelLayout(layout, ggside = params$ggside)
    layout
  }
}


ggside_init_scales <- function(facet) {
  force(facet)
  function(layout, x_scale = NULL, y_scale = NULL, params){
    scales <- facet$init_scales(layout, x_scale, y_scale, params)
    if (!is.null(x_scale)&& !is.null(params$ggside$ysidex)){
      side_indx <-  unique(layout[layout$PANEL_TYPE=="y",]$SCALE_X)
      scales$x[side_indx] <- lapply(side_indx, function(i) params$ggside$ysidex$clone())

    }
    if (!is.null(y_scale)&& !is.null(params$ggside$xsidey)){
      side_indx <-  unique(layout[layout$PANEL_TYPE=="x",]$SCALE_Y)
      scales$y[side_indx] <- lapply(side_indx, function(i) params$ggside$xsidey$clone())

    }
    scales
  }
}



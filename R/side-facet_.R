
#'@rdname ggside-ggproto-facets
#'@description
#' S3 class that converts old Facet into one that
#' is compatible with ggside. Can also update
#' ggside on the object. Typically, the new ggproto
#' will inherit from the object being replaced.
#' @param facet Facet ggproto Object to replace
#' @param ggside ggside object to update
#' @export
ggside_facet <- function(facet, ggside) UseMethod("ggside_facet", facet)

ggside_facet.default <- function(facet, ggside = ggside()) {
  abort(
    sprintf("No %s() method for object of class <%s>", "ggside_facet", class(facet)[1]),
  )
}


ggside_facet.FacetNull <- function(facet, ggside = ggside()) {

  new_facet <- new_ggside_facet(facet, ggside)
  ggproto(
    "FacetSideNull",
    new_facet,
    params = c(new_facet$params, ggside = ggside),
    draw_panels = sideFacetNull_draw_panels,
    map_data = sideFacetNull_map_data
  )

}

ggside_facet.FacetGrid <- function(facet, ggside = ggside()) {

  new_facet <- new_ggside_facet(facet, ggside)
  ggproto(
    "FacetSideGrid",
    new_facet,
    draw_panels = sideFacetGrid_draw_panels,
    map_data = sideFacetGrid_map_data
  )
}

ggside_facet.FacetWrap <- function(facet, ggside = ggside()) {

  new_facet <- new_ggside_facet(facet, ggside)
  ggproto(
    "FacetSideWrap",
    new_facet,
    draw_panels = sideFacetWrap_draw_panels,
    map_data = sideFacetWrap_map_data
  )
}

new_ggside_facet <- function(facet, ggside) {
  force(facet)
  params <- facet$params
  params[["ggside"]] <- ggside
  ggproto(
    "ggsideFacet",
    facet,
    params = params,
    compute_layout = ggside_compute_layout(facet),
    init_scales = ggside_init_scales(facet),
    train_scales = ggside_train_scales(facet),
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

ggside_train_scales <- function(facet) {
  function(x_scales, y_scales, layout, data, params) {
    local_union_scale_aes(x_scales)
    local_union_scale_aes(y_scales)
    facet$train_scales(x_scales, y_scales, layout, data, params)
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



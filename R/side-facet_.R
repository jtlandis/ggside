
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
  force(facet)
  function(x_scales, y_scales, layout, data, params) {
    if (!is.null(x_scales) && !is.null(params$ggside$ysidex) &&
        (!any(vapply(x_scales, function(s) "ysidex" %in% s$aesthetics, logical(1))))) {
      side_indx <- unique(layout[layout$PANEL_TYPE=="y",]$SCALE_X)
      side_x <- lapply(side_indx, function(i) params$ggside$ysidex$clone())
      for (i in seq_along(side_indx)) {
        j <- side_indx[i]
        vec_poke_n(x_scales, j, side_x, i, n = 1L)
      }
      first_scale_x <- x_scales[[1]]
      side_aes <- unique(unlist(lapply(side_x, `[[`, "aesthetics")))
      first_scale_x$aesthetics <-c(first_scale_x$aesthetics, side_aes)
    }

    if (!is.null(y_scales) && !is.null(params$ggside$xsidey) &&
        (!any(vapply(y_scales, function(s) "xsidey" %in% s$aesthetics, logical(1))))) {
      side_indx <- unique(layout[layout$PANEL_TYPE=="x",]$SCALE_Y)
      side_y <- lapply(side_indx, function(i) params$ggside$xsidey$clone())
      for (i in seq_along(side_indx)) {
        j <- side_indx[i]
        vec_poke_n(y_scales, j, side_y, i, n = 1L)
      }
      first_scale_y <- y_scales[[1]]
      side_aes <- unique(unlist(lapply(side_y, `[[`, "aesthetics")))
      first_scale_y$aesthetics <-c(first_scale_y$aesthetics, side_aes)
    }
    facet$train_scales(x_scales, y_scales, layout, data, params)
  }
}




### INCLUDE BEGIN
#' @include side-facet_utils.R
#' @include utils-.R
#' @include utils-calls.R
#' @include utils-ggproto.R
NULL
### INCLUDE END

#'@rdname ggside-ggproto-facets
#'@description
#' S3 class that converts old Facet into one that
#' is compatible with ggside. Can also update
#' ggside on the object. Typically, the new ggproto
#' will inherit from the object being replaced.
#' @param facet Facet ggproto Object to replace
#' @param ggside ggside object to update
#' @export
ggside_facet <-
  function(facet, ggside)
    UseMethod("ggside_facet", facet)

ggside_facet.default <- function(facet, ggside = ggside()) {
  abort(sprintf(
    "No %s() method for object of class <%s>",
    "ggside_facet",
    class(facet)[1]
  ),)
}

ggside_facet.ggsideFacet <- function(facet, ggside = ggside()) {
  param <- facet$params
  param$ggside <- ggside
  check_facet(ggproto(NULL, facet, params = param))
}

ggside_facet.FacetNull <- function(facet, ggside = ggside()) {
  new_facet <- new_ggside_facet(facet, ggside)
  check_facet(
    ggproto(
      "FacetSideNull",
      new_facet,
      draw_panels = sideFacetNull_draw_panels,
      map_data = sideFacetNull_map_data
    )
  )

}

ggside_facet.FacetGrid <- function(facet, ggside = ggside()) {
  new_facet <- new_ggside_facet(facet, ggside)
  check_facet(
    ggproto(
      "FacetSideGrid",
      new_facet,
      draw_panels = sideFacetGrid_draw_panels,
      map_data = sideFacetGrid_map_data
    )
  )

}

ggside_facet.FacetWrap <- function(facet, ggside = ggside()) {
  new_facet <- new_ggside_facet(facet, ggside)
  check_facet(
    ggproto(
      "FacetSideWrap",
      new_facet,
      draw_panels = sideFacetWrap_draw_panels,
      map_data = sideFacetWrap_map_data
    )
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
    train_scales = mod_ggproto_fun(
      facet$train_scales,
      x_scales[[1]]$aesthetics ~ unique(unlist(lapply(
        x_scales, `[[`, "aesthetics"
      ))),
      y_scales[[1]]$aesthetics ~ unique(unlist(lapply(
        y_scales, `[[`, "aesthetics"
      )))
    ),
    finish_data = new_ggproto_fun(facet$finish_data,
                                  {
                                    if ("PANEL_TYPE" %in% names(data) &&
                                        all(data$PANEL_TYPE != "main")) {
                                      data <- use_side_aes(data, unique(data$PANEL_TYPE))
                                    }
                                    call_parent_method
                                  })
  )
}


ggside_compute_layout <- function(facet) {
  force(facet)
  function(data, params) {
    layout <- facet$compute_layout(data, params)
    layout <- check_scales_collapse(layout, params)
    layout <- sidePanelLayout(layout, ggside = params$ggside)
    layout
  }
}

check_facet <- function(facet) {
  fp <- facet$params
  #this proto should be the same as the one on the plot
  ggside <- facet$ggside
  col <- ggside$collapse
  if (!is.null(fp$free) &&
      !is.null(col) &&
      inherits(facet, "FacetWrap") &&
      any(.lgl <- vapply(fp$free, identity, logical(1)))) {
    # if ggside collapse all - but scales is free - prioritize the scale and dont
    # collapse
    # i.e. facet_wrap(..., scales='free_y') + ggside(collapse="y") --> warning
    # main plots may have different y scales and thus we cannot collapse y.
    s <- sum(c(1, 2) * .lgl)
    new_col <- switch(s,
                      free_x = {
                        .f <- "free_x"
                        switch(col,
                               all = "y",
                               x = NULL,
                               col)
                      },
                      free_y = {
                        .f <- "free_y"
                        switch(col,
                               all = "x",
                               y = NULL,
                               col)
                      },
                      free = {
                        .f <- "free"
                        NULL
                      })

    warning(
      glue(
        "Plot's Facet parameter `scales = \"{.f}\"` is ",
        "incompatible with `ggside(..., collapse = \"{col}\")`.",
        " Setting collapse to ",
        if (is.null(new_col))
          'NULL'
        else
          glue('"{new_col}"')
      ),
      call. = F
    )
    ggside$collapse <- new_col

  }
  invisible(facet)
}

# ggside_train_scales <- function(facet) {
# force(facet)
# function(x_scales, y_scales, layout, data, params) {
# # browser()
# if (!is.null(x_scales) && !is.null(params$ggside$ysidex) &&
# (!any(vapply(x_scales, function(s) "ysidex" %in% s$aesthetics, logical(1))))) {
# side_indx <- unique(layout[layout$PANEL_TYPE=="y",]$SCALE_X)
# side_x <- lapply(side_indx, function(i) params$ggside$ysidex$clone())
# for (i in seq_along(side_indx)) {
# j <- side_indx[i]
# vec_poke_n(x_scales, j, side_x, i, n = 1L)
# }
# # first_scale_x <- x_scales[[1]]
# # side_aes <- unique(unlist(lapply(side_x, `[[`, "aesthetics")))
# # first_scale_x$aesthetics <-c(first_scale_x$aesthetics, side_aes)
# }
#
# if (!is.null(y_scales) && !is.null(params$ggside$xsidey) &&
# (!any(vapply(y_scales, function(s) "xsidey" %in% s$aesthetics, logical(1))))) {
# side_indx <- unique(layout[layout$PANEL_TYPE=="x",]$SCALE_Y)
# side_y <- lapply(side_indx, function(i) params$ggside$xsidey$clone())
# for (i in seq_along(side_indx)) {
# j <- side_indx[i]
# vec_poke_n(y_scales, j, side_y, i, n = 1L)
# }
# # first_scale_y <- y_scales[[1]]
# # side_aes <- unique(unlist(lapply(side_y, `[[`, "aesthetics")))
# # first_scale_y$aesthetics <-c(first_scale_y$aesthetics, side_aes)
# }
# facet$train_scales(x_scales, y_scales, layout, data, params)
# }
# }

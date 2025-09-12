### INCLUDE BEGIN
#' @include utils-side-facet.R
#' @include ggside.R
#' @include utils-.R
#' @include utils-calls.R
#' @include utils-ggproto.R
NULL
### INCLUDE END

#' @rdname ggside-ggproto-facets
#' @description
#' S3 class that converts old Facet into one that
#' is compatible with ggside. Can also update
#' ggside on the object. Typically, the new ggproto
#' will inherit from the object being replaced.
#' @param facet Facet ggproto Object to replace
#' @param ggside ggside object to update
#' @export
ggside_facet <-
  function(facet, ggside) {
    UseMethod("ggside_facet", facet)
  }

#' @exportS3Method ggside::ggside_facet
ggside_facet.default <- function(facet, ggside = ggside()) {
  abort(sprintf(
    "No %s() method for object of class <%s>",
    "ggside_facet",
    class(facet)[1]
  ), )
}

find_super_facet <- function(facet, class = "ggsideFacet") {
  while (class(facet)[1] != class) {
    facet <- facet$super()
  }

  while (class(facet)[1] == class) {
    candidate <- facet
    facet <- facet$super()
  }
  candidate
}

#' @exportS3Method ggside::ggside_facet
ggside_facet.ggsideFacet <- function(facet, ggside = ggside()) {
  facet_dispatch <- find_super_facet(facet, "ggsideFacet")$super()
  ggside_facet(
    facet_dispatch,
    ggside = ggside
  )
}

#' @exportS3Method ggside::ggside_facet
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

#' @exportS3Method ggside::ggside_facet
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

#' @exportS3Method ggside::ggside_facet
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
    finish_data = new_ggproto_fun(facet$finish_data, {
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
  # this proto should be the same as the one on the plot
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
          col
        )
      },
      free_y = {
        .f <- "free_y"
        switch(col,
          all = "x",
          y = NULL,
          col
        )
      },
      free = {
        .f <- "free"
        NULL
      }
    )

    warning(
      glue(
        "Plot's Facet parameter `scales = \"{.f}\"` is ",
        "incompatible with `ggside(..., collapse = \"{col}\")`.",
        " Setting collapse to ",
        if (is.null(new_col)) {
          "NULL"
        } else {
          glue('"{new_col}"')
        }
      ),
      call. = F
    )
    ggside$collapse <- new_col
  }
  invisible(facet)
}

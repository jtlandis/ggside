

new_ggside_layer <- function(side = c("x","y"), geom = NULL, stat = NULL, data = NULL, mapping = NULL,
                             position = NULL, params = list(), inherit.aes = TRUE, check.aes = TRUE,
                             check.param = TRUE, show.legend = NA, key_glyph = NULL) {

  side <- match.arg(side, c("x","y"))
  mapping <- force_panel_type_mapping(mapping, side)
  names(mapping) <- rename_side(names(mapping), side)
  other <- switch(side, x = "y", y = "x")
  `_class` <- switch(side, x = "XLayer", y = "YLayer")
  layer <- layer(geom = geom, stat = stat,
                 data = data, mapping = mapping,
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = params,
                 check.aes = check.aes,
                 check.param = check.param,
                 key_glyph = key_glyph)

  parent_layer <- ggproto(
    `_class`,
    layer,
    .side = side,
    .other = other
  )
  ggproto(
    "ggside_layer",
    parent_layer,
    setup_layer = function(self, data, plot) {
      data <- ggproto_parent(parent_layer, self)$setup_layer(data, plot)
      data[["PANEL_TYPE"]] <- self$.side
      data
    },
    compute_aesthetics = function(self, data, plot) {
      data <- ggproto_parent(parent_layer, self)$compute_aesthetics(data, plot)
      data[, setdiff(names(data), self$.other), drop = FALSE]
    },
    compute_statistic = function(self, data, layout) {
      data <- self$geom$.data_unmapper(data)
      data <- ggproto_parent(parent_layer, self)$compute_statistic(data, layout)
      self$geom$.data_mapper(data)
    },
    compute_position = function(self, data, layout) {
      data <- self$geom$.data_unmapper(data)
      data <- ggproto_parent(layer, self)$compute_position(data, layout)
      self$geom$.data_mapper(data)
    }
  )


}

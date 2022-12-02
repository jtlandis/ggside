
#' @name ggside_layer
#' @title New ggside layer
#' @description utility function to make a ggside layer compatible with `ggside` internals
#' @inheritParams ggplot2::layer
#' @export
ggside_layer <- function(geom = NULL, stat = NULL, data = NULL, mapping = NULL,
                             position = NULL, params = list(), inherit.aes = TRUE, check.aes = TRUE,
                             check.param = TRUE, show.legend = NA, key_glyph = NULL) {


  side <- layer$geom$.side
  if (! side %in% c("x", "y")) {
    stop("A ggside Layer must have a ggside Geom")
  }
  mapping <- force_panel_type_mapping(mapping, side)
  names(mapping) <- rename_side(names(mapping), side)
  layer <- ggplot2::layer(geom = geom, stat = stat,
                 data = data, mapping = mapping,
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = params,
                 check.aes = check.aes,
                 check.param = check.param,
                 key_glyph = key_glyph)

  new_ggside_layer(layer = layer, side = side)

}

new_ggside_layer <- function(layer, side) {

  other <- switch(side, x = "y", y = "x")
  `_class` <- switch(side, x = "XLayer", y = "YLayer")
  parent_layer <- ggproto(
    `_class`,
    layer,
    mapping = mapping,
    .side = side,
    .other = other
  )
  ggproto(
    "ggside_layer",
    parent_layer,
    setup_layer = function(self, data, plot) {
      names(plot$mapping) <- rename_side(names(plot$mapping), self$.side)
      plot$mapping <- drop_plot_aes(plot$mapping, self$mapping, self$.side)
      data <- ggproto_parent(parent_layer, self)$setup_layer(data, plot)
      data[["PANEL_TYPE"]] <- self$.side
      data
    },
    compute_aesthetics = function(self, data, plot) {
      parent <- ggproto_parent(parent_layer, self)
      data <- parent$compute_aesthetics(data, plot)
      aes_to_drop <- self$.other
      if (all(paste0(c(self$.side, ""), "colour") %in% names(data))) {
        aes_to_drop <- c(aes_to_drop, "colour")
      }
      if (all(paste0(c(self$.side, ""), "fill") %in% names(data))) {
        aes_to_drop <- c(aes_to_drop, "fill")
      }
      data[, setdiff(names(data), aes_to_drop), drop = FALSE]
    },
    compute_statistic = function(self, data, layout) {
      data <- self$geom$.data_unmapper(data)
      data <- ggproto_parent(parent_layer, self)$compute_statistic(data, layout)
      self$geom$.data_mapper(data)
    },
    map_statistic = function(self, data, plot) {
      old_nms <- names(self$stat$default_aes)
      names(self$stat$default_aes) <- rename_side(names(self$stat$default_aes), self$.side)
      parent <- ggproto_parent(parent_layer, self)
      data <- parent$map_statistic(data, plot)
      names(self$stat$default_aes) <- old_nms
      data
    },
    compute_position = function(self, data, layout) {
      data <- self$geom$.data_unmapper(data)
      data <- ggproto_parent(layer, self)$compute_position(data, layout)
      self$geom$.data_mapper(data)
    }
  )
}


drop_plot_aes <- function(plot_map, layer_map, side) {
  #if layer mapping fill/colour variant exists
  #remove it
  p_nms <- names(plot_map)
  l_nms <- names(layer_map)
  if (paste0(side, "colour") %in% l_nms && any(to_drop <- p_nms %in% "colour"))
    plot_map <- plot_map[!to_drop]

  if (paste0(side, "fill") %in% l_nms && any(to_drop <- p_nms %in% "fill"))
    plot_map <- plot_map[!to_drop]

  plot_map
}

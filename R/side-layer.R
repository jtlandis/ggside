### INCLUDE BEGIN
#' @include utils-ggplot2-reimpl-.R
#' @include constructor-.R
#' @include utils-.R
#' @include utils-constructors.R
#' @include utils-ggproto.R
NULL
### INCLUDE END

#' @name ggside_layer
#' @title New ggside layer
#' @description utility function to make a ggside layer compatible with
#' `ggside` internals
#' @inheritParams ggplot2::layer
#' @export
ggside_layer <-
  function(geom = NULL,
           stat = NULL,
           data = NULL,
           mapping = NULL,
           position = NULL,
           params = list(),
           inherit.aes = TRUE,
           check.aes = TRUE,
           check.param = TRUE,
           show.legend = NA,
           key_glyph = NULL,
           side = NULL) {
    resolve_arg(side, c("x", "y"), null.ok = FALSE)
    `_class` <- switch(side,
      x = "XLayer",
      y = "YLayer"
    )
    Side <- switch(side,
      x = "Xside",
      y = "Yside"
    )
    names(mapping) <- rename_side(names(mapping), side)
    # check class
    geom <- check_subclass(geom, "Geom",
      env = parent.frame(),
      call = parent.frame()
    )
    stat <- check_subclass(stat, "Stat",
      env = parent.frame(),
      call = parent.frame()
    )

    # remaps
    geom_aes_map <- aes_to_map(geom, side)
    stat_aes_map <- aes_to_map(stat, side)
    remap <- union(geom_aes_map, stat_aes_map)
    # ggside_geom
    geom <- ggside_geom(paste0(Side, class(geom)[1]),
      geom = geom,
      side = side
    )
    stat <- ggside_stat(paste0(Side, class(stat)[1]),
      stat = stat,
      side = side
    )
    layer <- ggplot2::layer(
      geom = geom,
      stat = stat,
      data = data,
      mapping = mapping,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = params,
      check.aes = check.aes,
      check.param = check.param,
      key_glyph = key_glyph
    )

    new_ggside_layer(
      layer = ggproto(`_class`, layer),
      side = side,
      remap = remap
    )
  }


#' @rdname ggside_layer
#' @param layer a LayerInstance object made from \link[ggplot2]{layer}
#' @param side should the resulting `ggplot2_layer` be configured for x or y side
#' @export
as_ggside_layer <- function(layer, side) UseMethod("as_ggside_layer", layer)

#' @export
as_ggside_layer.ggside_layer <- function(layer, side) layer

#' @export
as_ggside_layer.LayerInstance <- function(layer, side = NULL) {
  if (!side %in% c("x", "y")) {
    stop("You must specify a side for the layer")
  }
  geom <- layer$geom
  new_geom <- ggside_geom("ggside_psudo_geom", geom, side)
  mapping <- force_panel_type_mapping(layer$mapping, side)
  names(mapping) <- rename_side(names(mapping), side)
  layer$mapping <- mapping
  layer$geom <- new_geom
  new_ggside_layer(layer, side)
}

new_ggside_layer <- function(layer, side, remap, constructor) {
  other <- switch(side,
    x = "y",
    y = "x"
  )
  type <- sprintf("%sside", side)
  ggproto(
    "ggside_layer",
    layer,
    setup_layer = new_ggproto_fun(layer$setup_layer, {
      names(plot$mapping) <- rename_side(names(plot$mapping), !!side)
      plot$mapping <- drop_plot_aes(plot$mapping, self$mapping, !!side)
      data <- call_parent_method
      data[["PANEL_TYPE"]] <- !!side
      data
    }),
    compute_aesthetics = new_ggproto_fun(layer$compute_aesthetics, {
      data <- call_parent_method
      aes_to_drop <- !!other
      if (all(paste0(c(!!side, ""), "colour") %in% names(data))) {
        aes_to_drop <- c(aes_to_drop, "colour")
      }
      if (all(paste0(c(!!side, ""), "fill") %in% names(data))) {
        aes_to_drop <- c(aes_to_drop, "fill")
      }
      data[, setdiff(names(data), aes_to_drop), drop = FALSE]
    }),
    compute_statistic = new_ggproto_fun(layer$compute_statistic, {
      # browser()
      push_aes(self$stat, "required_aes", !!type)
      data <- data_unmap(data, !!side)
      data <- call_parent_method
      data_map(data, !!side, !!remap)
    }),
    map_statistic = new_ggproto_fun(layer$map_statistic, {
      # old_nms <- names(self$stat$default_aes)
      # names(self$stat$default_aes) <- rename_side(names(self$stat$default_aes), !!side)
      # browser()
      data <- call_parent_method
      data
    }),
    compute_geom_1 = new_ggproto_fun(layer$compute_geom_1, {
      # browser()
      data <- parse_side_aes(data)
      push_aes(self$geom, "required_aes", !!type)
      levels <- levels(data$PANEL)
      data$PANEL <- droplevels(data$PANEL)
      data <- data_unmap(data, !!side)
      data <- call_parent_method
      data$PANEL <- factor(data$PANEL, levels = levels)
      data_map(data, !!side, !!remap)
    }),
    draw_geom = new_ggproto_fun(layer$draw_geom, {
      data <- data_unmap(data, !!side)
      data <- call_parent_method
      data_map(data, !!side, !!remap)
    }),
    compute_position = new_ggproto_fun(layer$compute_position, {
      data <- data_unmap(data, !!side)
      data <- call_parent_method
      data_map(data, !!side, !!remap)
    })
  )
}


drop_plot_aes <- function(plot_map, layer_map, side) {
  # if layer mapping fill/colour variant exists
  # remove it
  p_nms <- names(plot_map)
  l_nms <- names(layer_map)
  if (paste0(side, "colour") %in% l_nms &&
    any(to_drop <- p_nms %in% "colour")) {
    plot_map <- plot_map[!to_drop]
  }

  if (paste0(side, "fill") %in% l_nms &&
    any(to_drop <- p_nms %in% "fill")) {
    plot_map <- plot_map[!to_drop]
  }

  plot_map
}

#' @rdname ggside-ggproto-geoms
#' @export
parse_side_aes <- function(data, params) {
  # determine if fill, xfill, or yfill should be used
  all_names <- c(colnames(data))
  if (any(c("fill", "xfill", "yfill") %in% all_names)) {
    fill_opts <- all_names[all_names %in% c("fill", "xfill", "yfill")]
    side_fill <- c("xfill", "yfill") %in% fill_opts
    if (any(side_fill)) {
      fill_prec <- c("xfill", "yfill")[side_fill]
    } else {
      fill_prec <- "fill"
    }
    data[[fill_prec]] <- data[[fill_prec]]
    exclude <- fill_opts[!fill_opts %in% fill_prec]
    if (length(exclude) != 0) {
      data <- data[, setdiff(colnames(data), exclude), drop = F]
    }
  }

  if (any(c("colour", "xcolour", "ycolour") %in% all_names)) {
    colour_opts <- all_names[all_names %in% c("colour", "xcolour", "ycolour")]
    side_colour <- c("xcolour", "ycolour") %in% colour_opts
    if (any(side_colour)) {
      colour_prec <- c("xcolour", "ycolour")[side_colour]
    } else {
      colour_prec <- "colour"
    }
    data[[colour_prec]] <- data[[colour_prec]]
    exclude <- colour_opts[!colour_opts %in% colour_prec]
    if (length(exclude) != 0) {
      data <- data[, setdiff(colnames(data), exclude), drop = F]
    }
  }

  return(data)
}


push_aes <- function(ggproto, member, side) {
  ggproto_arg <- enexpr(ggproto)
  env <- caller_env()
  old_value <- ggproto[[member]]

  ggproto[[member]] <- sub(side, "", old_value)
  expr <- expr(on.exit((!!ggproto_arg)[[!!member]] <- !!old_value, add = TRUE))
  eval_bare(expr, env = env)
}

### INCLUDE BEGIN
#' @include utils-ggplot2-reimpl-facet.R
#' @include utils-ggplot2-reimpl-.R
#' @include utils-side-facet.R
#' @include utils-.R
NULL
### INCLUDE END

convertInd <- function(row, col, nrow) {
  (col - 1) * nrow + row
}

is.zero <- function(x) is.null(x) || inherits(x, "zeroGrob")

weave_tables_col <- function(table, table2, col_shift, col_width, name, z = 1, clip = "off") {
  panel_col <- panel_cols(table)$l
  panel_row <- panel_rows(table)$t
  for (i in rev(seq_along(panel_col))) {
    col_ind <- panel_col[i] + col_shift
    table <- gtable_add_cols(table, col_width[i], pos = col_ind)
    if (!missing(table2)) {
      table <- gtable_add_grob(table, table2[, i], t = panel_row, l = col_ind + 1, clip = clip, name = paste0(name, "-", seq_along(panel_row), "-", i), z = z)
    }
  }
  table
}
weave_tables_row <- function(table, table2, row_shift, row_height, name, z = 1, clip = "off") {
  panel_col <- panel_cols(table)$l
  panel_row <- panel_rows(table)$t
  for (i in rev(seq_along(panel_row))) {
    row_ind <- panel_row[i] + row_shift
    table <- gtable_add_rows(table, row_height[i], pos = row_ind)
    if (!missing(table2)) {
      table <- gtable_add_grob(table, table2[i, ], t = row_ind + 1, l = panel_col, clip = clip, name = paste0(name, "-", seq_along(panel_col), "-", i), z = z)
    }
  }
  table
}

sideFacetWrap_draw_panels <- function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
  if ((params$free$x || params$free$y) && !coord$is_free()) {
    abort(glue("{snake_class(coord)} doesn't support free scales"))
  }

  if (inherits(coord, "CoordFlip")) {
    if (params$free$x) {
      layout$SCALE_X <- seq_len(nrow(layout))
    } else {
      layout$SCALE_X <- 1L
    }
    if (params$free$y) {
      layout$SCALE_Y <- seq_len(nrow(layout))
    } else {
      layout$SCALE_Y <- 1L
    }
  }
  if (params$ggside$strip != "default") {
    warn("`ggside(strip = 'main', ...)` is only compatible with `facet_grid(...)`, not `facet_wrap(...)`",
      .frequency = "regularly",
      .frequency_id = "ggside_strip_misuse_wrap"
    )
  }
  collapse <- params$ggside$collapse %||% "default"
  collapse_y <- collapse %in% c("all", "y")
  collapse_x <- collapse %in% c("all", "x")
  ncol <- max(layout$COL)
  nrow <- max(layout$ROW)
  n <- nrow(layout)
  panel_order <- order(layout$ROW, layout$COL)
  layout <- layout[panel_order, ]
  panels <- panels[panel_order]
  panel_pos <- convertInd(layout$ROW, layout$COL, nrow)
  layout$panel_pos <- panel_pos
  side_panels_present <- c("x", "y")[c("x", "y") %in% layout$PANEL_TYPE]
  x.pos <- params$ggside$x.pos
  y.pos <- params$ggside$y.pos
  ranges <- map_panel_type(ranges, layout$PANEL_TYPE)

  axes <- render_axes(ranges, ranges, coord, theme, transpose = TRUE)

  layout <- unwrap(layout, by = c("ROW", "COL"), cols = "FACET_VARS")

  if (length(params$facets) == 0) {
    # Add a dummy label
    labels_df <- data_frame0(`(all)` = "(all)")
  } else {
    labels_df <- unique(layout[layout[["PANEL_TYPE"]] == "main", names(params$facets), drop = FALSE])
  }
  attr(labels_df, "facet") <- "wrap"
  strips <- render_strips(
    structure(labels_df, type = "rows"),
    structure(labels_df, type = "cols"),
    params$labeller, theme
  )

  # If user hasn't set aspect ratio, and we have fixed scales, then
  # ask the coordinate system if it wants to specify one
  aspect_ratio <- theme$aspect.ratio
  if (is.null(aspect_ratio) && !params$free$x && !params$free$y) {
    aspect_ratio <- coord$aspect(ranges[[layout[layout$PANEL_TYPE == "main", ]$PANEL[1L]]])
  }

  if (is.null(aspect_ratio)) {
    aspect_ratio <- 1
    respect <- FALSE
  } else {
    respect <- TRUE
  }
  # theme side panel scale
  side.panel.scale.x <- calc_element("ggside.panel.scale.x", theme)
  side.panel.scale.y <- calc_element("ggside.panel.scale.y", theme)

  empty_table <- matrix(list(zeroGrob()), nrow = nrow, ncol = ncol)
  panel_table <- empty_table
  panel_table[panel_pos] <- panels
  empties <- apply(panel_table, c(1, 2), function(x) is.zero(x[[1]]))
  p.widths <- if ("y" %in% side_panels_present) {
    if (collapse_y) {
      .widths <- rep(1, ncol - 1)
      .widths <- c(.widths, sum(.widths) * side.panel.scale.y)
      if (y.pos == "left") {
        .widths <- rev(.widths)
      }
      unit(.widths, "null")
    } else {
      .widths <- c(1, side.panel.scale.y)
      if (y.pos == "left") {
        .widths <- rev(.widths)
      }
      unit(rep(.widths, ncol / 2), "null")
    }
  } else {
    unit(rep(1, ncol), "null")
  }
  p.heights <- if ("x" %in% side_panels_present) {
    if (collapse_x) {
      .heights <- rep(c(aspect_ratio), nrow - 1)
      .heights <- c(.heights, sum(.heights) * side.panel.scale.x)
      if (x.pos == "top") {
        .heights <- rev(.heights)
      }
      unit(.heights, "null")
    } else {
      .heights <- c(aspect_ratio, aspect_ratio * side.panel.scale.x)
      if (x.pos == "top") {
        .heights <- rev(.heights)
      }
      unit(rep(abs(.heights), nrow / 2), "null")
    }
  } else {
    unit(rep(aspect_ratio, nrow), "null")
  }
  panel_table <- gtable_matrix("layout", panel_table,
    widths = p.widths,
    heights = p.heights, respect = respect,
    clip = coord$clip, z = matrix(1, ncol = ncol, nrow = nrow)
  )
  panel_table$layout$name <- paste0("panel-", rep(seq_len(ncol), nrow), "-", rep(seq_len(nrow), each = ncol))

  # need to register theme element
  sidepanel.spacing.x <- calc_element("ggside.panel.spacing.x", theme)
  xpanel_spacing <- calc_element("panel.spacing.x", theme)
  sidepanel.spacing.y <- calc_element("ggside.panel.spacing.y", theme)
  ypanel_spacing <- calc_element("panel.spacing.y", theme)
  col.widths <- if ("y" %in% side_panels_present) {
    if (collapse_y) {
      .tmp <- rep(c(xpanel_spacing), length(panel_table$widths) - 2)
      if (y.pos == "left") {
        unit(c(sidepanel.spacing.x, .tmp), "pt")
      } else {
        unit(c(.tmp, sidepanel.spacing.x), "pt")
      }
    } else {
      unit(rep(c(sidepanel.spacing.x, xpanel_spacing), length.out = length(panel_table$widths) - 1), "pt")
    }
  } else {
    xpanel_spacing
  }
  panel_table <- gtable_add_col_space(panel_table, col.widths)
  row.heights <- if ("x" %in% side_panels_present) {
    if (collapse_x) {
      .tmp <- rep(c(ypanel_spacing), length(panel_table$heights) - 2)
      if (x.pos == "top") {
        unit(c(sidepanel.spacing.y, .tmp), "pt")
      } else {
        unit(c(.tmp, sidepanel.spacing.y), "pt")
      }
    } else {
      unit(rep(c(sidepanel.spacing.y, ypanel_spacing), length.out = length(panel_table$heights) - 1), "pt")
    }
  } else {
    ypanel_spacing
  }

  panel_table <- gtable_add_row_space(panel_table, row.heights)
  # Add axes
  axis_mat_x_top <- empty_table
  axis_mat_x_top[panel_pos] <- axes$x$top
  axis_mat_x_bottom <- empty_table
  axis_mat_x_bottom[panel_pos] <- axes$x$bottom
  axis_mat_y_left <- empty_table
  axis_mat_y_left[panel_pos] <- axes$y$left
  axis_mat_y_right <- empty_table
  axis_mat_y_right[panel_pos] <- axes$y$right

  .xgroupby <- if (!params$free$x | collapse_x) "COL" else c("COL", "PANEL_GROUP")
  .y <- NULL # if("PANEL_GROUP" %in% .xgroupby) "y" else NULL
  .ygroupby <- if (!params$free$y | collapse_y) "ROW" else c("ROW", "PANEL_GROUP")
  .x <- NULL # if("PANEL_GROUP" %in% .ygroupby) "x" else NULL

  bottom <- do_by(layout[!layout[["PANEL_TYPE"]] %in% .y, ], .xgroupby,
    function(x, on) {
      x[["ROW2"]] <- switch(on,
        default = max(x[["ROW"]]),
        main = max(x[["ROW"]][x[["PANEL_TYPE"]] != "x"]),
        side = max(x[["ROW"]][x[["PANEL_TYPE"]] != "main"])
      )
      x
    },
    on = params$ggside$draw_x_on
  )
  top <- do_by(layout[!layout[["PANEL_TYPE"]] %in% .y, ], .xgroupby,
    function(x, on) {
      x[["ROW2"]] <- switch(on,
        default = min(x[["ROW"]]),
        main = min(x[["ROW"]][x[["PANEL_TYPE"]] != "x"]),
        side = min(x[["ROW"]][x[["PANEL_TYPE"]] != "main"])
      )
      x
    },
    on = params$ggside$draw_x_on
  )
  right <- do_by(layout[!layout[["PANEL_TYPE"]] %in% .x, ], .ygroupby,
    function(x, on) {
      x[["COL2"]] <- switch(on,
        default = max(x[["COL"]]),
        main = max(x[["COL"]][!x[["PANEL_TYPE"]] == "y"]),
        side = max(x[["COL"]][!x[["PANEL_TYPE"]] == "main"])
      )
      x
    },
    on = params$ggside$draw_y_on
  )
  left <- do_by(layout[!layout[["PANEL_TYPE"]] %in% .x, ], .ygroupby,
    function(x, on) {
      x[["COL2"]] <- switch(on,
        default = min(x[["COL"]]),
        main = min(x[["COL"]][!x[["PANEL_TYPE"]] == "y"]),
        side = min(x[["COL"]][!x[["PANEL_TYPE"]] == "main"])
      )
      x
    },
    on = params$ggside$draw_y_on
  )

  if (params$ggside$scales %in% c("free", "free_y")) { # if y is free, include x PANELS_TYPES
    right <- right[right[["COL"]] == right[["COL2"]] | right[["PANEL_TYPE"]] == "x", ]
    left <- left[left[["COL"]] == left[["COL2"]] | left[["PANEL_TYPE"]] == "x", ]
  } else {
    right <- right[right[["COL"]] == right[["COL2"]], ]
    left <- left[left[["COL"]] == left[["COL2"]], ]
  }
  if (params$ggside$scales %in% c("free", "free_x")) { # if x is free, include y PANELS_TYPES
    top <- top[top[["ROW"]] == top[["ROW2"]] | top[["PANEL_TYPE"]] == "y", ]
    bottom <- bottom[bottom[["ROW"]] == bottom[["ROW2"]] | bottom[["PANEL_TYPE"]] == "y", ]
  } else {
    top <- top[top[["ROW"]] == top[["ROW2"]], ]
    bottom <- bottom[bottom[["ROW"]] == bottom[["ROW2"]], ]
  }

  # top, left, bottom, right, variables includes panels that would have axis shown
  # for their relavent positions.
  # Do an anti_join against layout to find panels that should get a zeroGrobe
  bottom <- anti_join(layout, bottom, by = c("ROW", "COL"))[["panel_pos"]]
  top <- anti_join(layout, top, by = c("ROW", "COL"))[["panel_pos"]]
  right <- anti_join(layout, right, by = c("ROW", "COL"))[["panel_pos"]]
  left <- anti_join(layout, left, by = c("ROW", "COL"))[["panel_pos"]]
  # pulled panel positions
  # Place ZeroGrobs
  axis_mat_x_top[top] <- list(zeroGrob())
  axis_mat_x_bottom[bottom] <- list(zeroGrob())
  axis_mat_y_left[left] <- list(zeroGrob())
  axis_mat_y_right[right] <- list(zeroGrob())

  # calculate spacing based on ggside params and layout
  spacing <- calc_panel_spacing(
    ggside = params$ggside, layout = layout,
    top = axis_mat_x_top, right = axis_mat_y_right,
    bot = axis_mat_x_bottom, left = axis_mat_y_left
  )
  panel_table <- weave_tables_row(panel_table, axis_mat_x_top, -1, spacing$top, "axis-t", 3)
  panel_table <- weave_tables_row(panel_table, axis_mat_x_bottom, 0, spacing$bot, "axis-b", 3)
  panel_table <- weave_tables_col(panel_table, axis_mat_y_left, -1, spacing$left, "axis-l", 3)
  panel_table <- weave_tables_col(panel_table, axis_mat_y_right, 0, spacing$right, "axis-r", 3)

  params$strip.position <- params$strip.position %||% "top"
  if (any(c("top", "bottom") %in% params$strip.position)) {
    strip_panel_pos <- do_by(layout[!is.na(layout[["PANEL_GROUP"]]), ], "PANEL_GROUP",
      function(x, strip) {
        if ("top" %in% strip) {
          x[["ROW"]] <- min(x[["ROW"]])
        } else {
          x[["ROW"]] <- max(x[["ROW"]])
        }
        x
      },
      strip = params$strip.position
    )
    strip_panel_pos <- semi_join(layout, strip_panel_pos, by = c("PANEL_GROUP", "ROW"))
    strip_panel_pos <- unique(strip_panel_pos[!strip_panel_pos[["PANEL_TYPE"]] == "y", c("PANEL", "panel_pos")])
  } else if (any(c("left", "right") %in% params$strip.position)) {
    strip_panel_pos <- do_by(layout[!is.na(layout[["PANEL_GROUP"]]), ], "PANEL_GROUP",
      function(x, strip) {
        if ("left" %in% strip) {
          x[["COL"]] <- min(x[["COL"]])
        } else {
          x[["COL"]] <- max(x[["COL"]])
        }
        x
      },
      strip = params$strip.position
    )
    strip_panel_pos <- semi_join(layout, strip_panel_pos, by = c("PANEL_GROUP", "COL"))
    strip_panel_pos <- unique(strip_panel_pos[!strip_panel_pos[["PANEL_TYPE"]] == "x", c("PANEL", "panel_pos")])
  }


  strip_padding <- convertUnit(
    calc_element("strip.switch.pad.grid", theme), "cm"
  )
  strip_name <- paste0("strip-", substr(params$strip.position, 1, 1))
  strip_mat <- empty_table
  strip_mat[strip_panel_pos$panel_pos] <- unlist(unname(strips), recursive = FALSE)[[params$strip.position]]
  if (params$strip.position %in% c("top", "bottom")) {
    inside_x <- (theme$strip.placement.x %||% theme$strip.placement %||% "inside") == "inside"
    if (params$strip.position == "top") {
      placement <- if (inside_x) -1 else -2
      strip_pad <- spacing$top
    } else {
      placement <- if (inside_x) 0 else 1
      strip_pad <- spacing$bot
    }
    strip_height <- unit(apply(strip_mat, 1, max_height, value_only = TRUE), "cm")
    panel_table <- weave_tables_row(panel_table, strip_mat, placement, strip_height, strip_name, 2, coord$clip)
    if (!inside_x) {
      strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
      panel_table <- weave_tables_row(panel_table, row_shift = placement, row_height = strip_pad)
    }
  } else {
    inside_y <- (theme$strip.placement.y %||% theme$strip.placement %||% "inside") == "inside"
    if (params$strip.position == "left") {
      placement <- if (inside_y) -1 else -2
      strip_pad <- spacing$left
    } else {
      placement <- if (inside_y) 0 else 1
      strip_pad <- spacing$right
    }
    strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
    strip_width <- unit(apply(strip_mat, 2, max_width, value_only = TRUE), "cm")
    panel_table <- weave_tables_col(panel_table, strip_mat, placement, strip_width, strip_name, 2, coord$clip)
    if (!inside_y) {
      strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
      panel_table <- weave_tables_col(panel_table, col_shift = placement, col_width = strip_pad)
    }
  }
  panel_table
}


sideFacetWrap_map_data <- function(data, layout, params) {
  if (empty(data)) {
    return(cbind(data, PANEL = integer(0)))
  }
  if (!"PANEL_TYPE" %in% colnames(data)) {
    data$PANEL_TYPE <- "main"
  }
  layout <- unwrap(layout, c("ROW", "COL"), "FACET_VARS")
  vars <- c(params$facets, PANEL_TYPE = quo(PANEL_TYPE))
  if (length(vars) == 0) {
    data$PANEL <- layout$PANEL
    return(data)
  }

  facet_vals <- eval_facets(vars, data, params$.possible_columns)
  facet_vals[] <- lapply(facet_vals[], as.factor)
  missing_facets <- setdiff(names(vars), names(facet_vals))
  if (length(missing_facets) > 0) {
    to_add <- unique(layout[missing_facets])
    data_rep <- rep.int(1:nrow(data), nrow(to_add))
    facet_rep <- rep(1:nrow(to_add), each = nrow(data))
    data <- unrowname(data[data_rep, , drop = FALSE])
    facet_vals <- unrowname(cbind(facet_vals[data_rep, ,
      drop = FALSE
    ], to_add[facet_rep, , drop = FALSE]))
  }

  keys <- join_keys(facet_vals, layout, by = names(vars))
  data$PANEL <- layout$PANEL[match(keys$x, keys$y)]
  data
}

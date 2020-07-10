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



sideFacet_draw_panels <- function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
  if ((params$free$x || params$free$y) && !coord$is_free()) {
    abort(glue("{snake_class(coord)} doesn't support free scales"))
  }
  #browser()
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

  ncol <- max(layout$COL)
  nrow <- max(layout$ROW)
  n <- nrow(layout)
  panel_order <- order(layout$ROW, layout$COL)
  layout <- layout[panel_order, ]
  panels <- panels[panel_order]
  panel_pos <- convertInd(layout$ROW, layout$COL, nrow)
  layout$panel_pos <- panel_pos

  axes <- render_axes(ranges, ranges, coord, theme, transpose = TRUE)

  if (length(params$facets) == 0) {
    # Add a dummy label
    labels_df <- new_data_frame(list("(all)" = "(all)"), n = 1)
  } else {
    labels_df <- layout[names(params$facets)]
  }
  attr(labels_df, "facet") <- "wrap"
  strips <- render_strips(
    structure(labels_df, type = "rows"),
    structure(labels_df, type = "cols"),
    params$labeller, theme)

  # If user hasn't set aspect ratio, and we have fixed scales, then
  # ask the coordinate system if it wants to specify one
  aspect_ratio <- theme$aspect.ratio
  if (is.null(aspect_ratio) && !params$free$x && !params$free$y) {
    aspect_ratio <- coord$aspect(ranges[[1]])
  }

  if (is.null(aspect_ratio)) {
    aspect_ratio <- 1
    respect <- FALSE
  } else {
    respect <- TRUE
  }

  empty_table <- matrix(list(zeroGrob()), nrow = nrow, ncol = ncol)
  panel_table <- empty_table
  panel_table[panel_pos] <- panels
  empties <- apply(panel_table, c(1,2), function(x) is.zero(x[[1]]))
  p.widths <- unit(rep(c(1,.1), ncol/2), "null")
  p.heights <- unit(rep(abs(c(aspect_ratio,aspect_ratio*.1)), nrow/2), "null")
  panel_table <- gtable_matrix("layout", panel_table,
                               widths = p.widths,
                               heights = p.heights, respect = respect,
                               clip = coord$clip, z = matrix(1, ncol = ncol, nrow = nrow))
  panel_table$layout$name <- paste0('panel-', rep(seq_len(ncol), nrow), '-', rep(seq_len(nrow), each = ncol))

  #need to register theme element
  sidepanel.spacing <- theme$sidepanel.spacing %||% unit(as.numeric(theme$panel.spacing)*.25,"pt")
  sidepanel.spacing.x <- theme$sidepanel.spacing.x %||% if(is.null(theme$panel.spacing.x)) sidepanel.spacing else unit(as.numeric(theme$panel.spacing.x)*.25,"pt")
  sidepanel.spacing.y <- theme$sidepanel.spacing.y %||% if(is.null(theme$panel.spacing.y)) sidepanel.spacing else unit(as.numeric(theme$panel.spacing.y)*.25,"pt")
  col.widths <- unit(rep(c(sidepanel.spacing.x, (theme$panel.spacing.x %||% theme$panel.spacing)), length.out = length(panel_table$widths)-1), "pt")
  panel_table <- gtable_add_col_space(panel_table,
                                      col.widths)
  row.heights <- unit(rep(c(sidepanel.spacing.y, (theme$panel.spacing.y %||% theme$panel.spacing)), length.out = length(panel_table$heights)-1), "pt")
  panel_table <- gtable_add_row_space(panel_table,
                                      row.heights)
  browser()
  # Add axes
  axis_mat_x_top <- empty_table
  axis_mat_x_top[panel_pos] <- axes$x$top[layout$SCALE_X]
  axis_mat_x_bottom <- empty_table
  axis_mat_x_bottom[panel_pos] <- axes$x$bottom[layout$SCALE_X]
  axis_mat_y_left <- empty_table
  axis_mat_y_left[panel_pos] <- axes$y$left[layout$SCALE_Y]
  axis_mat_y_right <- empty_table
  axis_mat_y_right[panel_pos] <- axes$y$right[layout$SCALE_Y]
  if (!params$free$x) {

    axis_mat_x_top[-1,]<- list(zeroGrob())
    axis_mat_x_bottom[-nrow,]<- list(zeroGrob())
  }
  if (!params$free$y) {
    axis_mat_y_left[, -1] <- list(zeroGrob())
    axis_mat_y_right[, -ncol] <- list(zeroGrob())
  }
  axis_height_top <- unit(
    apply(axis_mat_x_top, 1, max_height, value_only = TRUE),
    "cm"
  )
  axis_height_bottom <- unit(
    apply(axis_mat_x_bottom, 1, max_height, value_only = TRUE),
    "cm"
  )
  axis_width_left <- unit(
    apply(axis_mat_y_left, 2, max_width, value_only = TRUE),
    "cm"
  )
  axis_width_right <- unit(
    apply(axis_mat_y_right, 2, max_width, value_only = TRUE),
    "cm"
  )
  # Add back missing axes
  # if (any(empties)) {
  #   first_row <- which(apply(empties, 1, any))[1] - 1
  #   first_col <- which(apply(empties, 2, any))[1] - 1
  #   row_panels <- which(layout$ROW == first_row & layout$COL > first_col)
  #   row_pos <- convertInd(layout$ROW[row_panels], layout$COL[row_panels], nrow)
  #   row_axes <- axes$x$bottom[layout$SCALE_X[row_panels]]
  #   col_panels <- which(layout$ROW > first_row & layout$COL == first_col)
  #   col_pos <- convertInd(layout$ROW[col_panels], layout$COL[col_panels], nrow)
  #   col_axes <- axes$y$right[layout$SCALE_Y[col_panels]]
  #   inside <- (theme$strip.placement %||% "inside") == "inside"
  #   if (params$strip.position == "bottom" &&
  #       !inside &&
  #       any(!vapply(row_axes, is.zero, logical(1))) &&
  #       !params$free$x) {
  #     warn("Suppressing axis rendering when strip.position = 'bottom' and strip.placement == 'outside'")
  #   } else {
  #     axis_mat_x_bottom[row_pos] <- row_axes
  #   }
  #   if (params$strip.position == "right" &&
  #       !inside &&
  #       any(!vapply(col_axes, is.zero, logical(1))) &&
  #       !params$free$y) {
  #     warn("Suppressing axis rendering when strip.position = 'right' and strip.placement == 'outside'")
  #   } else {
  #     axis_mat_y_right[col_pos] <- col_axes
  #   }
  # }
  panel_table <- weave_tables_row(panel_table, axis_mat_x_top, -1, axis_height_top, "axis-t", 3)
  panel_table <- weave_tables_row(panel_table, axis_mat_x_bottom, 0, axis_height_bottom, "axis-b", 3)
  panel_table <- weave_tables_col(panel_table, axis_mat_y_left, -1, axis_width_left, "axis-l", 3)
  panel_table <- weave_tables_col(panel_table, axis_mat_y_right, 0, axis_width_right, "axis-r", 3)
  #return(panel_table)
  params$strip.position <- params$strip.position %||% "top"
  main_panel_pos <- layout %>% filter(PANEL_TYPE%in%"main") %>% pull(panel_pos)

  strip_padding <- convertUnit(theme$strip.switch.pad.wrap, "cm")
  strip_name <- paste0("strip-", substr(params$strip.position, 1, 1))
  strip_mat <- empty_table
  strip_mat[main_panel_pos] <- unlist(unname(strips), recursive = FALSE)[[params$strip.position]]
  if (params$strip.position %in% c("top", "bottom")) {
    inside_x <- (theme$strip.placement.x %||% theme$strip.placement %||% "inside") == "inside"
    if (params$strip.position == "top") {
      placement <- if (inside_x) -1 else -2
      strip_pad <- axis_height_top
    } else {
      placement <- if (inside_x) 0 else 1
      strip_pad <- axis_height_bottom
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
      strip_pad <- axis_width_left
    } else {
      placement <- if (inside_y) 0 else 1
      strip_pad <- axis_width_right
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

#' @export
make_sideFacets <- function(facet){

  sideFacets <- function(layout,
                         sidePanel = c("x","y"),
                         scales = "fixed"){
    if(all(c("x","y")%in%sidePanel)){
      main <- layout %>%
        mutate(ROW = ROW*2-1,
               COL = COL*2-1,
               PANEL_TYPE = "main")
      yp <- layout %>%
        mutate(ROW = ROW*2-1,
               COL = COL*2,
               SCALE_X = max(SCALE_X) + 1,
               PANEL_TYPE = "y")
      xp <- layout %>%
        mutate(ROW = ROW*2,
               COL = COL*2-1,
               SCALE_Y = max(SCALE_Y) + 1,
               PANEL_TYPE = "x")
      layout <- bind_rows(main, yp, xp)

    } else if("x"%in% sidePanel){
      main <- layout %>%
        mutate(ROW = ROW*2-1,
               PANEL_TYPE = "main")
      xp <- layout %>%
        mutate(ROW = ROW*2,
               SCALE_Y = max(SCALE_Y) + 1,
               PANEL_TYPE = "x")
      layout <- bind_rows(main, xp)
    } else {
      main <- layout %>%
        mutate(COL = COL*2-1,
               PANEL_TYPE = "main")
      yp <- layout %>%
        mutate(COL = COL*2,
               SCALE_X = max(SCALE_X) + 1,
               PANEL_TYPE = "y")
      layout <- bind_rows(main, yp)

    }
    layout <- layout %>%
      arrange(ROW, COL) %>%
      mutate(PANEL = factor(1:n()))
    return(layout)
  }

  ggproto(NULL,
          facet,
          compute_layout = function(data, params,
                                    facet_compute = facet$compute_layout){
            browser()
            layout <- facet_compute(data, params)
            layout <- sideFacets(layout)
            layout },
          map_data = function(data, layout,
                              params, facet_mapping = facet$map_data){
            browser()
            facet_vars <- c(names(params$facets),names(params$rows),names(params$cols))
            if(is.null(facet_vars)){
              data <- mutate(data,
                             PANEL = list(unique(layout$PANEL)))
            } else {
              panels <- layout %>% group_by(.dots = facet_vars) %>%
                summarise(PANEL = list(unique(PANEL)))
              data <- left_join(data, panels, by = facet_vars)
            }

            data %>%
              tidyr::unnest(PANEL)
          },
          draw_panels = sideFacet_draw_panels

  )
}



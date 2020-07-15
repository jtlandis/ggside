

sideFacetGrid_draw_panels <- function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params, ggside) {
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
  side_panels_present <- c("x","y")[c("x","y")%in%layout$PANEL_TYPE]
  x.pos <- params$ggside$x.pos
  y.pos <- params$ggside$y.pos

  axes <- render_axes(ranges, ranges, coord, theme, transpose = TRUE)

  col_vars <- unique(layout[names(params$cols)])
  row_vars <- unique(layout[names(params$rows)])
  # Adding labels metadata, useful for labellers
  attr(col_vars, "type") <- "cols"
  attr(col_vars, "facet") <- "grid"
  attr(row_vars, "type") <- "rows"
  attr(row_vars, "facet") <- "grid"
  strips <- render_strips(col_vars, row_vars, params$labeller, theme)

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
  #theme side panel scale
  side.panel.scale.x <- theme$sidepanel.scale.x %||% theme$sidepanel.scale %||% .1
  side.panel.scale.y <- theme$sidepanel.scale.y %||% theme$sidepanel.scale %||% .1

  if (params$space_free$x) {
    ps <- layout$PANEL[layout$ROW == 1]
    widths <- vapply(ps, function(i) diff(ranges[[i]]$x.range), numeric(1))
  } else if("y"%in%side_panels_present){
    widths <- rep(1, ncol/2)
  } else {
    widths <- rep(1, ncol)
  }
  if ("y" %in% side_panels_present){
    widths <- vapply(widths, function(x, y, z){
      vec <- c(1, y)
      if(z){
        vec <- rev(vec)
      }
      x <- x*vec
      x}, FUN.VALUE = numeric(2), y = side.panel.scale.y, z = y.pos=="left")
    widths <- as.vector(widths)
  }
  panel_widths <- unit(widths, "null")

  if (params$space_free$y) {
    ps <- layout$PANEL[layout$COL == 1]
    heights <- vapply(ps, function(i) diff(ranges[[i]]$y.range), numeric(1))
  } else if ("x"%in%side_panels_present){
    heights <- rep(1*aspect_ratio, nrow/2)
  } else {
    heights <- rep(1*aspect_ratio, nrow)
  }
  if ("x" %in% side_panels_present){
    heights <- vapply(heights, function(x, y, z){
      vec <- c(1, y)
      if(z){
        vec <- rev(vec)
      }
      x <- x*vec
      x}, FUN.VALUE = numeric(2), y = side.panel.scale.x, z = x.pos=="top")
    heights <- as.vector(heights)
  }
  panel_heights <- unit(heights, "null")

  empty_table <- matrix(list(zeroGrob()), nrow = nrow, ncol = ncol)
  panel_table <- empty_table
  panel_table[panel_pos] <- panels
  empties <- apply(panel_table, c(1,2), function(x) is.zero(x[[1]]))
  panel_table <- gtable_matrix("layout", panel_table,
                               widths = panel_widths,
                               heights = panel_heights, respect = respect,
                               clip = coord$clip, z = matrix(1, ncol = ncol, nrow = nrow))
  panel_table$layout$name <- paste0('panel-', rep(seq_len(ncol), nrow), '-', rep(seq_len(nrow), each = ncol))

  #need to register theme element
  sidepanel.spacing <- theme$sidepanel.spacing %||% unit(as.numeric(theme$panel.spacing)*.25,"pt")
  sidepanel.spacing.x <- theme$sidepanel.spacing.x %||%
    if(is.null(theme$panel.spacing.x)) sidepanel.spacing else unit(as.numeric(theme$panel.spacing.x)*.25,"pt")
  xpanel_spacing <- theme$panel.spacing.x %||% theme$panel.spacing
  sidepanel.spacing.y <- theme$sidepanel.spacing.y %||%
    if(is.null(theme$panel.spacing.y)) sidepanel.spacing else unit(as.numeric(theme$panel.spacing.y)*.25,"pt")
  ypanel_spacing <- theme$panel.spacing.y %||% theme$panel.spacing
  col.widths <- if("y"%in%side_panels_present){
    unit(rep(c(sidepanel.spacing.x, xpanel_spacing), length.out = length(panel_table$widths)-1), "pt")
  } else {
    xpanel_spacing
  }
  panel_table <- gtable_add_col_space(panel_table, col.widths)
  row.heights <- if("x"%in%side_panels_present){
    unit(rep(c(sidepanel.spacing.y, ypanel_spacing), length.out = length(panel_table$heights)-1), "pt")
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

  .xgroupby <- if(!params$free$x) "COL" else c("COL","PANEL_GROUP")
  .ygroupby <- if(!params$free$y) "ROW" else c("ROW","PANEL_GROUP")

  bottom <- layout %>% group_by(.dots = .xgroupby) %>%
    mutate(ROW2 = max(ROW))
  top <- layout %>% group_by(COL) %>%
    mutate(ROW2 = min(ROW))
  right <- layout %>% group_by(.dots = .ygroupby) %>%
    mutate(COL2 = max(COL))
  left <- layout %>% group_by(.dots = .ygroupby) %>%
    mutate(COL2 = min(COL))

  if(params$ggside$scales%in%c("free","free_y")){ #if y is free, include x PANELS_TYPES
    right <- filter(right, COL==COL2|PANEL_TYPE%in%"x")
    left <- filter(left, COL==COL2|PANEL_TYPE%in%"x")
  } else {
    right <- filter(right, COL==COL2)
    left <- filter(left, COL==COL2)
  }
  if(params$ggside$scales%in%c("free","free_x")){ #if x is free, include y PANELS_TYPES
    top <- filter(top, ROW==ROW2|PANEL_TYPE%in%"y")
    bottom <- filter(bottom, ROW==ROW2|PANEL_TYPE%in%"y")
  } else {
    top <- filter(top, ROW==ROW2)
    bottom <- filter(bottom, ROW==ROW2)
  }

  #top, left, bottom, right, variables includes panels that would have axis shown
  #for their relavent positions.
  #Do an anti_join against layout to find panels that should get a zeroGrobe
  bottom <- bottom %>% select(-ROW2) %>% {
    suppressMessages(anti_join(x = layout, y = .))
  } %>% pull(panel_pos)
  top <- top %>% select(-ROW2) %>% {
    suppressMessages(anti_join(x = layout, y = .))
  } %>% pull(panel_pos)
  right <- right %>% select(-COL2) %>% {
    suppressMessages(anti_join(x = layout, y = .))
  } %>% pull(panel_pos)
  left <- left %>% select(-COL2) %>% {
    suppressMessages(anti_join(x = layout, y = .))
  } %>% pull(panel_pos)
  #pulled panel positions
  #Place ZeroGrobs
  axis_mat_x_top[top]<- list(zeroGrob())
  axis_mat_x_bottom[bottom]<- list(zeroGrob())
  axis_mat_y_left[left] <- list(zeroGrob())
  axis_mat_y_right[right] <- list(zeroGrob())

  if(all(c("x","y") %in% side_panels_present)){
    x_pos <- layout %>% filter(PANEL_TYPE %in%"x") %>% pull(panel_pos)
    if(y.pos=="left"){
      for(i in x_pos){
        axis_mat_y_left[i][[1]]$width <- NULL
      }
    } else {
      for(i in x_pos){
        axis_mat_y_right[i][[1]]$width <- NULL
      }
    }

    y_pos <- layout %>% filter(PANEL_TYPE %in%"y") %>% pull(panel_pos)
    if(x.pos=="top"){
      for(i in y_pos){
        axis_mat_x_top[i][[1]]$height <- NULL
      }
    } else {
      for(i in y_pos){
        axis_mat_x_bottom[i][[1]]$height <- NULL
      }
    }
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
  panel_table <- weave_tables_row(panel_table, axis_mat_x_top, -1, axis_height_top, "axis-t", 3)
  panel_table <- weave_tables_row(panel_table, axis_mat_x_bottom, 0, axis_height_bottom, "axis-b", 3)
  panel_table <- weave_tables_col(panel_table, axis_mat_y_left, -1, axis_width_left, "axis-l", 3)
  panel_table <- weave_tables_col(panel_table, axis_mat_y_right, 0, axis_width_right, "axis-r", 3)
  browser()
  #By default strip.positions are top and right. if switch == "both" set to bottom and left
  strip.position <- params$switch %||% "default"
  strip.position <- switch(strip.position,
                           default = c("top","right"), both = c("bottom","left"),
                           x = c("bottom","right"), y = c("top", "left"))
  vertical.strip <- c("top","bottom")[c("top","bottom")%in% strip.position]
  horizont.strip <- c("left","right")[c("left","right")%in% strip.position]
  Vstrip_panel_pos <- layout %>% group_by(COL) %>%
    summarise(ROW = case_when(vertical.strip=="top" ~ min(ROW),
                              TRUE ~ max(ROW))) %>%
    semi_join(x = layout, y = ., by = c("COL","ROW")) %>%
    filter(!PANEL_TYPE %in% "y") %>% distinct(PANEL, panel_pos)

  Hstrip_panel_pos <- layout %>% group_by(ROW) %>%
    summarise(COL = case_when(horizont.strip=="left" ~ min(COL),
                              TRUE ~ max(COL))) %>%
    semi_join(x = layout, y = ., by = c("ROW","COL")) %>%
    filter(!PANEL_TYPE %in% "x") %>% distinct(PANEL, panel_pos)

  strip_padding <- convertUnit(theme$strip.switch.pad.wrap, "cm")
  hstrip_name <- paste0("strip-", substr(horizont.strip, 1, 1))
  vstrip_name <- paste0("strip-", substr(vertical.strip, 1, 1))
  vstrip_mat <- empty_table
  vstrip_mat[Vstrip_panel_pos$panel_pos] <- unlist(unname(strips), recursive = FALSE)[[vertical.strip]]
  hstrip_mat <- empty_table
  hstrip_mat[Hstrip_panel_pos$panel_pos] <- unlist(unname(strips), recursive = FALSE)[[horizont.strip]]

  inside_x <- (theme$strip.placement.x %||% theme$strip.placement %||% "inside") == "inside"
  if (vertical.strip == "top") {
    placement <- if (inside_x) -1 else -2
    strip_pad <- axis_height_top
  } else {
    placement <- if (inside_x) 0 else 1
    strip_pad <- axis_height_bottom
  }
  strip_height <- unit(apply(vstrip_mat, 1, max_height, value_only = TRUE), "cm")
  panel_table <- weave_tables_row(panel_table, vstrip_mat, placement, strip_height, vstrip_name, 2, coord$clip)
  if (!inside_x) {
    strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
    panel_table <- weave_tables_row(panel_table, row_shift = placement, row_height = strip_pad)
  }

  inside_y <- (theme$strip.placement.y %||% theme$strip.placement %||% "inside") == "inside"
  if (horizont.strip == "left") {
    placement <- if (inside_y) -1 else -2
    strip_pad <- axis_width_left
  } else {
    placement <- if (inside_y) 0 else 1
    strip_pad <- axis_width_right
  }
  strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
  strip_width <- unit(apply(hstrip_mat, 2, max_width, value_only = TRUE), "cm")
  panel_table <- weave_tables_col(panel_table, hstrip_mat, placement, strip_width, hstrip_name, 2, coord$clip)
  if (!inside_y) {
    strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
    panel_table <- weave_tables_col(panel_table, col_shift = placement, col_width = strip_pad)
  }


  panel_table
}

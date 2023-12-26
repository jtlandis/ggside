

sideFacetNull_draw_panels <- function(panels, layout, x_scales, y_scales,
                                      ranges, coord, data, theme, params) {


  if (params$ggside$strip!="default") {
    warn("`ggside(strip = 'main', ...)` is only compatible with `facet_grid(...)`",
         .frequency = "regularly",
         .frequency_id = "ggside_strip_misuse_null")
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
  ranges <- map_panel_type(ranges, layout$PANEL_TYPE)
  axes <- render_axes(ranges, ranges, coord, theme, transpose = TRUE)

  # If user hasn't set aspect ratio, and we have fixed scales, then
  # ask the coordinate system if it wants to specify one
  aspect_ratio <- theme$aspect.ratio
  if (is.null(aspect_ratio)) {
    aspect_ratio <- coord$aspect(ranges[[layout[layout$PANEL_TYPE=="main",]$PANEL[1L]]])
  }

  if (is.null(aspect_ratio)) {
    aspect_ratio <- 1
    respect <- FALSE
  } else {
    respect <- TRUE
  }
  #theme side panel scale
  side.panel.scale.x <- calc_element("ggside.panel.scale.x", theme)
  side.panel.scale.y <- calc_element("ggside.panel.scale.y", theme)

  empty_table <- matrix(list(zeroGrob()), nrow = nrow, ncol = ncol)
  panel_table <- empty_table
  panel_table[panel_pos] <- panels
  empties <- apply(panel_table, c(1,2), function(x) is.zero(x[[1]]))
  p.widths <- if("y"%in% side_panels_present) {
    .widths <- c(1, side.panel.scale.y)
    .tmp <- layout[layout[["PANEL_TYPE"]]=="y",][["COL"]]
    if(y.pos=="left"){
      .widths <- rev(.widths)
    }
    unit(rep(.widths, ncol/2), "null")
  } else {
    unit(rep(1, ncol), "null")
  }
  p.heights <- if("x"%in% side_panels_present) {
    .heights <- c(aspect_ratio, aspect_ratio*side.panel.scale.x)
    .tmp <- layout[layout[["PANEL_TYPE"]]=="x",][["ROW"]]
    if(x.pos=="top"){
      .heights <- rev(.heights)
    }
    unit(rep(abs(.heights), nrow/2), "null")
  } else {
    unit(rep(aspect_ratio, nrow), "null")
  }
  panel_table <- gtable_matrix("layout", panel_table,
                               widths = p.widths,
                               heights = p.heights, respect = respect,
                               clip = coord$clip, z = matrix(1, ncol = ncol, nrow = nrow))
  panel_table$layout$name <- paste0('panel-', rep(seq_len(ncol), nrow), '-', rep(seq_len(nrow), each = ncol))

  #need to register theme element
  sidepanel.spacing.x <- calc_element("ggside.panel.spacing.x", theme)
  xpanel_spacing <- calc_element("panel.spacing.x", theme)
  sidepanel.spacing.y <- calc_element("ggside.panel.spacing.y", theme)
  ypanel_spacing <- calc_element("panel.spacing.y", theme)
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

  .xgroupby <- "COL"
  .ygroupby <- "ROW"
  bottom <- do_by(layout, "COL", function(x, on){
    x[["ROW2"]] <- switch(on,
                          default = max(x[["ROW"]]),
                          main = max(x[["ROW"]][x[["PANEL_TYPE"]]!="x"]),
                          side = max(x[["ROW"]][x[["PANEL_TYPE"]]!="main"]))
    x}, on = params$ggside$draw_x_on)
  top <- do_by(layout, "COL", function(x, on){
    x[["ROW2"]] <- switch(on,
                          default = min(x[["ROW"]]),
                          main = min(x[["ROW"]][x[["PANEL_TYPE"]]!="x"]),
                          side = min(x[["ROW"]][x[["PANEL_TYPE"]]!="main"]))
    x}, on = params$ggside$draw_x_on)
  right <- do_by(layout, "ROW", function(x, on){
    x[["COL2"]] <- switch(on,
                          default = max(x[["COL"]]),
                          main = max(x[["COL"]][!x[["PANEL_TYPE"]]=="y"]),
                          side = max(x[["COL"]][!x[["PANEL_TYPE"]]=="main"]))
    x}, on = params$ggside$draw_y_on)
  left <- do_by(layout, "ROW", function(x, on){
    x[["COL2"]] <- switch(on,
                          default = min(x[["COL"]]),
                          main = min(x[["COL"]][!x[["PANEL_TYPE"]]=="y"]),
                          side = min(x[["COL"]][!x[["PANEL_TYPE"]]=="main"]))
    x}, on = params$ggside$draw_y_on)

  if(params$ggside$scales%in%c("free","free_y")){ #if y is free, include x PANELS_TYPES
    right <- right[right[["COL"]]==right[["COL2"]]|right[["PANEL_TYPE"]]=="x",]
    left <- left[left[["COL"]]==left[["COL2"]]|left[["PANEL_TYPE"]]=="x",]
  } else {
    right <- right[right[["COL"]]==right[["COL2"]],]
    left <- left[left[["COL"]]==left[["COL2"]],]
  }
  if(params$ggside$scales%in%c("free","free_x")){ #if x is free, include y PANELS_TYPES
    top <- top[top[["ROW"]]==top[["ROW2"]]|top[["PANEL_TYPE"]]=="y",]
    bottom <- bottom[bottom[["ROW"]]==bottom[["ROW2"]]|bottom[["PANEL_TYPE"]]=="y",]
  } else {
    top <- top[top[["ROW"]]==top[["ROW2"]],]
    bottom <- bottom[bottom[["ROW"]]==bottom[["ROW2"]],]
  }

  #top, left, bottom, right, variables includes panels that would have axis shown
  #for their relavent positions.
  #Do an anti_join against layout to find panels that should get a zeroGrobe
  bottom <- anti_join(layout, bottom, by = c("ROW","COL"))[["panel_pos"]]
  top <- anti_join(layout, top, by = c("ROW","COL"))[["panel_pos"]]
  right <- anti_join(layout, right, by = c("ROW","COL"))[["panel_pos"]]
  left <- anti_join(layout, left, by = c("ROW","COL"))[["panel_pos"]]
  #pulled panel positions
  #Place ZeroGrobs
  axis_mat_x_top[top]<- list(zeroGrob())
  axis_mat_x_bottom[bottom]<- list(zeroGrob())
  axis_mat_y_left[left] <- list(zeroGrob())
  axis_mat_y_right[right] <- list(zeroGrob())

  include <- seq_len(ncol)
  if (!params$ggside$respect_side_labels && all(c("x","y") %in% side_panels_present)) {
    #only view main panels top and bot
    shift <- 0L
    if (y.pos=="right") {
      shift <- 1L
    }
    include <- which((include %% 2) == shift)
  }

  axis_height_top <- unit(
    apply(axis_mat_x_top, 1, calc_max_height, value_only = TRUE, include = include),
    "cm"
  )
  axis_height_bottom <- unit(
    apply(axis_mat_x_bottom, 1, calc_max_height, value_only = TRUE, include = include),
    "cm"
  )

  include <- seq_len(nrow)
  if (!params$ggside$respect_side_labels && all(c("x","y") %in% side_panels_present)) {
    #only view main panels top and bot
    shift <- 0L
    if (x.pos=="bottom") {
      shift <- 1L
    }
    include <- which((include %% 2) == shift)
  }

  axis_width_left <- unit(
    apply(axis_mat_y_left, 2, calc_max_width, value_only = TRUE, include = include),
    "cm"
  )
  axis_width_right <- unit(
    apply(axis_mat_y_right, 2, calc_max_width, value_only = TRUE, include = include),
    "cm"
  )
  panel_table <- weave_tables_row(panel_table, axis_mat_x_top, -1, axis_height_top, "axis-t", 3)
  panel_table <- weave_tables_row(panel_table, axis_mat_x_bottom, 0, axis_height_bottom, "axis-b", 3)
  panel_table <- weave_tables_col(panel_table, axis_mat_y_left, -1, axis_width_left, "axis-l", 3)
  panel_table <- weave_tables_col(panel_table, axis_mat_y_right, 0, axis_width_right, "axis-r", 3)

  panel_table
}

calc_max_height <- function(grobs, value_only = FALSE, include = seq_along(grobs)) {
  width <- max(unlist(
    lapply(grobs[include], height_cm)
  ))
  if (!value_only)
    width <- unit(width, "cm")
  width
}

calc_max_width <- function(grobs, value_only = FALSE, include = seq_along(grobs)) {
  width <- max(unlist(
    lapply(grobs[include], width_cm)
  ))
  if (!value_only)
    width <- unit(width, "cm")
  width
}

width_cm <- function (x)
{
  if (is.grob(x)) {
    convertWidth(grobWidth(x), "cm", TRUE)
  }
  else if (is.unit(x)) {
    convertWidth(x, "cm", TRUE)
  }
  else if (is.list(x)) {
    vapply(x, width_cm, numeric(1))
  }
  else {
    cli::cli_abort("Don't know how to get width of {.cls {class(x)}} object")
  }
}

height_cm <- function (x)
{
  if (is.grob(x)) {
    convertHeight(grobHeight(x), "cm", TRUE)
  }
  else if (is.unit(x)) {
    convertHeight(x, "cm", TRUE)
  }
  else if (is.list(x)) {
    vapply(x, height_cm, numeric(1))
  }
  else {
    cli::cli_abort("Don't know how to get height of {.cls {class(x)}} object")
  }
}

sideFacetNull_map_data <- function (data, layout, params) {
  if (is.waive(data))
    return(new_data_frame(list(PANEL = factor())))
  if (empty(data))
    return(new_data_frame(c(data, list(PANEL = factor()))))

  if(!"PANEL_TYPE"%in%colnames(data)){
    data$PANEL_TYPE <- "main"
  }
  layout <- unwrap(layout, c("ROW","COL"), "FACET_VARS")
  keys <- join_keys(data, layout, by = "PANEL_TYPE")
  data[["PANEL"]] <- layout[["PANEL"]][match(keys$x, keys$y)]
  data
}

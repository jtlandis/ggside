#sideFacetWrap

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
  #browser()
  collapse <- params$ggside$collapse %||% "default"
  collapse_y <- collapse %in% c("all","y")
  collapse_x <- collapse %in% c("all","x")
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

  layout <- unwrap(layout, by = c("ROW","COL"),cols = "FACET_VARS")

  if (length(params$facets) == 0) {
    # Add a dummy label
    labels_df <- new_data_frame(list("(all)" = "(all)"), n = 1)
  } else {
    labels_df <- unique(layout[layout[["PANEL_TYPE"]]=="main",names(params$facets), drop = FALSE])
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
  #theme side panel scale
  side.panel.scale.x <- theme$ggside.panel.scale.x %||% theme$ggside.panel.scale %||% .1
  side.panel.scale.y <- theme$ggside.panel.scale.y %||% theme$ggside.panel.scale %||% .1

  empty_table <- matrix(list(zeroGrob()), nrow = nrow, ncol = ncol)
  panel_table <- empty_table
  panel_table[panel_pos] <- panels
  empties <- apply(panel_table, c(1,2), function(x) is.zero(x[[1]]))
  p.widths <- if("y"%in% side_panels_present) {
    if(collapse_y){
      .widths <- rep(1, ncol-1)
      .widths <- c(.widths, sum(.widths)*side.panel.scale.y)
      if(y.pos=="left"){
        .widths <- rev(.widths)
      }
      unit(.widths, "null")
    } else {
      .widths <- c(1, side.panel.scale.y)
      if(y.pos=="left"){
        .widths <- rev(.widths)
      }
      unit(rep(.widths, ncol/2), "null")
    }
  } else {
    unit(rep(1, ncol), "null")
  }
  p.heights <- if("x"%in% side_panels_present) {
    if(collapse_x){
      .heights <- rep(c(aspect_ratio), nrow-1)
      .heights <- c(.heights, sum(.heights)*side.panel.scale.x)
      if(x.pos=="top"){
        .heights <- rev(.heights)
      }
      unit(.heights, "null")
    } else {
      .heights <- c(aspect_ratio, aspect_ratio*side.panel.scale.x)
      if(x.pos=="top"){
        .heights <- rev(.heights)
      }
      unit(rep(abs(.heights), nrow/2), "null")
    }
  } else {
    unit(rep(aspect_ratio, nrow), "null")
  }
  panel_table <- gtable_matrix("layout", panel_table,
                               widths = p.widths,
                               heights = p.heights, respect = respect,
                               clip = coord$clip, z = matrix(1, ncol = ncol, nrow = nrow))
  panel_table$layout$name <- paste0('panel-', rep(seq_len(ncol), nrow), '-', rep(seq_len(nrow), each = ncol))

  #need to register theme element
  sidepanel.spacing <- theme$ggside.panel.spacing %||% theme$panel.spacing
  sidepanel.spacing.x <- theme$ggside.panel.spacing.x %||% sidepanel.spacing
  xpanel_spacing <- theme$panel.spacing.x %||% theme$panel.spacing
  sidepanel.spacing.y <- theme$ggside.panel.spacing.y %||% sidepanel.spacing
  ypanel_spacing <- theme$panel.spacing.y %||% theme$panel.spacing
  col.widths <- if("y"%in%side_panels_present){
    if(collapse_y){
      .tmp <- rep(c(xpanel_spacing), length(panel_table$widths)-2)
      if(y.pos=="left"){
        unit(c(sidepanel.spacing.x, .tmp), "pt")
      } else {
        unit(c(.tmp, sidepanel.spacing.x), "pt")
      }
    } else {
      unit(rep(c(sidepanel.spacing.x, xpanel_spacing), length.out = length(panel_table$widths)-1), "pt")
    }
  } else {
    xpanel_spacing
  }
  panel_table <- gtable_add_col_space(panel_table, col.widths)
  row.heights <- if("x"%in%side_panels_present){
    if(collapse_x){
      .tmp <- rep(c(ypanel_spacing), length(panel_table$heights)-2)
      if(x.pos=="top"){
        unit(c(sidepanel.spacing.y, .tmp), "pt")
      } else {
        unit(c(.tmp, sidepanel.spacing.y), "pt")
      }
    } else {
      unit(rep(c(sidepanel.spacing.y, ypanel_spacing), length.out = length(panel_table$heights)-1), "pt")
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

  .xgroupby <- if(!params$free$x|collapse_x) "COL" else c("COL","PANEL_GROUP")
  .y <- if("PANEL_GROUP" %in% .xgroupby) "y" else NULL
  .ygroupby <- if(!params$free$y|collapse_y) "ROW" else c("ROW","PANEL_GROUP")
  .x <- if("PANEL_GROUP" %in% .ygroupby) "x" else NULL

  bottom <- do_by(layout[!layout[["PANEL_TYPE"]]%in%.y,], .xgroupby,
                  function(x){x[["ROW2"]] <- max(x[["ROW"]]);x})
  top <- do_by(layout[!layout[["PANEL_TYPE"]]%in%.y,], .xgroupby,
               function(x){x[["ROW2"]] <- min(x[["ROW"]]);x})
  right <- do_by(layout[!layout[["PANEL_TYPE"]]%in%.x,], .ygroupby,
                 function(x){x[["COL2"]] <- max(x[["COL"]]);x})
  left <- do_by(layout[!layout[["PANEL_TYPE"]]%in%.x,], .ygroupby,
                function(x){x[["COL2"]] <- min(x[["COL"]]);x})

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

  if(all(c("x","y") %in% side_panels_present)){
    x_pos <- layout[layout[["PANEL_TYPE"]]=="x",][["panel_pos"]]
    if(y.pos=="left"){
      for(i in x_pos){
        axis_mat_y_left[i][[1]]$width <- NULL
      }
    } else {
      for(i in x_pos){
        axis_mat_y_right[i][[1]]$width <- NULL
      }
    }

    y_pos <- layout[layout[["PANEL_TYPE"]]=="y",][["panel_pos"]]
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

  params$strip.position <- params$strip.position %||% "top"
  if(any(c("top","bottom")%in%params$strip.position)){

    strip_panel_pos <- do_by(layout[!is.na(layout[["PANEL_GROUP"]]),], "PANEL_GROUP",
                             function(x, strip){
                               if("top"%in%strip){
                                 x[["ROW"]] <- min(x[["ROW"]])
                               } else {
                                 x[["ROW"]] <- max(x[["ROW"]])
                               }
                               x
                             }, strip = params$strip.position)
    strip_panel_pos <- semi_join(layout, strip_panel_pos, by = c("PANEL_GROUP","ROW"))
    strip_panel_pos <- unique(strip_panel_pos[!strip_panel_pos[["PANEL_TYPE"]]=="y",c("PANEL","panel_pos")])
  } else if(any(c("left","right")%in%params$strip.position)){
    strip_panel_pos <- do_by(layout[!is.na(layout[["PANEL_GROUP"]]),], "PANEL_GROUP",
                             function(x, strip){
                               if("left"%in%strip){
                                 x[["COL"]] <- min(x[["COL"]])
                               } else {
                                 x[["COL"]] <- max(x[["COL"]])
                               }
                               x
                             }, strip = params$strip.position)
    strip_panel_pos <- semi_join(layout, strip_panel_pos, by = c("PANEL_GROUP","COL"))
    strip_panel_pos <- unique(strip_panel_pos[!strip_panel_pos[["PANEL_TYPE"]]=="x",c("PANEL","panel_pos")])
  }


  strip_padding <- convertUnit(theme$strip.switch.pad.wrap, "cm")
  strip_name <- paste0("strip-", substr(params$strip.position, 1, 1))
  strip_mat <- empty_table
  strip_mat[strip_panel_pos$panel_pos] <- unlist(unname(strips), recursive = FALSE)[[params$strip.position]]
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

#' @rdname ggside-ggproto-facets
#' @usage NULL
#' @format NULL
#' @export
FacetSideWrap <- ggplot2::ggproto("FacetSideWrap",
                                  ggplot2::FacetWrap,
                                  compute_layout = function(data, params){
                                    layout <- ggplot2::FacetWrap$compute_layout(data, params)
                                    layout <- check_scales_collapse(layout, params)
                                    layout <- sidePanelLayout(layout, ggside = params$ggside)
                                    layout },
                                  map_data = map_data_ggside,
                                  draw_panels = sideFacetWrap_draw_panels
)


sideFacetNull_draw_panels <- function(panels, layout, x_scales, y_scales,
                                     ranges, coord, data, theme, params) {

  # if (inherits(coord, "CoordFlip")) {
  #   if (params$free$x) {
  #     layout$SCALE_X <- seq_len(nrow(layout))
  #   } else {
  #     layout$SCALE_X <- 1L
  #   }
  #     layout$SCALE_Y <- 1L
  #   }
  # }

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

  # If user hasn't set aspect ratio, and we have fixed scales, then
  # ask the coordinate system if it wants to specify one
  aspect_ratio <- theme$aspect.ratio
  if (is.null(aspect_ratio)) {
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
  sidepanel.spacing <- theme$ggside.panel.spacing %||% theme$panel.spacing
  sidepanel.spacing.x <- theme$ggside.panel.spacing.x %||% sidepanel.spacing
  xpanel_spacing <- theme$panel.spacing.x %||% theme$panel.spacing
  sidepanel.spacing.y <- theme$ggside.panel.spacing.y %||% sidepanel.spacing
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

  .xgroupby <- "COL"
  .ygroupby <- "ROW"

  bottom <- do_by(layout, "COL", function(x){x[["ROW2"]] <- max(x[["ROW"]]); x})
  top <- do_by(layout, "COL", function(x){x[["ROW2"]] <- min(x[["ROW"]]); x})
  right <- do_by(layout, "ROW", function(x){x[["COL2"]] <- max(x[["COL"]]); x})
  left <- do_by(layout, "ROW", function(x){x[["COL2"]] <- min(x[["COL"]]); x})

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

  panel_table
}

#' @rdname ggside-ggproto-facets
#' @usage NULL
#' @format NULL
#' @export
FacetSideNull <- ggplot2::ggproto("FacetSideNull",
                                  FacetNull,
                                  compute_layout = function(data, params){
                                    layout <- ggplot2::FacetNull$compute_layout(data, params)
                                    layout <- check_scales_collapse(layout, params)
                                    layout <- sidePanelLayout(layout, ggside = params$ggside)
                                    layout },
                                  init_scales = function(layout, x_scale = NULL, y_scale = NULL, params){
                                    scales <- FacetNull$init_scales(layout, x_scale, y_scale, params)
                                    if (!is.null(x_scale)&& !is.null(params$ggside$ysidex)){
                                      side_indx <-  layout[layout$PANEL_TYPE=="y",]$SCALE_X
                                      scales$x[side_indx] <- lapply(side_indx, function(i) params$ggside$ysidex$clone())

                                    }
                                    if (!is.null(y_scale)&& !is.null(params$ggside$xsidey)){
                                      side_indx <-  layout[layout$PANEL_TYPE=="x",]$SCALE_Y
                                      scales$y[side_indx] <- lapply(side_indx, function(i) params$ggside$xsidey$clone())

                                    }
                                    scales
                                  },
                                  map_data = map_data_ggside,
                                  draw_panels = sideFacetNull_draw_panels)

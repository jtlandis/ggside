### INCLUDE BEGIN
#' @include compat-plyr.R
#' @include utils-.R
#' @include performance.R
#' @include plot-construction.R
#' @include side-facet-ggplot_clones.R
NULL
### INCLUDE END


#'@rdname ggside-ggproto-facets
#'@description
#' `check_scales_collapse` is a helper function that
#' is meant to be called after the inherited Facet's
#' compute_layout method
#' @param data data passed through ggproto object
#' @param params parameters passed through ggproto object
#' @export
check_scales_collapse <- function(data, params) {
  collapse <- params$ggside$collapse %||% "default"
  if(collapse %in%c("all","x")){
    checkX <- unlist(
      lapply(
        split(data[["SCALE_X"]],
              data[["COL"]]),
        function(x) length(unique(x))
      )
    )
    if(!all(checkX==1)){
      warn(glue("free x scales is not compatible with collapse {collapse}. Assigning new x scales."))
      data[["SCALE_X"]] <- data[["COL"]]
    }
  }
  if(collapse %in%c("all","y")){
    checkY <- unlist(
      lapply(
        split(data[["SCALE_Y"]],
              data[["ROW"]]),
        function(x) length(unique(x))
      )
    )
    if(!all(checkY==1)){
      warn(glue("free y scales is not compatible with collapse {collapse}. Assigning new y scales."))
      data[["SCALE_Y"]] <- data[["ROW"]]
    }
  }
  data
}

#'@rdname ggside-ggproto-facets
#'@description
#' `sidePanelLayout` is a helper function that
#' is meant to be called after the inherited Facet's
#' compute_layout method and after `check_scales_collapse`
#' @param layout layout computed by inherited ggproto Facet compute_layout method
#' @export
sidePanelLayout <- function(layout,
                            ggside){
  ggside$collapse <- check_collapse(ggside$collapse, ggside$sides_used)
  facet_vars <- setdiff(colnames(layout), c("PANEL","ROW","COL","SCALE_X","SCALE_Y","PANEL_GROUP","PANEL_TYPE"))
  x.pos = ggside$x.pos
  y.pos = ggside$y.pos
  scales = ggside$scales
  collapse <- ggside$collapse %||% "default" #default is no collapsing
  sidePanel <- ggside$sides_used
  if(collapse%in%c("all","x")){
    xrow <- x.pos
    mrow <- "ALL"
    yrow <- mrow
  } else if("x"%in%sidePanel){
    xrow <- ifelse(x.pos=="top","ODD","EVEN")
    mrow <- ifelse(xrow=="EVEN","ODD","EVEN")
    yrow <- mrow
  } else {
    xrow <- "ALL"
    mrow <- "ALL"
    yrow <- "ALL"
  }

  if(collapse%in%c("all","y")){
    ycol <- y.pos
    mcol <- "ALL"
    xcol <- mcol
  } else if("y"%in%sidePanel){
    ycol <- ifelse(y.pos=="right","EVEN", "ODD")
    mcol <- ifelse(ycol=="EVEN","ODD","EVEN")
    xcol <- mcol
  } else {
    ycol <- "ALL"
    mcol <- "ALL"
    xcol <- "ALL"
  }

  data <- data_frame(PANEL_TYPE = factor(c("main", "x", "y")),
                     ROW_trans = c(mrow,xrow,yrow),
                     COL_trans = c(mcol,xcol,ycol))
  data <- data[data$PANEL_TYPE %in% c("main", sidePanel),]
  include <- switch(collapse, x = c("main","y"), y = c("main","x"), all = c("main"), c("main","x","y"))
  collapsed <- data[!data$PANEL_TYPE %in% include,]
  data <- data[data$PANEL_TYPE %in% include,]
  x_scale_fun <- switch(scales,
                        free_x = free_fun,
                        free = free_fun,
                        fixed_fun)
  y_scale_fun <- switch(scales,
                        free_y = free_fun,
                        free = free_fun,
                        fixed_fun)
  layout$PANEL_GROUP <- layout$PANEL
  layout <- cbind.data.frame(layout[rep(1:nrow(layout), each = nrow(data)),],
                             data[rep(1:nrow(data), nrow(layout)),])

  #transform ROW and COL
  layout[["ROW"]] <- layout[["ROW"]]*ifelse(layout[["ROW_trans"]]=="ALL",1L,2L) - ifelse(layout[["ROW_trans"]]=="ODD",1L,0L)
  layout[["COL"]] <- layout[["COL"]]*ifelse(layout[["COL_trans"]]=="ALL",1L,2L) - ifelse(layout[["COL_trans"]]=="ODD",1L,0L)

  if(!empty(collapsed)){

    if(collapse %in% c("all","x")){
      x_collapse <- unique(layout[layout$PANEL_TYPE %in% "main",
                                  c("COL","ROW","PANEL_TYPE",
                                    "SCALE_X","SCALE_Y",
                                    "ROW_trans","COL_trans",
                                    facet_vars)])
      x_collapse$ROW <- 0
      x_collapse$PANEL_TYPE <- factor("x", levels = levels(layout$PANEL_TYPE))
      x_collapse$SCALE_Y <- 0
      x_collapse[,c("ROW_trans","COL_trans")] <- collapsed[collapsed$PANEL_TYPE%in%"x",
                                                           c("ROW_trans","COL_trans")]
      layout <- vec_rbind(layout, x_collapse)
      if(x.pos=="bottom"){
        layout[["ROW"]] <- layout[["ROW"]] + ifelse(layout[["ROW_trans"]]=="bottom", max(layout[["ROW"]])+1, 0L)
      } else {
        layout[["ROW"]] <- layout[["ROW"]] + 1L
      }
      #Need to do something with scales on a collapse...
    }

    if(collapse %in% c("all","y")){
      y_collapse <- unique(layout[layout$PANEL_TYPE %in% "main",
                                  c("COL","ROW","PANEL_TYPE",
                                    "SCALE_X","SCALE_Y",
                                    "ROW_trans","COL_trans",
                                    facet_vars)])
      y_collapse$COL <- 0
      y_collapse$PANEL_TYPE <- factor("y", levels = levels(layout$PANEL_TYPE))
      y_collapse$SCALE_X <- 0
      y_collapse[,c("ROW_trans","COL_trans")] <- collapsed[collapsed$PANEL_TYPE%in%"y",
                                                           c("ROW_trans","COL_trans")]
      layout <- vec_rbind(layout, y_collapse)
      if(y.pos=="right"){
        layout[["COL"]] <- layout[["COL"]] + ifelse(layout[["COL_trans"]]=="right", max(layout[["COL"]])+1, 0L)
      } else {
        layout[["COL"]] <- layout[["COL"]] + 1L
      }
    }

  }
  .pty <- layout[["PANEL_TYPE"]]=="y"
  layout[["SCALE_X"]][.pty] <-  x_scale_fun(layout[["SCALE_X"]], .pty, interaction(layout[["ROW"]], layout[["COL"]], lex.order = TRUE))
  .ptx <- layout[["PANEL_TYPE"]]=="x"
  layout[["SCALE_Y"]][.ptx] <-  y_scale_fun(layout[["SCALE_Y"]], .ptx, interaction(layout[["ROW"]], layout[["COL"]], lex.order = TRUE))
  layout <- layout[,setdiff(colnames(layout), c("ROW_trans","COL_trans","PANEL"))]
  layout <- unique(layout)
  layout <- layout[order(layout$ROW, layout$COL),]
  layout <- wrapup(layout, by = c("ROW","COL"), FACET_VARS = facet_vars)
  layout$PANEL <- factor(1:nrow(layout))
  return(layout)
}



fixed_fun <- function(x, lgl, indx){
  rep(max(x)+1L,sum(lgl))
}

free_fun <- function(x, lgl, indx){
  ind <- indx[lgl]
  uindx <- unique(ind)
  scale <- seq_along(uindx)
  max(x) + scale[match(ind, uindx)]
}

max_factor <- function(x){
  lvl <- levels(x)
  max_ <- lvl[max(which(lvl%in%x))]
  unique(x[x%in%max_])
}
min_factor <- function(x){
  lvl <- levels(x)
  min_ <- lvl[min(which(lvl%in%x))]
  unique(x[x%in%min_])
}



wrapup <- function(df, by, ...){
  if(...length()==0) return(df)
  indx <- interaction(df[,by], drop = T)
  indx <- match(indx, unique(indx))
  dots_ <- list(...)
  if(!all(unlist(lapply(dots_, function(x,y){all(x%in%y)}, y = colnames(df))))) abort("all RHS must exist in column names of `df`.")
  wrap_columns <- unlist(dots_)
  l_ <- split(df, indx)
  l_ <- lapply(l_, function(x, d){
    wrap <- lapply(d, function(y) list(x[,y, drop = FALSE]))
    x <- unique(x[,setdiff(colnames(x), wrap_columns), drop = FALSE])
    x[,names(d)] <- wrap
    x
  }, d = dots_)
  data <- vec_rbind(!!!l_)
  data
}

unwrap <- function(df, by, cols = NULL){
  if(is.null(cols)) return(df)
  if(!all(cols%in%colnames(df))) abort("all `cols` must exist in column names of `df`")
  indx <- interaction(df[,by], drop = T)
  indx <- match(indx, unique(indx))
  l_ <- split(df, indx)
  l_ <- lapply(l_, function(x){
    nest <- do.call('cbind',unlist(Map(function(d, y) {d[,y,drop=T]}, d = list(x), y = cols),recursive = F))
    x <- x[, setdiff(colnames(x), cols), drop = FALSE]
    if(nrow(x)!=1) stop("by must uniquely index df")
    cbind(x[rep(1, nrow(nest)),], nest)
  })
  data <- vec_rbind(!!!l_)
  data
}


map_panel_type <- function(panel_params, panel_types) {
  mapply(function(x, y) {x$ggside_panel_type <- y; x}, x = panel_params, y = panel_types, SIMPLIFY = F)
}

calc_panel_spacing <- function(ggside, layout, top, right, bot, left) {

  respect <- ggside$respect_side_labels
  y.pos <- ggside$y.pos
  x.pos <- ggside$x.pos
  xside <- "x" %in% layout$PANEL_TYPE
  yside <- "y" %in% layout$PANEL_TYPE
  n_row <- max(layout$ROW)
  collapse <- ggside$collapse
  collapsed <- !is.null(collapse)

  top_height <- vapply(top, height_cm, numeric(1))
  right_width <- vapply(right, width_cm, numeric(1))
  bot_height <- vapply(bot, height_cm, numeric(1))
  left_width <- vapply(left, width_cm, numeric(1))

  xsub <- layout[layout$PANEL_TYPE=="x",]
  ysub <- layout[layout$PANEL_TYPE=="y",]
  xside_panels <- xsub$panel_pos
  yside_panels <- ysub$panel_pos

  if (respect=="default" && xside && yside) {
    #heights
    if (y.pos=="left") {
      left_width[xside_panels] <- 0
    } else {
      right_width[xside_panels] <- 0
    }

    #widths
    if (x.pos=="top") {
      top_height[yside_panels] <- 0
    } else {
      bot_height[yside_panels] <- 0
    }
  } else if (respect == "independent" && xside && yside) {
    #heights
    if (y.pos=="left") {
      if (collapsed && collapse %in% c("y", "all")) {
        left_width[tapply(xsub$panel_pos, xsub$ROW, min)] <- 0
      } else {
        left_width[xside_panels] <- 0
      }
    } else {
      if (collapsed && collapse %in% c("y", "all")) {
        left_width[tapply(xsub$panel_pos, xsub$ROW, max)] <- 0
      } else {
        right_width[xside_panels] <- 0
      }
    }

    #widths
    if (x.pos=="top") {
      if (collapsed && collapse %in% c("x", "all")) {
        top_height[tapply(ysub$panel_pos, ysub$COL, min)]
      } else {
        top_height[yside_panels] <- 0
      }
    } else {
      if (collapsed && collapse %in% c("x", "all")) {
        #only set bottom y panel to 0
        bot_height[tapply(ysub$panel_pos, ysub$COL, max)] <- 0
      } else {
        bot_height[yside_panels] <- 0
      }
    }

  } else {
    if (respect %in% c("x", "none") && yside) {
      bot_height[yside_panels] <- top_height[yside_panels] <- 0
    }
    if (respect %in% c("y", "none") && xside) {
      left_width[xside_panels] <- right_width[xside_panels] <- 0
    }
  }

  list(
    top = unit(apply(matrix(top_height, nrow = n_row), 1, max), "cm"),
    right = unit(apply(matrix(right_width, nrow = n_row), 2, max), "cm"),
    bot = unit(apply(matrix(bot_height, nrow = n_row), 1, max), "cm"),
    left = unit(apply(matrix(left_width, nrow = n_row), 2, max), "cm")
  )

}

do_by <- function(data, by, fun, ...){
  order_cache <- do.call('order', lapply(by, function(x){data[[x]]}))
  data <- data[order_cache,]
  split_by <- interaction(data[,by, drop = F], drop = T, lex.order = T)
  data <- vec_rbind(!!!lapply(split(data, split_by), FUN = fun, ...))
  data <- data[order(order_cache),]
  rownames(data) <- seq_len(nrow(data))
  data
}

anti_join <- function(x, y, by) {
  keys <- join_keys(x, y, by)
  x[!keys$x%in%keys$y,]
}
semi_join <- function(x, y, by) {
  keys <- join_keys(x, y, by)
  x[keys$x%in%keys$y,]
}


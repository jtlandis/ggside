
fixed_fun <- function(x, lgl){
  rep(max(x)+1L,sum(lgl))
}

free_fun <- function(x, lgl){
  max(x)+(seq_len(sum(lgl)))
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
  data <- rbind_dfs(l_)
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
  data <- rbind_dfs(l_)
  data
}

#'@rdname ggside-ggproto-facets
#'@description
#' S3 class that converts old Facet into one that
#' is compatible with ggside. Can also update
#' ggside on the object. Typically, the new ggproto
#' will inherit from the object being replaced.
#' @param facet Facet ggproto Object to replace
#' @param ggside ggside object to update
#'@export
as_ggsideFacet <- function(facet, ggside) UseMethod("as_ggsideFacet")
as_ggsideFacet.default <- function(facet, ggside){
  abort(glue("No known method to make {class(facet)[1]} ggside friendly"))
}
as_ggsideFacet.FacetNull <- function(facet, ggside){
  facet$params[["ggside"]] <- ggside
  ggplot2::ggproto(NULL,
                   FacetSideNull,
                   params = facet$params,
                   shrink = facet$shrink)
}
as_ggsideFacet.FacetGrid <- function(facet, ggside){
  facet$params[["ggside"]] <- ggside
  ggplot2::ggproto(NULL,
                   FacetSideGrid,
                   params = facet$params,
                   shrink = facet$shrink)
}
as_ggsideFacet.FacetWrap <- function(facet, ggside){
  facet$params[["ggside"]] <- ggside
  ggplot2::ggproto(NULL,
                   FacetSideWrap,
                   params = facet$params,
                   shrink = facet$shrink)
}

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
      layout <- rbind_dfs(list(layout, x_collapse))
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
      layout <- rbind_dfs(list(layout, y_collapse))
      if(y.pos=="right"){
        layout[["COL"]] <- layout[["COL"]] + ifelse(layout[["COL_trans"]]=="right", max(layout[["COL"]])+1, 0L)
      } else {
        layout[["COL"]] <- layout[["COL"]] + 1L
      }
    }

  }
  .pty <- layout[["PANEL_TYPE"]]=="y"
  layout[["SCALE_X"]][.pty] <-  x_scale_fun(layout[["SCALE_X"]],.pty)
  .ptx <- layout[["PANEL_TYPE"]]=="x"
  layout[["SCALE_Y"]][.ptx] <-  y_scale_fun(layout[["SCALE_Y"]], .ptx)
  layout <- layout[,setdiff(colnames(layout), c("ROW_trans","COL_trans","PANEL"))]
  layout <- unique(layout)
  layout <- layout[order(layout$ROW, layout$COL),]
  layout <- wrapup(layout, by = c("ROW","COL"), FACET_VARS = facet_vars)
  layout$PANEL <- factor(1:nrow(layout))
  return(layout)
}

#'@rdname ggside-ggproto-facets
#'@description
#' `map_data_ggside` is the mapping function
#' used to replace all map_data method on [FacetSideNull],
#' [FacetSideGrid], and [FacetSideWrap]. It is exported
#' for conviences of extensibility.
#'@export
map_data_ggside <- function(data, layout, params){
  if (is.waive(data))
    return(new_data_frame(list(PANEL = factor())))

  if (empty(data))
    return(new_data_frame(c(data, list(PANEL = factor()))))

  facet_vars <- c(names(params$facets),names(params$rows),names(params$cols))
  if(!"PANEL_TYPE"%in%colnames(data)){
    data$PANEL_TYPE <- "main"
  }
  layout <- unwrap(layout, c("ROW","COL"), "FACET_VARS")
  keys <- join_keys(data, layout, by = c("PANEL_TYPE",facet_vars))
  data[["PANEL"]] <- layout[["PANEL"]][match(keys$x, keys$y)]
  data
}

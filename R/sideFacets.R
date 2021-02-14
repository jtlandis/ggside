
fixed_fun <- function(x){
  max(x)+1L
}

free_fun <- function(x){
  max(x)+(1L:length(x))
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

#' @importFrom dplyr case_when
sidePanelLayout <- function(layout,
                            ggside){

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
  layout <- bind_cols(layout[rep(1:nrow(layout), each = nrow(data)),],
                      data[rep(1:nrow(data), nrow(layout)),])
  layout$ROW <- case_when(layout$ROW_trans=="EVEN" ~ layout$ROW*2L,
                          layout$ROW_trans=="ODD"  ~ layout$ROW*2L-1L,
                          layout$ROW_trans=="ALL"  ~ layout$ROW)
  layout$COL <- case_when(layout$COL_trans=="EVEN" ~ layout$COL*2L,
                          layout$COL_trans=="ODD"  ~ layout$COL*2L-1L,
                          layout$COL_trans=="ALL"  ~ layout$COL)

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
      layout <- bind_rows(layout, x_collapse)
      if(x.pos=="bottom"){
        layout$ROW <- case_when(layout$ROW_trans=="bottom" ~ max(layout$ROW)+1L,
                                TRUE ~ layout$ROW)
      } else {
        layout$ROW <- layout$ROW + 1L
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
      layout <- bind_rows(layout, y_collapse)
      if(y.pos=="right"){
        layout$COL <- case_when(layout$COL_trans=="right" ~ max(layout$COL)+1L,
                                TRUE ~ layout$COL)
      } else {
        layout$COL <- layout$COL + 1L
      }
    }

  }
  layout$SCALE_X <- case_when(layout$PANEL_TYPE=="y" ~ x_scale_fun(layout$SCALE_X),
                              TRUE ~ layout$SCALE_X)
  layout$SCALE_Y <- case_when(layout$PANEL_TYPE=="x" ~ y_scale_fun(layout$SCALE_Y),
                              TRUE ~ layout$SCALE_Y)
  layout <- layout[,setdiff(colnames(layout), c("ROW_trans","COL_trans","PANEL"))]
  layout <- unique(layout)
  layout <- layout[order(layout$ROW, layout$COL),]
  layout <- wrapup(layout, by = c("ROW","COL"), FACET_VARS = facet_vars)
  layout$PANEL <- factor(1:nrow(layout))
  return(layout)
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

sideFacetDraw <- function(facet){
  UseMethod("sideFacetDraw")
}

sideFacetDraw.FacetWrap <- function(facet){
  sideFacetWrap_draw_panels
}

sideFacetDraw.FacetGrid <- function(facet){
  sideFacetGrid_draw_panels
}

sideFacetDraw.FacetNull <- function(facet){
  sideFacetNull_draw_panels
}

get_Facet <- function(facet){
  UseMethod("get_Facet")
}

get_Facet.default <- function(facet){
  abort(glue("No method implimented for facet of class {class(facet)[1]}"))
}

get_Facet.FacetNull <- function(facet) ggplot2::FacetNull

get_Facet.FacetGrid <- function(facet) ggplot2::FacetGrid

get_Facet.FacetWrap <- function(facet) ggplot2::FacetWrap


make_sideFacets <- function(facet, ggside) UseMethod("make_sideFacets")


make_sideFacets.default <- function(facet, ggside){

  base_facet <- get_Facet(facet)
  sideFacet_draw_panels <- sideFacetDraw(base_facet)

  ggproto(NULL,
          base_facet,
          params = facet$params,
          setup_params = function(data, params){
            params$.possible_columns <- unique(unlist(lapply(data, names)))
            params$ggside <- ggside
            params
          },
          compute_layout = function(data, params,
                                    facet_compute = base_facet$compute_layout){
            layout <- facet_compute(data, params)
            layout <- check_scales_collapse(layout, params)
            layout <- sidePanelLayout(layout, ggside = params$ggside)
            layout },
          map_data = function(data, layout,
                              params, facet_mapping = facet$map_data){
            if (ggplot2:::is.waive(data))
              return(new_data_frame(list(PANEL = factor())))

            if (ggplot2:::empty(data))
              return(ggplot2:::new_data_frame(c(data, list(PANEL = factor()))))

            facet_vars <- c(names(params$facets),names(params$rows),names(params$cols))
            if(!"PANEL_TYPE"%in%colnames(data)){
              data$PANEL_TYPE <- "main"
            }
            layout <- unwrap(layout, c("ROW","COL"), "FACET_VARS")
            .x <- interaction(data[,c("PANEL_TYPE",facet_vars)])
            .y <- interaction(layout[,c("PANEL_TYPE",facet_vars)])
            data <- cbind.data.frame(data, layout[match(.x,.y),"PANEL", drop = FALSE])

            data
          },
          draw_panels = sideFacet_draw_panels

  )
}

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


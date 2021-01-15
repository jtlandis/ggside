
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
                                    ggside,
                                    sidePanel = c("x","y")){
  browser()
  facet_vars <- setdiff(colnames(layout), c("PANEL","ROW","COL","SCALE_X","SCALE_Y","PANEL_GROUP","PANEL_TYPE"))
  x.pos = ggside$x.pos
  y.pos = ggside$y.pos
  scales = ggside$scales
  collapse <- ggside$collapse %||% "default" #default is no collapsing
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

  data <- data.frame(PANEL_TYPE = c("main", "x", "y"),
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

    if(!"x"%in% include){
      .tmp <- layout[layout$PANEL_TYPE %in% "main",]
      .tmp <- layout %>% filter(PANEL_TYPE %in% "main") %>%
        group_by(COL)
      .tmp1 <- .tmp %>% select(all_of(c("COL", facet_vars))) %>% distinct()
      .tmp <- .tmp %>%
        summarise(ROW = 0,
                  #PANEL_GROUP = case_when(x.pos=="bottom" ~ max_factor(PANEL_GROUP), TRUE ~ min_factor(PANEL_GROUP)),
                  PANEL_TYPE = factor("x", levels = levels(layout$PANEL_TYPE)),
                  SCALE_X = unique(SCALE_X),
                  SCALE_Y = 0) %>%
        left_join(x = . , y = collapsed, by = c("PANEL_TYPE")) %>%
        left_join(x = ., y = .tmp1, by = "COL")
      layout <- bind_rows(layout, .tmp)
      if(x.pos=="bottom"){
        layout <- mutate(layout, ROW = case_when(ROW_trans=="bottom" ~ max(ROW)+1L, TRUE ~ ROW))
      } else {
        layout <- mutate(layout, ROW = ROW + 1L)
      }
      #Need to do something with scales on a collapse...
    }

    if(!"y"%in% include){
      .tmp <- layout%>% filter(PANEL_TYPE %in% "main") %>%
        group_by(ROW)
      .tmp1 <- .tmp %>% select(all_of(c("ROW", facet_vars))) %>% distinct()
      .tmp <- .tmp %>%
        summarise(COL = 0,
                 # PANEL_GROUP = case_when(y.pos=="left" ~ max_factor(PANEL_GROUP), TRUE ~ min_factor(PANEL_GROUP)),
                  PANEL_TYPE = factor("y", levels = levels(layout$PANEL_TYPE)),
                  SCALE_X = 0,
                  SCALE_Y = unique(SCALE_Y)) %>%
        left_join(x = ., y = collapsed, by = c("PANEL_TYPE")) %>%
        left_join(x = ., y = .tmp1, by = "ROW")
      layout <- bind_rows(layout, .tmp)
      if(y.pos=="right"){
        layout <- mutate(layout, COL = case_when(COL_trans=="right" ~ max(COL)+1L, TRUE ~ COL))
      } else {
        layout <- mutate(layout, COL = COL + 1L)
      }
    }

  }
  #browser()
  layout$SCALE_X <- case_when(layout$PANEL_TYPE=="y" ~ x_scale_fun(layout$SCALE_X),
                              TRUE ~ layout$SCALE_X)
  layout$SCALE_Y <- case_when(layout$PANEL_TYPE=="x" ~ y_scale_fun(layout$SCALE_Y),
                              TRUE ~ layout$SCALE_Y)
  layout <- layout[,setdiff(colnames(layout), c("ROW_trans","COL_trans","PANEL"))]
  layout <- unique(layout)
  layout <- layout[order(layout$ROW, layout$COL),]
  #i_ <- interaction(layout[,c("ROW","COL")]) ; factor(match(i_, unique(i_)))
  layout$PANEL <- factor(1:nrow(layout))
  return(layout)
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

#' @export
make_sideFacets <- function(facet, ggside, sides = c("x","y")){

  sideFacet_draw_panels <- sideFacetDraw(facet)

  ggproto(NULL,
          facet,
          setup_params = function(data, params){
            params$.possible_columns <- unique(unlist(lapply(data, names)))
            params$ggside <- ggside
            params
          },
          compute_layout = function(data, params,
                                    facet_compute = facet$compute_layout){
            #browser()
            collapse <- params$ggside$collapse %||% "default"
            layout <- facet_compute(data, params)
            if(collapse %in%c("all","x")){
              checkX <- layout %>% group_by(COL) %>%
                summarise(SCALE_X = list(unique(SCALE_X))) %>%
                pull(SCALE_X) %>% lapply(FUN = length) %>% unlist()
              if(!all(checkX==1)){
                warn(glue("free x scales is not compatible with collapse {collapse}. Assigning new x scales"))
                layout <- mutate(layout, SCALE_X = COL)
              }
            }
            if(collapse %in%c("all","y")){
              checkY <- layout %>% group_by(ROW) %>%
                summarise(SCALE_Y = list(unique(SCALE_Y))) %>%
                pull(SCALE_Y) %>% lapply(FUN = length) %>% unlist()
              if(!all(checkY==1)){
                warn(glue("free y scales is not compatible with collapse {collapse}. Assigning new y scales"))
                layout <- mutate(layout, SCALE_Y = ROW)
              }
            }
            layout <- sidePanelLayout(layout, sidePanel = sides, ggside = params$ggside)
            layout },
          map_data = function(data, layout,
                              params, facet_mapping = facet$map_data){
            #browser()
            facet_vars <- c(names(params$facets),names(params$rows),names(params$cols))
            # layout <- layout %>% unnest(c(facet_vars))
            # data <- unnest(data, PANEL_TYPE)
            .x <- interaction(data[,c("PANEL_TYPE",facet_vars)])
            .y <- interaction(layout[,c("PANEL_TYPE",facet_vars)])
            data <- cbind.data.frame(data, layout[match(.x,.y),"PANEL", drop = FALSE])
            # if(is.null(facet_vars)){
            #   panels <- layout %>% mutate(PANEL_TYPE = as.character(PANEL_TYPE)) %>%
            #     group_by(PANEL_TYPE) %>%
            #     summarise(PANEL = list(unique(PANEL)))
            #   data <- left_join(data, panels, by = c("PANEL_TYPE")) %>%
            #     tidyr::unnest(PANEL)
            # } else {
            #   panels <- layout %>% mutate(PANEL_TYPE = as.character(PANEL_TYPE)) %>%
            #     group_by(PANEL_TYPE, .dots = facet_vars) %>%
            #     summarise(PANEL = list(unique(PANEL)))
            #   data <- left_join(data, panels, by = c(facet_vars, "PANEL_TYPE")) %>%
            #     tidyr::unnest(PANEL)
            # }

            data
          },
          draw_panels = sideFacet_draw_panels

  )
}



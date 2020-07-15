
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

sidePanelLayout <- function(layout,
                                    ggside,
                                    sidePanel = c("x","y")){
  browser()
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
                     COL_trans = c(mcol,xcol,ycol)) %>%
    filter(PANEL_TYPE %in% c("main",sidePanel))
  include <- switch(collapse, x = c("main","y"), y = c("main","x"), all = c("main"), c("main","x","y"))
  collapsed <- filter(data, !PANEL_TYPE %in% include)
  data <- filter(data, PANEL_TYPE %in% include)
  x_scale_fun <- switch(scales,
                        free_x = free_fun,
                        free = free_fun,
                        fixed_fun)
  y_scale_fun <- switch(scales,
                        free_y = free_fun,
                        free = free_fun,
                        fixed_fun)
  layout <- layout %>%
    mutate(PANEL_GROUP = PANEL) %>%
    mutate(PANEL_TYPE = list(data$PANEL_TYPE),
           ROW_trans = list(data$ROW_trans),
           COL_trans = list(data$COL_trans)) %>%
    unnest(cols = c(PANEL_TYPE, ROW_trans, COL_trans)) %>%
    mutate(ROW = case_when(ROW_trans=="EVEN" ~ ROW*2L,
                           ROW_trans=="ODD" ~ ROW*2L-1L,
                           ROW_trans=="ALL" ~ ROW),
           COL = case_when(COL_trans=="EVEN" ~ COL*2L,
                           COL_trans=="ODD" ~ COL*2L-1L,
                           COL_trans=="ALL" ~ COL))
  if(!empty(collapsed)){

    if(!"x"%in% include){
      .tmp <- layout %>% filter(PANEL_TYPE %in% "main") %>%
        group_by(COL) %>%
        summarise(ROW = 0,
                  PANEL_GROUP = case_when(x.pos=="bottom" ~ max_factor(PANEL_GROUP), TRUE ~ min_factor(PANEL_GROUP)),
                  PANEL_TYPE = factor("x", levels = levels(layout$PANEL_TYPE)),
                  SCALE_X = unique(SCALE_X),
                  SCALE_Y = 0) %>%
        left_join(x = ., y = collapsed, by = c("PANEL_TYPE"))
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
        group_by(ROW) %>%
        summarise(COL = 0,
                  PANEL_GROUP = case_when(y.pos=="left" ~ max_factor(PANEL_GROUP), TRUE ~ min_factor(PANEL_GROUP)),
                  PANEL_TYPE = factor("y", levels = levels(layout$PANEL_TYPE)),
                  SCALE_X = 0,
                  SCALE_Y = unique(SCALE_Y)) %>%
        left_join(x = ., y = collapsed, by = c("PANEL_TYPE"))
      layout <- bind_rows(layout, .tmp)
      if(y.pos=="right"){
        layout <- mutate(layout, COL = case_when(COL_trans=="right" ~ max(COL)+1L, TRUE ~ COL))
      } else {
        layout <- mutate(layout, COL = COL + 1L)
      }
    }

  }
  layout <- layout %>%
    mutate(SCALE_X = case_when(PANEL_TYPE=="y" ~ x_scale_fun(SCALE_X),
                               TRUE ~ SCALE_X),
           SCALE_Y = case_when(PANEL_TYPE=="x" ~ y_scale_fun(SCALE_Y),
                               TRUE ~ SCALE_Y)) %>%
    arrange(ROW, COL) %>%
    mutate(PANEL = factor(1:n())) %>%
    select(-ROW_trans, -COL_trans)
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
                warn("free x scales is not compatible with collapse {collapse}. Assigning new x scales")
                layout <- mutate(layout, SCALE_X = COL)
              }
            }
            if(collapse %in%c("all","y")){
              checkY <- layout %>% group_by(ROW) %>%
                summarise(SCALE_Y = list(unique(SCALE_Y))) %>%
                pull(SCALE_Y) %>% lapply(FUN = length) %>% unlist()
              if(!all(checkY==1)){
                warn("free y scales is not compatible with collapse {collapse}. Assigning new y scales")
                layout <- mutate(layout, SCALE_Y = ROW)
              }
            }
            layout <- sidePanelLayout(layout, sidePanel = sides, ggside = ggside)
            layout },
          map_data = function(data, layout,
                              params, facet_mapping = facet$map_data){
            browser()
            facet_vars <- c(names(params$facets),names(params$rows),names(params$cols))
            collapsedSet <- layout %>% filter_at(vars(matches(facet_vars)), any_vars(is.na(.)))
            layout <- layout %>% filter_at(vars(matches(facet_vars)), any_vars(!is.na(.)))
            data <- unnest(data, PANEL_TYPE)
            .data <- data
            if(is.null(facet_vars)){
              panels <- layout %>% mutate(PANEL_TYPE = as.character(PANEL_TYPE)) %>%
                group_by(PANEL_TYPE) %>%
                summarise(PANEL = list(unique(PANEL)))
              data <- left_join(data, panels, by = c("PANEL_TYPE")) %>%
                tidyr::unnest(PANEL)
            } else {
              panels <- layout %>% mutate(PANEL_TYPE = as.character(PANEL_TYPE)) %>%
                group_by(PANEL_TYPE, .dots = facet_vars) %>%
                summarise(PANEL = list(unique(PANEL)))
              data <- left_join(data, panels, by = c(facet_vars, "PANEL_TYPE")) %>%
                tidyr::unnest(PANEL)
            }

            data %>%
              filter(!PANEL_TYPE %in% "empty")
          },
          draw_panels = sideFacet_draw_panels

  )
}



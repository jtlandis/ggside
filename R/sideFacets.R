
fixed_fun <- function(x){
  max(x)+1L
}

free_fun <- function(x){
  max(x)+(1L:length(x))
}

sidePanelLayout <- function(layout,
                            ggside,
                            sidePanel = c("x","y")){
  #browser()
  x.pos = ggside$x.pos
  y.pos = ggside$y.pos
  scales = ggside$scales
  xrow <- ifelse(x.pos=="top","ODD","EVEN")
  mrow <- ifelse(xrow=="EVEN","ODD","EVEN")
  ycol <- ifelse(y.pos=="right","EVEN", "ODD")
  mcol <- ifelse(ycol=="EVEN","ODD","EVEN")
  if( all(c("x","y")%in% sidePanel)){
    xcol <- xrow
    yrow <- ycol
  } else {
    xcol <- "ALL"
    yrow <- "ALL"
    if("x"%in%sidePanel){
      mcol <- "ALL"
    } else {
      mrow <- "ALL"
    }
  }
  data <- data.frame(PANEL_TYPE = c("main", "x", "y"),
                     ROW_trans = c(mrow,xrow,yrow),
                     COL_trans = c(mcol,xcol,ycol)) %>%
    filter(PANEL_TYPE %in% c("main",sidePanel))
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
                           COL_trans=="ALL" ~ COL),
           SCALE_X = case_when(PANEL_TYPE=="y" ~ x_scale_fun(SCALE_X),
                               TRUE ~ SCALE_X),
           SCALE_Y = case_when(PANEL_TYPE=="x" ~ y_scale_fun(SCALE_Y),
                               TRUE ~ SCALE_Y))
  layout <- layout %>%
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
            layout <- facet_compute(data, params)
            layout <- sidePanelLayout(layout, sidePanel = sides, ggside = ggside)
            layout },
          map_data = function(data, layout,
                              params, facet_mapping = facet$map_data){
            #browser()
            facet_vars <- c(names(params$facets),names(params$rows),names(params$cols))
            data <- unnest(data, PANEL_TYPE)
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



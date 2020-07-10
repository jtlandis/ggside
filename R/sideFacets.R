#' @export
make_sideFacets <- function(facet){

  sideFacets <- function(layout,
                         sidePanel = c("x","y"),
                         scales = "fixed"){
    if(all(c("x","y")%in%sidePanel)){
      main <- layout %>%
        mutate(ROW = ROW*2-1,
               COL = COL*2-1)
      yp <- layout %>%
        mutate(ROW = ROW*2-1,
               COL = COL*2,
               SCALE_X = max(SCALE_X) + 1)
      xp <- layout %>%
        mutate(ROW = ROW*2,
               COL = COL*2-1,
               SCALE_Y = max(SCALE_Y) + 1)
      layout <- bind_rows(main, yp, xp)

    } else if("x"%in% sidePanel){
      main <- layout %>%
        mutate(ROW = ROW*2-1)
      xp <- layout %>%
        mutate(ROW = ROW*2,
               SCALE_Y = max(SCALE_Y) + 1)
      layout <- bind_rows(main, xp)
    } else {
      main <- layout %>%
        mutate(COL = COL*2-1)
      yp <- layout %>%
        mutate(COL = COL*2,
               SCALE_X = max(SCALE_X) + 1)
      layout <- bind_rows(main, yp)

    }
    layout <- layout %>%
      arrange(ROW, COL) %>%
      mutate(PANEL = factor(1:n()))
    return(layout)
  }

  ggproto(NULL,
          facet,
          compute_layout = function(data, params,
                                    facet_compute = facet$compute_layout){
            browser()
            layout <- facet_compute(data, params)
            layout <- sideFacets(layout)
            layout },
          map_data = function(data, layout,
                              params, facet_mapping = facet$map_data){
            browser()
            facet_vars <- c(names(params$facets),names(params$rows),names(params$cols))
            if(is.null(facet_vars)){
              data <- mutate(data,
                             PANEL = list(unique(layout$PANEL)))
            } else {
              panels <- layout %>% group_by(.dots = facet_vars) %>%
                summarise(PANEL = list(unique(PANEL)))
              data <- left_join(data, panels, by = facet_vars)
            }

            data %>%
              tidyr::unnest(PANEL)
          }
  )
}



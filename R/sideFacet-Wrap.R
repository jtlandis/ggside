#sideFacetWrap



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
                                  init_scales = function(layout, x_scale = NULL, y_scale = NULL, params){
                                    scales <- FacetNull$init_scales(layout, x_scale, y_scale, params)
                                    if (!is.null(x_scale)&& !is.null(params$ggside$ysidex)){
                                      side_indx <-  unique(layout[layout$PANEL_TYPE=="y",]$SCALE_X)
                                      scales$x[side_indx] <- lapply(side_indx, function(i) params$ggside$ysidex$clone())

                                    }
                                    if (!is.null(y_scale)&& !is.null(params$ggside$xsidey)){
                                      side_indx <-  unique(layout[layout$PANEL_TYPE=="x",]$SCALE_Y)
                                      scales$y[side_indx] <- lapply(side_indx, function(i) params$ggside$xsidey$clone())

                                    }
                                    scales
                                  },
                                  ,
                                  draw_panels = sideFacetWrap_draw_panels
)

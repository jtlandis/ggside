


#' @rdname ggside-ggproto-facets
#' @usage NULL
#' @format NULL
#' @export
FacetSideNull <- ggplot2::ggproto("FacetSideNull",
                                  ggplot2::FacetNull,
                                  compute_layout = ,
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
                                  map_data = function (data, layout, params) {
                                    if (is.waive(data))
                                      return(new_data_frame(list(PANEL = factor())))
                                    if (empty(data))
                                      return(new_data_frame(c(data, list(PANEL = factor()))))

                                    prep_map_data(layout, data)
                                    keys <- join_keys(data, layout, by = "PANEL_TYPE")
                                    data[["PANEL"]] <- layout[["PANEL"]][match(keys$x, keys$y)]
                                    data
                                  },
                                  draw_panels = sideFacetNull_draw_panels)

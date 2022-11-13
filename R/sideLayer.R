
# Very crude way of relying on 'ggplot2:::Layer' without
# causing R CMD CHECK fails.
Layer <- do.call(":::", c(as.name("ggplot2"), as.name("Layer")))

find_parent <- function(find){
  for(N in seq_len(sys.parent())){
    if(find %in% ls(parent.frame(N))){
      obj <- get(find, envir = parent.frame(N))
      return(obj)
    }
  }
}

get_proper_scales <- function(data, scales){
  new_scale_list <- scales$clone()
  use_panels <- unique(data[['PANEL']])
  proto_layout <- find_parent('layout')
  layout_df <- proto_layout$layout
  layout_df <- layout_df[layout_df$PANEL %in% use_panels,]
  panel_type <- as.character(unique(layout_df[["PANEL_TYPE"]]))
  if (length(panel_type)==0) return(NULL)
  aesthetic <-switch(panel_type,
                     x = 'y',
                     y = 'x')
  scale_ref <- switch(aesthetic,
                      #if aesthetic is "x" we only need to replace y
                      x = proto_layout$panel_scales_x[layout_df$SCALE_X][[1]],
                      y = proto_layout$panel_scales_y[layout_df$SCALE_Y][[1]],
                      NULL)
  if(is.null(scale_ref)) return(NULL)
  new_scale_list$scales[new_scale_list$find(aesthetic)] <- NULL
  new_scale_list$add(scale_ref$clone())
  return(new_scale_list)
}

exclude_plot_aes_ggside <- function(plot, layer) {
  priority_aes <- c("xfill","yfill","xcolour","ycolour")
  side_aes_used <- priority_aes %in% names(layer$mapping)
  if (any(side_aes_used)) {
    side_aes_used <- priority_aes[side_aes_used]
    plot_aes <- unique(gsub("^(x|y)", "", side_aes_used))
    plot_aes_used <- names(plot$mapping) %in% plot_aes
    plot$mapping <- plot$mapping[!plot_aes_used]
  }
  plot
}

XLayer <- ggplot2::ggproto("XLayer",
                           Layer,
                           setup_layer = function(self, data, plot){
                             plot <- exclude_plot_aes_ggside(plot, self)
                             data <- ggproto_parent(Layer, self)$setup_layer(data = data, plot = plot)
                             data$PANEL_TYPE <- "x"
                             data
                           },
                           map_statistic = function(self, data, plot) {
                             plot <- plot_clone(plot)
                             plot$scales <- get_proper_scales(data, plot$scales) %||% plot$scales
                             ggproto_parent(Layer, self)$map_statistic(data, plot)
                           })

YLayer <- ggplot2::ggproto("YLayer",
                           Layer,
                           setup_layer = function(self, data, plot){
                             plot <- exclude_plot_aes_ggside(plot, self)
                             data <- ggproto_parent(Layer, self)$setup_layer(data = data, plot = plot)
                             data$PANEL_TYPE <- "y"
                             data
                           },
                           map_statistic = function(self, data, plot) {
                             plot <- plot_clone(plot)
                             plot$scales <- get_proper_scales(data, plot$scales) %||% plot$scales
                             ggproto_parent(Layer, self)$map_statistic(data, plot)
                           })

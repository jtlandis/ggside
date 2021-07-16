
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
  new_scale_list$scales <- NULL
  use_panels <- unique(data[['PANEL']])
  proto_layout <- find_parent('layout')
  layout_df <- proto_layout$layout
  layout_df <- layout_df[layout_df$PANEL %in% use_panels,]
  scales_x <- proto_layout$panel_scales_x[layout_df$SCALE_X]
  scales_y <- proto_layout$panel_scales_y[layout_df$SCALE_Y]
  scales <- c(scales_x, scales_y)
  if(length(scales)==0L) return(NULL)
  lapply(scales, function(scale, scales_list) scales_list$add(scale$clone()), scales_list = new_scale_list)
  return(new_scale_list)
}


XLayer <- ggplot2::ggproto("XLayer",
                           Layer,
                           setup_layer = function(self, data, plot){
                             data$PANEL_TYPE <- "x"
                             data
                           },
                           map_statistic = function(self, data, plot) {
                             if (empty(data)) return(new_data_frame())

                             # Make sure data columns are converted to correct names. If not done, a
                             # column with e.g. a color name will not be found in an after_stat()
                             # evaluation (since the evaluation symbols gets renamed)
                             data <- rename_aes(data)

                             # Assemble aesthetics from layer, plot and stat mappings
                             aesthetics <- self$mapping
                             if (self$inherit.aes) {
                               aesthetics <- defaults(aesthetics, plot$mapping)
                             }
                             aesthetics <- defaults(aesthetics, self$stat$default_aes)
                             aesthetics <- compact(aesthetics)

                             new <- strip_dots(aesthetics[is_calculated_aes(aesthetics) | is_staged_aes(aesthetics)])
                             if (length(new) == 0) return(data)

                             # Add map stat output to aesthetics
                             env <- child_env(baseenv(), stat = stat, after_stat = after_stat)
                             stage_mask <- child_env(emptyenv(), stage = stage_calculated)
                             mask <- new_data_mask(as_environment(data, stage_mask), stage_mask)
                             mask$.data <- as_data_pronoun(mask)

                             new <- substitute_aes(new)
                             stat_data <- lapply(new, eval_tidy, mask, env)

                             # Check that all columns in aesthetic stats are valid data
                             nondata_stat_cols <- check_nondata_cols(stat_data)
                             if (length(nondata_stat_cols) > 0) {
                               msg <- paste0(
                                 "Aesthetics must be valid computed stats. Problematic aesthetic(s): ",
                                 paste0(vapply(nondata_stat_cols, function(x) {paste0(x, " = ", as_label(aesthetics[[x]]))}, character(1)), collapse = ", "),
                                 ". \nDid you map your stat in the wrong layer?"
                               )
                               abort(msg)
                             }

                             names(stat_data) <- names(new)
                             stat_data <- new_data_frame(compact(stat_data))

                             # Add any new scales, if needed
                             scales_add_defaults(plot$scales, data, new, plot$plot_env)
                             # Transform the values, if the scale say it's ok
                             # (see stat_spoke for one exception)
                             if (self$stat$retransform) {
                               new_scales <- get_proper_scales(data, plot$scales) %||% plot$scales
                               stat_data <- scales_transform_df(new_scales, stat_data)
                             }

                             cunion(stat_data, data)
                           })

YLayer <- ggplot2::ggproto("YLayer",
                           Layer,
                           setup_layer = function(self, data, plot){
                             data$PANEL_TYPE <- "y"
                             data
                           },
                           map_statistic = function(self, data, plot) {
                             if (empty(data)) return(new_data_frame())

                             # Make sure data columns are converted to correct names. If not done, a
                             # column with e.g. a color name will not be found in an after_stat()
                             # evaluation (since the evaluation symbols gets renamed)
                             data <- rename_aes(data)

                             # Assemble aesthetics from layer, plot and stat mappings
                             aesthetics <- self$mapping
                             if (self$inherit.aes) {
                               aesthetics <- defaults(aesthetics, plot$mapping)
                             }
                             aesthetics <- defaults(aesthetics, self$stat$default_aes)
                             aesthetics <- compact(aesthetics)

                             new <- strip_dots(aesthetics[is_calculated_aes(aesthetics) | is_staged_aes(aesthetics)])
                             if (length(new) == 0) return(data)

                             # Add map stat output to aesthetics
                             env <- child_env(baseenv(), stat = stat, after_stat = after_stat)
                             stage_mask <- child_env(emptyenv(), stage = stage_calculated)
                             mask <- new_data_mask(as_environment(data, stage_mask), stage_mask)
                             mask$.data <- as_data_pronoun(mask)

                             new <- substitute_aes(new)
                             stat_data <- lapply(new, eval_tidy, mask, env)

                             # Check that all columns in aesthetic stats are valid data
                             nondata_stat_cols <- check_nondata_cols(stat_data)
                             if (length(nondata_stat_cols) > 0) {
                               msg <- paste0(
                                 "Aesthetics must be valid computed stats. Problematic aesthetic(s): ",
                                 paste0(vapply(nondata_stat_cols, function(x) {paste0(x, " = ", as_label(aesthetics[[x]]))}, character(1)), collapse = ", "),
                                 ". \nDid you map your stat in the wrong layer?"
                               )
                               abort(msg)
                             }

                             names(stat_data) <- names(new)
                             stat_data <- new_data_frame(compact(stat_data))

                             # Add any new scales, if needed
                             scales_add_defaults(plot$scales, data, new, plot$plot_env)
                             # Transform the values, if the scale say it's ok
                             # (see stat_spoke for one exception)
                             if (self$stat$retransform) {
                               new_scales <- get_proper_scales(data, plot$scales) %||% plot$scales
                               stat_data <- scales_transform_df(new_scales, stat_data)
                             }

                             cunion(stat_data, data)
                           })

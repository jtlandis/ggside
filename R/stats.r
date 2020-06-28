#Stat


#' @import dplyr
StatSidebar <- ggplot2::ggproto("Sidebar",
                       Stat,
                       required_aes = c("x|y", "xfill|yfill"),
                       compute_panel = function(self, data, scales, ...) {
                         browser()
                         if (empty(data)) return(new_data_frame())
                         #determine if xfill or yfill was passed...
                         is_xbar <- "xfill"%in%colnames(data)
                         if(is_xbar){
                           yres <- if(resolution(data$y, FALSE)!=1) (diff(range(data$y))*.05) else 1
                           data$height <- data$height %||% yres
                           data <- data %>%
                             mutate(bottom = min(y)-height,
                                    top = max(y)-height,
                                    group = x) %>%
                             select(-y) %>%
                             gather(key = "stat_key", value = "y", bottom, top)
                         } else {
                           xres <- if(resolution(data$x, FALSE)!=1) (diff(range(data$x))*.05) else 1
                           data$width <- data$width %||% xres
                           data <- data %>%
                             mutate(left = min(x)-width,
                                    right = max(x)+width,
                                    group = y) %>%
                             select(-x) %>%
                             gather(key = "stat_key", value = "x", left, right)
                         }
                         groups <- split(data, data$group)
                         stats <- lapply(groups, function(group) {
                           self$compute_group(data = group, scales = scales, ...)
                         })

                         stats <- mapply(function(new, old) {
                           if (empty(new)) return(new_data_frame())
                           unique <- uniquecols(old)
                           missing <- !(names(unique) %in% names(new))
                           cbind(
                             new,
                             unique[rep(1, nrow(new)), missing,drop = FALSE]
                           )
                         }, stats, groups, SIMPLIFY = FALSE)

                         rbind_dfs(stats)
                       },
                       compute_group = function(self, data, scales){
                         dplyr::distinct_all(data)
                       })

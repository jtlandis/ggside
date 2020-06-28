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
                           pos.posit <- range(data$y) + (yres*c(-1,1))
                           data <- data %>%
                             select(-y) %>%
                             mutate(bottom = pos.posit[1],
                                    top = pos.posit[2],
                                    group = x) %>%
                             gather(key = "stat_key", value = "y", ymin, ymax)
                         } else {
                           xres <- if(resolution(data$x, FALSE)!=1) (diff(range(data$x))*.05) else 1
                           data <- mutate(data,
                                          x = xrex,
                                          group = y)
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

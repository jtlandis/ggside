#Stat


#' @import dplyr
StatSidebar <- ggplot2::ggproto("Sidebar",
                       Stat,
                       required_aes = c("x|y", "xfill|yfill"),
                       compute_panel = function(self, data, scales, ...) {
                         #
                         if (empty(data)) return(new_data_frame())
                         #determine if xfill or yfill was passed...
                         is_xbar <- "xfill"%in%colnames(data)
                         if(is_xbar){
                           yres <- if(resolution(data$y, FALSE)!=1) (diff(range(data$y))*.1) else 1
                           data$height <- data$height %||% yres #insure height is positive
                           data$height <- abs(data$height)
                           data <- data %>%
                             mutate(bottom = min(y)-yres,
                                    top = max(y)+yres,
                                    group = x,
                                    yres = yres) %>%
                             select(-y) %>%
                             gather(key = "stat_key", value = "y", bottom, top)
                         } else {
                           xres <- if(resolution(data$x, FALSE)!=1) (diff(range(data$x))*.1) else 1
                           data$width <- data$width %||% xres
                           data$width <- abs(data$width)
                           data <- data %>%
                             mutate(left = min(x)-xres,
                                    right = max(x)+xres,
                                    group = y,
                                    xres = xres) %>%
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
                       },
                       compute_layer = function(self, data, params, layout) {
                         #
                         check_required_aesthetics(
                           self$required_aes,
                           c(names(data), names(params)),
                           snake_class(self)
                         )

                         # Make sure required_aes consists of the used set of aesthetics in case of
                         # "|" notation in self$required_aes
                         required_aes <- intersect(
                           names(data),
                           unlist(strsplit(self$required_aes, "|", fixed = TRUE))
                         )

                         data <- remove_missing(data, params$na.rm,
                                                c(required_aes, self$non_missing_aes),
                                                snake_class(self),
                                                finite = FALSE
                         )

                         # Trim off extra parameters
                         params <- params[intersect(names(params), self$parameters())]

                         args <- c(list(data = quote(data), scales = quote(scales)), params)
                         dapply(data, "PANEL", function(data) {
                           scales <- layout$get_scales(data$PANEL[1])
                           tryCatch(do.call(self$compute_panel, args), error = function(e) {
                             warn(glue("Computation failed in `{snake_class(self)}()`:\n{e$message}"))
                             new_data_frame()
                           })
                         })
                       })

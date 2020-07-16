

# sidebar <- function(data = NULL,
#                     mapping = aes(),
#                     xfill = vars(),
#                     yfill = vars(),
#                     xlocation = "bottom",
#                     ylocation = "left",
#                     xstat = "sidebar",
#                     ystat = "sidebar",
#                     xfun = list(),
#                     yfun = list(),
#                     xfun.args = list(),
#                     yfun.args = list(),
#                     ...){
#   browser()
#   p <- ggplot(data = data, mapping = mapping) +
#     scale_x_discrete() +
#     scale_y_discrete()
#   #parse x information
#   x <- data %>% pull(!!mapping$x)
#   x_discrete <- is.character(x)||is.factor(x)
#   xres <- if(x_discrete) 1 else diff(range(x))*.05
#   xmin <- if(x_discrete) 1 else min(x)
#   xmax <- if(x_discrete) length(unique(x)) else max(x)
#   #parse y information
#   y <- data %>% pull(!!mapping$y)
#   y_discrete <- is.character(y)||is.factor(y)
#   yres <- if(y_discrete) 1 else diff(range(y))*.05
#   ymin <- if(y_discrete) 1 else min(y)
#   ymax <- if(y_discrete) length(unique(y)) else max(y)
#
#   if(length(xfill)>0){
#     xdata <- data.frame(xfill=unlist(lapply(xfill,as_label)),
#                         location = xlocation, yres = yres,
#                         ymin = ymin, ymax = ymax) %>%
#       mutate(..order.. = 1:n()) %>%
#       group_by(location) %>%
#       mutate(instance = 1:n(),
#              yint = case_when(location=="bottom" ~ ymin-(yres*instance),
#                               location=="top" ~ ymax+(yres*instance))) %>% ungroup()
#     for(i in 1:nrow(xdata)){
#       if(xstat=="summarise"&&length(xfun)>0){
#         for(j in 1:length(xfun)){
#           p <- p +
#             geom_xsidebar(aes_(xfill = xfill[[i]],
#                                y = 0,
#                                yintercept = xdata$yint[i]),
#                           fun = xfun[[j]])
#         }
#       }else{
#         p <- p +
#           geom_xsidebar(aes_(xfill = xfill[[i]],
#                              y = 0,
#                              yintercept = xdata$yint[i]))
#       }
#     }
#   }
#
#   if(length(yfill)>0){
#     ydata <- data.frame(yfill=unlist(lapply(yfill,as_label)),
#                         location = ylocation, xres = xres,
#                         xmin = xmin, xmax = xmax) %>%
#       mutate(..order.. = 1:n()) %>%
#       group_by(location) %>%
#       mutate(instance = 1:n(),
#              xint = case_when(location=="left" ~ xmin-(xres*instance),
#                               location=="right" ~ xmax+(xres*instance))) %>% ungroup()
#     .ydata <- data %>% select(!!mapping$y, !!!yfill) %>%
#       tidyr::gather(key = "yfill", value = "ybar", -1) %>%
#       left_join(., y = ydata, by = "yfill")
#
#     for(i in 1:nrow(ydata)){
#       if(ystat=="summarise"&&length(yfun)>0){
#         for(j in 1:length(yfun)){
#           p <- p +
#             geom_ysidebar(aes_(yfill = yfill[[i]],
#                                x = 0,
#                                xintercept = ydata$xint[i]),
#                           fun = yfun[[j]])
#         }
#       }else{
#         p <- p +
#           geom_ysidebar(aes_(yfill = yfill[[i]],
#                              x = 0,
#                              xintercept = ydata$xint[i]))
#       }
#     }
#   }
#   p
# }

# mtcars1 <- as_tibble(mtcars, rownames = "Cars") %>%
#   gather(key = "qualities", value = "value", -Cars) %>%
#   group_by(qualities) %>%
#   mutate(scaledValue = scale(value)) %>%
#   ungroup() %>%
#   mutate(carCase = case_when(str_detect(Cars, "^[A-M]") ~ "A-M",
#                              TRUE ~ "N-Z"),
#          qualityCase = case_when(str_detect(qualities, "^[a-m]") ~ "a-m",
#                                  TRUE ~ "n-z"),
#          endCase = case_when(str_detect(Cars, regex("[a-m]$", ignore_case = T)) ~ "[a-m]$",
#                              str_detect(Cars, regex("[n-z]$", ignore_case = T)) ~ "[n-z]$",
#                              TRUE ~ "[0-9]$")) %>%
#   group_by(Cars) %>%
#   mutate(mean_scale=mean(scaledValue)) %>% ungroup() %>%
#   group_by(qualities) %>%
#   mutate(mean_qual=mean(value)) %>% ungroup()
#
# sidebar(mtcars1, aes(Cars, qualities), xfill = vars(carCase, endCase), xlocation = "top",
#         yfill = vars(scaledValue, mean_qual), yfun = list(mean, sd), ystat = "summarise") +
#   geom_tile(aes(fill = scaledValue))

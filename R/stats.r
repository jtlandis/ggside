#Stat

# Notes: overriding y is not effective for absolute positions, unless statIdentity is used.
# If user wants to place bars at specific points with StatSidebar, use yintercept aesthetic.
# using statidentity or yintercept aesthetics causes location assignments to be ignored.
# y in the context of geom_xsidebar is only used to determine an appropriate location for
# sidebars such that they do not overlap points in top layer. Passing a scalar value or vector
# whose unique length equals 1 to y will force yintercept behavior. Note, if both y and yintercept
# are passed, yintercept takes precidence over y in StatSidebar and StatSummarise.
#' @importFrom tidyr gather

StatSidebar <- ggplot2::ggproto("Sidebar",
                       Stat,
                       required_aes = c("x|y", "xfill|yfill"),
                       optional_aes = c("yintercept|xintercept"),
                       compute_panel = function(self, data, scales,location = NULL,
                                                height = NULL,width = NULL,
                                                yintercept = NULL, xintercept = NULL) {
                         #
                         #browser()
                         env <- find_build_plotEnv()
                         .hs <- get_variable(".build_history", envir = env) %||% data_frame(loc = c("top","right","bottom","left"),
                                                                                            indx = c(1,1,1,1))
                         if (empty(data)) return(new_data_frame())
                         #determine if xfill or yfill was passed...
                         is_xbar <- "xfill"%in%colnames(data)
                         if(is_xbar){
                           #check if y exists in data, if not assign 0
                           data$y <- data$y %||% 0
                           #if unique length is 1, treat as if it was yintercept
                           #WARNING: this disables location functionality
                           if(length(unique(data$y))==1){
                             yintercept <- yintercept %||% data$y
                           }
                           #check resolution of y data to see how far to adjust
                           yres <- if(resolution(data$y, FALSE)!=1) (diff(range(data$y))*.1) else 1
                           #provide the height adjustment. If none specified, use yres
                           data$height <- data$height %||% height %||% yres
                           if(any(data$height<0)){
                             warn("height has been given a negative value. Using abosulte value")
                             data$height <- abs(data$height)
                           }
                           data$width <- data$width %||% width %||% resolution(data$x, FALSE)
                           data$location <- data$location %||% location %||% "bottom"
                           data <- mutate(data, group = x)
                           yint <- data$yintercept %||% yintercept
                           if(is.null(yint)){ #e.i. user prefer's StatSidebar to determine y positions
                             if(!all(data$location%in%c("top","bottom"))){
                               .badLoc <- !data$location%in%c("top","bottom")
                               warn(glue("unrecognized location assignments:\n",
                                         "{paste(unique(data$location[.badLoc]),collapse = \", \")}",
                                         "\nConverting to default: bottom"))
                               data$location[.badLoc] <- "bottom"
                             }
                             data <-  left_join(data, .hs, by = c("location"="loc")) %>%
                               mutate(y = case_when(location=="bottom" ~ min(y)-yres*indx + ((yres-height)/2),
                                                    location=="top" ~ max(y)+yres*indx - ((yres-height)/2)))

                           } else {
                             data$y <- yint
                           }
                         } else {
                           #check if x exists in data, if not assign 0
                           data$x <- data$x %||% 0
                           #if unique length is 1, treat as if it was xintercept
                           #WARNING: this disables location functionality
                           if(length(unique(data$x))==1){
                             xintercept <- xintercept %||% data$x
                           }
                           #check resolution of x data to see how far to adjust
                           xres <- if(resolution(data$x, FALSE)!=1) (diff(range(data$x))*.1) else 1
                           #provide the width adjustment. If none specified, use xres
                           data$width <- data$width %||% width %||% xres
                           if(any(data$width<0)){
                             warn("width has been given a negative value. Using abosulte value")
                             data$width <- abs(data$width)
                           }
                           data$height <- data$height %||% height %||% resolution(data$y, FALSE)
                           data$location <- data$location %||% location %||% "left"
                           data <- mutate(data, group = y)
                           xint <- data$xintercept %||% xintercept
                           if(is.null(xint)){ #e.i. user prefer's StatSidebar to determine x positions
                             if(!all(data$location%in%c("right","left"))){
                               .badLoc <- !data$location%in%c("right","left")
                               warn(glue("unrecognized location assignments:\n",
                                         "{paste(unique(data$location[.badLoc]),collapse = \", \")}",
                                         "\nConverting to default: left"))
                               data$location[.badLoc] <- "left"
                             }
                             data <-  left_join(data, .hs, by = c("location"="loc")) %>%
                               mutate(x = case_when(location=="left" ~ min(x)-xres*indx + ((xres-width)/2),
                                                    location=="right" ~ max(x)+xres*indx - ((xres-width)/2)))


                           } else {
                             data$x <- xint
                           }
                         }
                         data <- data[,!colnames(data)%in%"indx"] %>% dplyr::distinct_all()
                         logi <- .hs$loc %in% data$location
                         .hs[logi,"indx"] <- .hs[logi,"indx"] + 1
                         assign(".build_history", value = .hs, envir = env)
                         data
                       },
                       compute_group = function(self, data, scales){
                         dplyr::distinct_all(data)
                       },
                       compute_layer = function(self, data, params, layout) {
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


StatSummarise <- ggplot2::ggproto("Summarise",
                                StatSidebar,
                                required_aes = c("x|y", "xfill|yfill"),
                                option_aes = c("yintercept|xintercept"),
                                compute_panel = function(self, data, scales, location = NULL,
                                                         height = NULL,width = NULL,
                                                         yintercept = NULL, xintercept = NULL,
                                                         fun = NULL, fun.args = list()) {
                                  #
                                  browser()
                                  env <- find_build_plotEnv()
                                  if(is.null(fun)) {
                                    warn("fun is null, using mean as default")
                                    fun <- mean
                                  }
                                  call_f <- function(fun, x) {
                                    if (is.null(fun)) return(NA_real_)
                                    fun <- as_function(fun)
                                    do.call(fun, c(list(quote(x)), fun.args))
                                  }

                                  if (empty(data)) return(new_data_frame())
                                  #determine if xfill or yfill was passed...
                                  is_xbar <- "xfill"%in%colnames(data)
                                  if(is_xbar){
                                    fun.data <- data %>%
                                      group_by(x) %>%
                                      dplyr::summarise(xfill=call_f(fun, xfill))
                                    data <- select(data,-xfill) %>%
                                      left_join(., y = fun.data, by = "x")
                                    #check if y exists in data, if not assign 0
                                    data$y <- data$y %||% 0
                                    #if unique length is 1, treat as if it was yintercept
                                    #WARNING: this disables location functionality
                                    if(length(unique(data$y))==1){
                                      yintercept <- yintercept %||% data$y
                                    }
                                    #check resolution of y data to see how far to adjust
                                    yres <- if(resolution(data$y, FALSE)!=1) (diff(range(data$y))*.1) else 1
                                    #provide the height adjustment. If none specified, use yres
                                    data$height <- data$height %||% height %||% yres
                                    if(any(data$height<0)){
                                      warn("height has been given a negative value. Using abosulte value")
                                      data$height <- abs(data$height)
                                    }
                                    data$width <- data$width %||% width %||% resolution(data$x, FALSE)
                                    data$location <- data$location %||% location %||% "bottom"
                                    data <- mutate(data, group = x)
                                    yint <- data$yintercept %||% yintercept
                                    if(is.null(yint)){ #e.i. user prefer's StatSidebar to determine y positions
                                      if(!all(data$location%in%c("top","bottom"))){
                                        .badLoc <- !data$location%in%c("top","bottom")
                                        warn(glue("unrecognized location assignments:\n",
                                                  "{paste(unique(data$location[.badLoc]),collapse = \", \")}",
                                                  "\nConverting to default: bottom"))
                                        data$location[.badLoc] <- "bottom"
                                      }
                                      data <-  mutate(data,
                                                      y = case_when(location=="bottom" ~ min(y)-yres + ((yres-height)/2),
                                                                    location=="top" ~ max(y)+yres - ((yres-height)/2)))

                                    } else {
                                      data$y <- yint
                                    }
                                  } else {
                                    fun.data <- data %>%
                                      group_by(y) %>%
                                      dplyr::summarise(yfill=call_f(fun, yfill))
                                    data <- select(data,-yfill) %>%
                                      left_join(., y = fun.data, by = "y")
                                    #check if x exists in data, if not assign 0
                                    data$x <- data$x %||% 0
                                    #if unique length is 1, treat as if it was xintercept
                                    #WARNING: this disables location functionality
                                    if(length(unique(data$x))==1){
                                      xintercept <- xintercept %||% data$x
                                    }
                                    #check resolution of x data to see how far to adjust
                                    xres <- if(resolution(data$x, FALSE)!=1) (diff(range(data$x))*.1) else 1
                                    #provide the width adjustment. If none specified, use xres
                                    data$width <- data$width %||% width %||% xres
                                    if(any(data$width<0)){
                                      warn("width has been given a negative value. Using abosulte value")
                                      data$width <- abs(data$width)
                                    }
                                    data$height <- data$height %||% height %||% resolution(data$y, FALSE)
                                    data$location <- data$location %||% location %||% "left"
                                    data <- mutate(data, group = y)
                                    xint <- data$xintercept %||% xintercept
                                    if(is.null(xint)){ #e.i. user prefer's StatSidebar to determine x positions
                                      if(!all(data$location%in%c("right","left"))){
                                        .badLoc <- !data$location%in%c("right","left")
                                        warn(glue("unrecognized location assignments:\n",
                                                  "{paste(unique(data$location[.badLoc]),collapse = \", \")}",
                                                  "\nConverting to default: left"))
                                        data$location[.badLoc] <- "left"
                                      }
                                      data <-  mutate(data,
                                                      x = case_when(location=="left" ~ min(x)-xres + ((xres-width)/2),
                                                                    location=="right" ~ max(x)+xres - ((xres-width)/2)))

                                    } else {
                                      data$x <- xint
                                    }
                                  }

                                  data <- distinct_all(data)
                                  data
                                },
                                compute_group = function(self, data, scales){
                                  dplyr::distinct_all(data)
                                },
                                compute_layer = function(self, data, params, layout) {
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

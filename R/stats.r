#Stat

# Notes: overriding y is not effective for absolute positions, unless statIdentity is used.
# If user wants to place bars at specific points with StatSidebar, use yintercept aesthetic.
# using statidentity or yintercept aesthetics causes location assignments to be ignored.
# y in the context of geom_xsidebar is only used to determine an appropriate location for
# sidebars such that they do not overlap points in top layer. Passing a scalar value or vector
# whose unique length equals 1 to y will force yintercept behavior. Note, if both y and yintercept
# are passed, yintercept takes precidence over y in StatSidebar and StatSummarise.

#' @rdname stat_summarise
#' @aliases stat_summarize
#' @title Summarise by grouping variable
#' @description Applies a function to a specified grouping variable
#' @examples
#' #library(tidyr)
#' i <- gather(iris,"key","value",-Species)
#' ggplot(i, aes(Species, fill = key, domain = value)) +
#'    geom_bar(aes(y = after_stat(summarise)), stat = "summarise") +
#'    stat_summarise(aes(y = after_stat(summarise),
#'                   label = after_stat(summarise)),
#'                   position = position_stack(vjust = .5), geom = "text")
#' ggplot(i, aes(key, fill = Species, domain = value)) +
#'    geom_bar(aes(y = after_stat(summarise)), stat = "summarise") +
#'    stat_summarise(aes(y = after_stat(summarise),
#'                   label = after_stat(summarise)),
#'                   position = position_stack(vjust = .5), geom = "text")
#' @export
stat_summarise <- function(mapping = NULL, data = NULL, geom = "bar", position = "identity",
                          ..., fun = NULL, fun.args = list(), show.legend = NA,
                          inherit.aes = TRUE){
  layer(geom = geom, stat = StatSummarise, data = data, mapping = mapping, position = position,
        params = list(fun = fun, fun.args = fun.args, ...), inherit.aes = inherit.aes,
        show.legend = show.legend)
}

#' @rdname stat_summarise
#' @export
stat_summarize <- stat_summarise

#' @rdname stat_summarise
#' @importFrom dplyr summarise
#' @usage NULL
#' @export
StatSummarise <- ggplot2::ggproto("Summarise",
                                Stat,
                                required_aes = c("domain"),
                                compute_panel = function(self, data, scales, domain = NULL,
                                                         fun = NULL, fun.args = list()) {
                                  #
                                  if (empty(data)) return(new_data_frame())
                                  #browser()

                                  if(is.null(fun)) {
                                    warn("fun is null, using mean as default")
                                    fun <- mean
                                  }

                                  groups <- split(data, data$group)
                                  stats <- lapply(groups, function(group){
                                    self$compute_group(data = group, fun = fun, fun.args = fun.args)
                                  })

                                  rbind_dfs(stats)
                                },
                                compute_group = function(self, data, scales,  fun = NULL, fun.args = fun.args){

                                  call_f <- function(fun = fun, x) {
                                    if (is.null(fun)) return(NA_real_)
                                    fun <- as_function(fun)
                                    do.call(fun, c(list(quote(x)), fun.args))
                                  }
                                  #browser()
                                  data <- data %>%
                                    transform(summarise = call_f(fun, domain), domain = NULL) %>%
                                    dplyr::distinct_all() %>%
                                    mutate(summarize = summarise)
                                  data
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

#' @rdname stat_summarise
#' @usage NULL
#' @export
StatSummarize <- ggplot2::ggproto("Summarize",
                                  Stat,
                                  required_aes = c("domain"),
                                  compute_panel = function(self, data, scales, domain = NULL,
                                                           fun = NULL, fun.args = list()) {
                                    #
                                    if (empty(data)) return(new_data_frame())
                                    #browser()

                                    if(is.null(fun)) {
                                      warn("fun is null, using mean as default")
                                      fun <- mean
                                    }

                                    groups <- split(data, data$group)
                                    stats <- lapply(groups, function(group){
                                      self$compute_group(data = group, fun = fun, fun.args = fun.args)
                                    })

                                    rbind_dfs(stats)
                                  },
                                  compute_group = function(self, data, scales,  fun = NULL, fun.args = fun.args){

                                    call_f <- function(fun = fun, x) {
                                      if (is.null(fun)) return(NA_real_)
                                      fun <- as_function(fun)
                                      do.call(fun, c(list(quote(x)), fun.args))
                                    }
                                    #browser()
                                    data <- data %>%
                                      transform(summarise = call_f(fun, domain), domain = NULL) %>%
                                      dplyr::distinct_all() %>%
                                      mutate(summarize = summarise)
                                    data
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


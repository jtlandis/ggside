### INCLUDE BEGIN
#' @include aaa-utilities.r
#' @include aab-other_utils.r
#' @include aes-evaluation.r
#' @include compat-plyr.R
#' @include performance.R
NULL
### INCLUDE END
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
#' @inheritParams ggplot2::layer
#' @param fun Summarising function to use. If no function provided
#' it will default to \link[base]{length}.
#' @param args List of additional arguments passed to the function.
#' @param ... additional arguments to pass to \link[ggplot2]{layer}.
#' @section Aesthetics:
#' Using stat_summarise requires that you use `domain` as an aesthetic
#' mapping. This allows you to summarise other data instead of assuming
#' that `x` is the function's `domain`.
#' @examples
#' library(tidyr)
#' i <- gather(iris,"key","value",-Species)
#' ggplot(i, aes(Species, fill = key, domain = value)) +
#'    geom_bar(aes(y = after_stat(summarise)), stat = "summarise", fun = mean) +
#'    stat_summarise(aes(y = after_stat(summarise),
#'                   label = after_stat(summarise)),
#'                   position = position_stack(vjust = .5), geom = "text", fun = mean)
#' @return A Layer object to be added to a ggplot
#' @export
stat_summarise <- function(mapping = NULL, data = NULL, geom = "bar", position = "identity",
                          ..., fun = NULL, args = list(), show.legend = NA,
                          inherit.aes = TRUE){
  layer(geom = geom, stat = StatSummarise, data = data, mapping = mapping, position = position,
        params = list(fun = fun, args = args, ...), inherit.aes = inherit.aes,
        show.legend = show.legend)
}

#' @rdname stat_summarise
#' @export
stat_summarize <- stat_summarise

#' @rdname stat_summarise
#' @usage NULL
#' @export
StatSummarise <- ggplot2::ggproto("StatSummarise",
                                Stat,
                                required_aes = c("domain"),
                                compute_panel = function(self, data, scales, domain = NULL,
                                                         fun = NULL, args = list()) {
                                  #
                                  if (empty(data)) return(new_data_frame())
                                  #browser()

                                  if(is.null(fun)) {
                                    warn("fun is NULL, using length as default")
                                    fun <- length
                                  }

                                  groups <- split(data, data$group)
                                  stats <- lapply(groups, function(group){
                                    self$compute_group(data = group, fun = fun, args = args)
                                  })

                                  vec_rbind(stats)
                                },
                                compute_group = function(self, data, scales,  fun = NULL, args = args){

                                  call_f <- function(fun = fun, x) {
                                    if (is.null(fun)) return(NA_real_)
                                    fun <- as_function(fun)
                                    do.call(fun, c(list(quote(x)), args))
                                  }
                                  data <- unique(transform(data, summarise = call_f(fun, domain), domain = NULL))
                                  data[['summarize']] <- data[['summarise']]
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
StatSummarize <- ggplot2::ggproto("StatSummarize",
                                  Stat,
                                  required_aes = c("domain"),
                                  compute_panel = function(self, data, scales, domain = NULL,
                                                           fun = NULL, args = list()) {
                                    #
                                    if (empty(data)) return(new_data_frame())
                                    #browser()

                                    if(is.null(fun)) {
                                      warn("fun is NULL, using length as default")
                                      fun <- length
                                    }

                                    groups <- split(data, data$group)
                                    stats <- lapply(groups, function(group){
                                      self$compute_group(data = group, fun = fun, args = args)
                                    })

                                    vec_rbind(stats)
                                  },
                                  compute_group = function(self, data, scales,  fun = NULL, args = args){

                                    call_f <- function(fun = fun, x) {
                                      if (is.null(fun)) return(NA_real_)
                                      fun <- as_function(fun)
                                      do.call(fun, c(list(quote(x)), args))
                                    }
                                    data <- unique(transform(data, summarise = call_f(fun, domain), domain = NULL))
                                    data[['summarize']] <- data[['summarise']]
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


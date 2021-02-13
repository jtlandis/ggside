#' @rdname ggside
#' @description Set characteristics of side panels
#' @docType package
#' @param x.pos x side panel can either take "top" or "bottom"
#' @param y.pos y side panel can either take "right" or "left"
#' @param scales Determines side panel's unaligned axis
#' scale. Inputs are similar to facet_* scales function. Default
#' is set to "fixed", but "free_x", "free_y" and "free" are
#' acceptable inputs. For example, xside panels are aligned to
#' the x axis of the main panel. Setting "free" or "free_y" will
#' cause all y scales of the x side Panels to be independent.
#' @param collapse Determins if side panels should be collapsed into
#' a single panel. Set "x" to collapse all x side panels, set "y" to
#' collapse all y side panels, set "all" to collapse both x and y
#' side panels.
#'
#' @export
ggside <- function(x.pos = "top", y.pos = "right", scales = "fixed", collapse = NULL){
  structure(list(x.pos = x.pos,
                 y.pos = y.pos,
                 scales = scales,
                 collapse = collapse), class = "ggside_options")
}

#' @export
is.ggside <- function(x) inherits(x, "ggside")

#' @export
is.ggside_layer <- function(x) inherits(x, "ggside_layer")


#
# setOldClass(c("ggproto"))
# setOldClass(c("gg", "ggplot"))
# setOldClass(c("theme"))
# setOldClass(c("guides"))
# setOldClass(c("Facet"))
# setOldClass("FacetWrap")
# setClass("ggside", contains = c("gg", "ggplot", "list"))
#
# #' @export
# setMethod("initialize", "ggside",
#           function(.Object, plot){
#             .Object[names(plot)] <- plot
#             .Object
#           })
#
# #' @export
# setMethod("+", c("ggside", "ggproto"),
#           function(e1, e2){
#             browser()
#             e2name <- deparse(substitute(e2))
#             plot <- ggplot2:::add_ggplot(e1, e2, e2name)
#             if(!is.ggside_layer(e2)){
#               plot <- as_ggside(plot, e2)
#             }
#             plot
#           }
#           )
#
# #' @export
# setMethod("+", c("ggside", "theme"),
#           function(e1, e2){
#             e2name <- deparse(substitute(e2))
#             ggplot2:::add_ggplot(e1, e2, e2name)
#           }
# )
#
# #' @export
# setMethod("+", c("ggside", "guides"),
#           function(e1, e2){
#             e2name <- deparse(substitute(e2))
#             ggplot2:::add_ggplot(e1, e2, e2name)
#           }
# )
#
# # #' @export
# # setMethod("+", c("ggside", "Facet"),
# #           function(e1, e2){
# #             browser()
# #             e2name <- deparse(substitute(e2))
# #             plot <- ggplot2:::add_ggplot(e1, e2, e2name)
# #             if(!is.ggside_layer(e2)){
# #               plot <- as_ggside(plot, e2)
# #             }
# #             plot
# #           }
# # )
#
# # #' @export
# # setMethod("+", c("ggside", "FacetWrap"),
# #           function(e1, e2){
# #             browser()
# #             e2name <- deparse(substitute(e2))
# #             plot <- ggplot2:::add_ggplot(e1, e2, e2name)
# #             if(!is.ggside_layer(e2)){
# #               plot <- as_ggside(plot, e2)
# #             }
# #             plot
# #           }
# #)
#
# #' @export
# setMethod("show", signature("ggside"),
#           function(object){
#             ggplot2:::print.ggplot(object)})
#
#
#





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
#'
#' @export
ggside <- function(x.pos = "top", y.pos = "right", scales = "fixed", collapse = NULL){
  structure(list(x.pos = x.pos,
                 y.pos = y.pos,
                 scales = scales,
                 collapse = collapse), class = "ggside_layer")
}

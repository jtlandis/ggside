
#' @export
`+.gg` <- function(e1, e2) {
  UseMethod("+.gg")
}

#' @export
`+.gg.default` <- ggplot2:::`+.gg`

#' @export
`+.gg.ggside` <- function(e1, e2) {
  browser()
  plot <- NextMethod("+.gg")
  if(!is.ggside_layer(e2)){
    plot <- make_ggside(plot, e2)
  }
  plot
}

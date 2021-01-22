

`+.gg` <- function(e1, e2) {
  UseMethod("+.gg")
}

`+.gg.default` <- ggplot2:::`+.gg`

`+.gg.ggside` <- function(e1, e2) {
  browser()
  plot <- NextMethod()
  if(!is.ggside_layer(e2)){
    plot <- make_ggside(plot, e2)
  }
  plot
}

#ggplot2 extras

plot_clone <- function(plot) {
  p <- plot
  p$scales <- plot$scales$clone()

  p
}

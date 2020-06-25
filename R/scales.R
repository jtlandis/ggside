scale_xfill_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                            direction = 1, na.value = "grey50", aesthetics = "xfill")
{
  ggplot2::discrete_scale(aesthetics, "hue",
                          scales::hue_pal(h, c, l, h.start, direction), na.value = na.value, ...)
}

scale_xfill_discrete <- scale_xfill_hue

library(ggside)
library(hexSticker)
library(dplyr)

gg <- mutate(iris,
             Species2 = rep(c("A","B"), 75),
             Species3 = interaction(Species, Species2, lex.order = T, drop = T))

p <- ggplot(gg, aes(Sepal.Width, Petal.Width, color = Species3)) +
  geom_point(size = .04) +
  geom_xsidedensity(aes(y = after_stat(density), fill = Species3), position = "stack", lwd=0.009) +
  geom_ysideboxplot(aes(x = as.numeric(Species), fill = NULL), orientation = "x", outlier.size = .0375, lwd=.09) +
  #geom_ysidedensity(aes(x = after_stat(density), fill = Species3), position = "stack") +
  #geom_ysidehistogram(aes(x = after_stat(count), fill = Species3),binwidth = .1, position = "stack") +
  facet_grid(cols = vars(Species), rows = vars(Species2)) +
  ggside(collapse = "all") +
  scale_color_brewer(palette = "PuRd")+
  scale_fill_brewer(palette = "PuRd") +
  theme_void() +
  theme(
    panel.spacing = unit(1.01, "points"),
    strip.text = element_blank(),
    plot.background = element_rect(fill = "#2A78B5", color = "#2A78B5"),
    panel.background = element_rect(fill = "#87C2F0", color = "#87C2F0"),
    panel.grid.major =  element_line(colour = "#2A78B5", size = .05),
    panel.grid.minor = element_blank(),
    ggside.panel.scale = .2
  ) +
  guides(color = F, fill = F)
p

sticker(p, package = "ggside",
        p_size = 6,
        p_x = 1,
        p_y = 1.7,
        s_x =1, s_y=1, s_width = 1.6, s_height = 1,
        h_fill = "#2A78B5",
        h_color = "#87C2F0",filename="inst/figures/ggside.png",
        url = "https://github.com/jtlandis/ggside",
        u_size = .8,
        u_color = "#FFFFFF",
        u_y = 0.045,
        spotlight = T)
plot(s1)
save_custom_sticker <- function (filename, sticker = last_plot(),
                                 width = 43.9, height = 50.8, units = "mm", ...)
{
  args <- list(filename = filename, plot = sticker,width = width,
               height = height, units = units, bg = "transparent", ...)
  is_png <- (!is.null(args$device) && args$device == "png") ||
    tools::file_ext(filename) == "png"
  is_win <- .Platform$OS.type == "windows"
  if (is_png && is_win && capabilities("cairo")) {
    args$type <- "cairo-png"
    args$antialias <- "subpixel"
  }
  do.call(ggsave, args)
}

p1 <- ggplot(gg, aes(Sepal.Width, Petal.Width, color = Species3)) +
  geom_point(size = .0001) +
  geom_xsidedensity(aes(y = after_stat(density), fill = Species3), position = "stack", lwd=0.0009) +
  geom_ysideboxplot(aes(x = as.numeric(Species), fill = NULL), orientation = "x", outlier.size = .00175, lwd=.005) +
  #geom_ysidedensity(aes(x = after_stat(density), fill = Species3), position = "stack") +
  #geom_ysidehistogram(aes(x = after_stat(count), fill = Species3),binwidth = .1, position = "stack") +
  facet_grid(cols = vars(Species), rows = vars(Species2)) +
  ggside(collapse = "all") +
  scale_color_brewer(palette = "PuRd")+
  scale_fill_brewer(palette = "PuRd") +
  theme_void() +
  theme(
    panel.spacing = unit(1.01, "points"),
    strip.text = element_blank(),
    plot.background = element_rect(fill = "#2A78B5", color = "#2A78B5"),
    panel.background = element_rect(fill = "#87C2F0", color = "#87C2F0"),
    panel.grid.major =  element_line(colour = "#2A78B5", size = .05),
    panel.grid.minor = element_blank(),
    ggside.panel.scale = .2
  ) +
  guides(color = F, fill = F)

s <- sticker(p1, package = "ggside",
             p_size = 3,
             p_x = 1,
             p_y = 1.7,
             s_x =1, s_y=1, s_width = 1.6, s_height = 1,
             h_fill = "#2A78B5",
             h_color = "#87C2F0",filename="inst/figures/ggside.png",
             url = "https://github.com/jtlandis/ggside",
             u_size = .4,
             u_color = "#FFFFFF",
             u_y = 0.085,
             spotlight = T)
plot(s)
save_custom_sticker("inst/figures/ggside.png",
                    s, width = 1, height = .86, units = 'in', dpi = 450)

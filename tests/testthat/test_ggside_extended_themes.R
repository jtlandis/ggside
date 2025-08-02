
library(ggplot2)
library(ggside)

p <- ggplot(iris, aes(Sepal.Width, Petal.Length, color = Species)) +
  geom_point() +
  geom_xsidedensity() +
  geom_ysidedensity() +
  theme_test()


test_that("ggside themes functions work", {

  expect_doppelganger("grey", p + theme_ggside_grey())
  expect_doppelganger("gray", p + theme_ggside_gray())
  expect_doppelganger("bw", p + theme_ggside_bw())
  expect_doppelganger("linedraw", p + theme_ggside_linedraw())
  expect_doppelganger("light", p + theme_ggside_light())
  expect_doppelganger("dark", p + theme_ggside_dark())
  expect_doppelganger("minimal", p + theme_ggside_minimal())
  expect_doppelganger("classic", p + theme_ggside_classic())
  expect_doppelganger("void", p + theme_ggside_void())

})


test_that("ggside theme inheritence work", {


  expect_doppelganger("side panel grid", p + theme(ggside.panel.scale = 0.3,
                                        ggside.panel.grid = element_line(linetype = "dotted", color = NA),
                                        ggside.xside.panel.grid.major.x = element_line(color = "red"),
                                        ggside.yside.panel.grid.major.y = element_line(color = "blue")))

  expect_doppelganger("side panel border", p + theme(ggside.panel.scale = 0.3,
                                          ggside.panel.border = element_rect(linetype = "dotted", fill = NA),
                                          ggside.xside.panel.border = element_rect(color = "red"),
                                          ggside.yside.panel.border = element_rect(color = "blue")))

  expect_doppelganger("side panel background", p + theme(ggside.panel.scale = 0.3,
                                                     ggside.panel.background = element_rect(linewidth = 10),
                                                     ggside.xside.panel.background = element_rect(fill = alpha("red", .1)),
                                                     ggside.yside.panel.background = element_rect(fill = alpha("blue", .1))))
})

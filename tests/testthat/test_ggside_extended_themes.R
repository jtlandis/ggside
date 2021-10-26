
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



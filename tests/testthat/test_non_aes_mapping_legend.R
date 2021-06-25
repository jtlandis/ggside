library(ggplot2)
library(ggside)
library(testthat)
library(vdiffr)

context("params aesthetics")

test_that('params aes are not in legend', {
  p <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Width)) +
    geom_point(color = "grey") +
    geom_smooth(aes(color = Species), method = "lm", se = FALSE)

  p2 <- p + geom_xsidedensity(aes(x = Sepal.Length, y = after_stat(density)), color = "blue")
  p3 <- p + geom_xsidedensity(aes(x = Sepal.Length, y = after_stat(density)), xcolor = "red")

  expect_doppelganger("Base Plot", p)
  expect_doppelganger("non-aes-color-blue", p2)
  expect_doppelganger("non-aes-xcolor-red", p3)
})




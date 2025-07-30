test_that("Overwritten `+.gg` still adds layers as expected", {
  p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species))
  expect_equal(length(p$layers), 0L)
  p1 <- p + geom_point()
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p1, "gg")
  expect_equal(length(p1$layers), 1L)
  p2 <- p1 + geom_rug()
  expect_equal(length(p2$layers), 2L)
  expect_identical(
    {
      p2 + theme_bw()
    }$theme,
    theme_bw()
  )
  p3 <- p2 + facet_wrap(~Species)
  expect_s3_class(p3$facet, "FacetWrap")
  p4 <- p2 + facet_wrap(~Species, scales = "free_y")
  expect_s3_class(p4$facet, "FacetWrap")
  expect_false(identical(p3$facet, p4$facet))
  expect_false(identical(p3$scales, p4$scales))
})

p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point()

test_that("New ggside layers are added correctly", {
  expect_s3_class(p, "ggplot")
  expect_s3_class(ggside(), "ggside_options")
  p1 <- p + geom_xsidedensity(aes(y = after_stat(density)))
  expect_s3_class(p1, "ggside")
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p1$layers[[2]], "ggside_layer")
  expect_s3_class(p1$layers[[2]]$geom, "GeomXsidedensity")
  p2 <- p1 + facet_wrap(~Species, ncol = 1)
  expect_s3_class(p1, "ggside")
  expect_s3_class(p2$facet, "FacetWrap")
  p3 <- p2 + ggside(collapse = "all")
  expect_warning(ggplot_build(p3), regexp = "only x used")
  expect_warning(ggplot_build(p + ggside(collapse = "all")), regexp = "no side geometry used")
  expect_warning(ggplot_build(p1 + ggside(collapse = "y")), regex = "no yside geometry used")
})


test_that("add_gg errors", {
  expect_error(+p, "argument \"e2\" is missing, with no default")
  expect_error(p + "", "Can't add `\"\"` to a")
  fake_theme <- structure(numeric(), class = "theme")
  expect_error(theme() + fake_theme, "to a theme object")
  expect_error(ggproto() + p, "Cannot add ggproto objects together")
})

test_that("add_gg identities", {
  expect_identical(p + NULL, p)
  expect_identical(theme() + theme_bw(), theme_bw())
})

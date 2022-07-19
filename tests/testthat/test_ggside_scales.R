library(vdiffr)

test_that("xsidey and ysidex appear",{
  p <- ggplot(mpg, aes(displ, hwy, colour = class)) +
    geom_point(size = 2) +
    geom_xsidedensity(aes(y = after_stat(density)), position = "stack") +
    geom_ysidedensity(aes(x = after_stat(density)), position = "stack") +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))

  expect_null(p$ggside$xsidey)
  expect_null(p$ggside$ysidex)

  xsidey_scale <- scale_xsidey_continuous(breaks = c(0,1,2))
  p <- p + xsidey_scale

  expect_identical(p$ggside$xsidey, xsidey_scale)

  ysidex_scale <- scale_ysidex_continuous(breaks = NULL, labels = NULL)
  p <- p + ysidex_scale

  expect_identical(p$ggside$ysidex, ysidex_scale)

  expect_doppelganger("xsidey-ysidex-FacetNull", p)

  p <- p + ggside(collapse = "all")

  expect_doppelganger("xsidey-ysidex-FacetWrap", p + facet_wrap(~manufacturer))

  expect_doppelganger("xsidey-ysidex-FacetGrid", p + facet_grid(rows = vars(class)))

})



test_that("xsidey and ysidex no warning", {


  p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Species)) +
    geom_point(aes(color = Species)) +
    geom_xsidedensity(alpha = .3, position = "stack") +
    geom_ysideboxplot(aes(x = Species), orientation = "x")

  expect_false(inherits(p$scales, "ggsideScalesList"))

  p <- p +
    scale_ysidex_discrete(guide = guide_axis(angle = 45))

  expect_true(inherits(p$scales, "ggsideScalesList"))

  expect_warning(p, NA)

  p <- ggplot(iris, aes(Species, Sepal.Length, color = Species)) +
    geom_boxplot() +
    geom_ysidepoint(aes(x = Petal.Length)) + scale_ysidex_continuous()

  expect_warning(p, NA)

})

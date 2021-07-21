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

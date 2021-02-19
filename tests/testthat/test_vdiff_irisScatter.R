
context("Iris scatterplot density type plots")
i2 <- mutate(iris, Species2 = rep(c("A","B"), 75))
p <- ggplot(i2, aes(Sepal.Width, Sepal.Length, fill = Species)) +
  geom_point(aes(color = Species))

test_that("sidedensities plot correctly", {
  p1 <- p +
    geom_xsidedensity(aes(y = after_stat(density)), alpha = .3) +
    geom_ysidedensity(aes(x = after_stat(density)), alpha = .3)
  expect_doppelganger("Basic Side Density", p1)
  p2 <- p +
    geom_xsidedensity(aes(y = after_stat(density)), position = "stack") +
    geom_ysidedensity(aes(x = after_stat(density), yfill = Species2), position = "stack")
  expect_doppelganger("Stacked Side Density", p2)
  p3 <- p2 +
    facet_grid(cols = vars(Species), rows = vars(Species2)) +
    scale_yfill_manual(values = c("darkred","darkblue"))
  expect_doppelganger("FacetGrid Side Density", p3)
  p4 <- p3 + ggside(collapse = "all")
  expect_doppelganger("FacetGrid Collapsed Density", p4)

})

test_that("sidehistograms plot correctly", {
  p1 <- p +
    geom_xsidehistogram(aes(y = after_stat(count))) +
    geom_ysidehistogram(aes(x = after_stat(count)))
  expect_doppelganger("Basic Side histogram", p1)
  p2 <- p +
    geom_xsidehistogram(aes(xfill = Species, y = after_stat(count))) +
    scale_xfill_manual(values = c("tan2","gold","grey")) +
    geom_ysidehistogram(aes(x = after_stat(count), yfill = Species2))
  expect_doppelganger("Side histogram xcolor changed - legend static", p2)
  p3 <- p2 +
    facet_grid(cols = vars(Species), rows = vars(Species2)) +
    scale_yfill_manual(values = c("darkred","darkblue")) +
    guides(fill = F)
  expect_doppelganger("FacetGrid Side histogram - xyside legends visible", p3)
  p4 <- p3 + ggside(collapse = "all")
  expect_doppelganger("FacetGrid Collapsed histogram", p4)
})



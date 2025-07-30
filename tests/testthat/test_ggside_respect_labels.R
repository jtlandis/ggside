library(vdiffr)

df <- data.frame(x = 1:10, y = 21:30,
                 a = rep(c("g1","g2"), 5),
                 b = rep(c("t1","t2"), each = 5))
p <- ggplot(df,  aes(x, y)) +
  geom_point()

# removing y axis from main to see
# the affects of the spacing more clearly
px <- p + geom_xsidecol(width = 0.9) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme_ggside_gray()

py <- p + geom_ysidecol(width = 0.9) +
  theme(ggside.axis.text.x = element_text(angle = 90, vjust = .5))

pxy <- p + geom_xsidecol(width = 0.9) +
  geom_ysidecol(width = 0.9) +
  theme(ggside.axis.text.x = element_text(angle = 90, vjust = .5))

test_that("ggside `respect_side_labels` works on xsides", {
  expect_doppelganger("xside respect default", px)
  expect_doppelganger("xside respect x", px + ggside(respect_side_labels = "x"))
  expect_doppelganger("xside respect y", px + ggside(respect_side_labels = "y"))
  expect_doppelganger("xside respect all", px + ggside(respect_side_labels = "all"))
  expect_doppelganger("xside respect none", px + ggside(respect_side_labels = "none"))
})


test_that("ggside `respect_side_labels` works on ysides", {
  expect_doppelganger("yside respect default", py)
  expect_doppelganger("yside respect x", py + ggside(respect_side_labels = "x"))
  expect_doppelganger("yside respect y", py + ggside(respect_side_labels = "y"))
  expect_doppelganger("yside respect all", py + ggside(respect_side_labels = "all"))
  expect_doppelganger("yside respect none", py + ggside(respect_side_labels = "none"))
})


test_that("ggside `respect_side_labels` works as expected on x and y sides", {
  expect_doppelganger("xyside respect long ysidex label", pxy)
  pxy_l <- pxy + ggside(y.pos = "left")
  pxy_b <- pxy + ggside(x.pos = "bottom") + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    theme_ggside_gray()
  expect_doppelganger("xyside yl respect default", pxy_l)
  expect_doppelganger("xyside yl respect x", pxy_l + ggside(respect_side_labels = "x"))
  expect_doppelganger("xyside yl respect y", pxy_l + ggside(respect_side_labels = "y"))
  expect_doppelganger("xyside yl respect all", pxy_l + ggside(respect_side_labels = "all"))
  expect_doppelganger("xyside yl respect none", pxy_l + ggside(respect_side_labels = "none"))
  expect_doppelganger("xyside xb respect default", pxy_b)
  expect_doppelganger("xyside xb respect  x", pxy_b + ggside(respect_side_labels = "x"))
  expect_doppelganger("xyside xb respect  y", pxy_b + ggside(respect_side_labels = "y"))
  expect_doppelganger("xyside xb respect  all", pxy_b + ggside(respect_side_labels = "all"))
  expect_doppelganger("xyside xb respect  none", pxy_b + ggside(respect_side_labels = "none"))


})

test_that("ggside `respect_side_labels` works as expected with other parameters and facets", {
  pxyf <- pxy + facet_grid(vars(a), vars(b))
  expect_doppelganger("xyside facet-Grid respect default", pxyf)
  expect_doppelganger("xyside facet-Grid respect none", pxyf + ggside(respect_side_labels = "none"))
  expect_doppelganger("xyside facet-Grid respect none free scales", pxyf + ggside(respect_side_labels = "none", scales = "free"))
  expect_doppelganger("xyside facet-Grid respect default free scales", pxyf + ggside(respect_side_labels = "default", scales = "free"))
})

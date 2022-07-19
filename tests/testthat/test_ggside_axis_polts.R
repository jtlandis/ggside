library(vdiffr)

df <- data.frame(x = 1:10, y = 21:30,
                 a = rep(c("g1","g2"), 5),
                 b = rep(c("t1","t2"), each = 5))
p <- ggplot(df, aes(x, y)) +
  geom_point()
test_that("base plot did not change",{
  expect_doppelganger("base plot", p)
})

px <- p + geom_xsidecol()
py <- p + geom_ysidecol()

test_that("ggside x-axis plotting",{
  expect_doppelganger("xside top", px)
  pxb <- px + ggside(x.pos = "bottom")
  expect_doppelganger("xside bottom", pxb)
  expect_doppelganger("xside top-pos-top", px + scale_x_continuous(position = "top"))
  expect_doppelganger("xside bot-pos-top", pxb + scale_x_continuous(position = "top"))
  expect_doppelganger("xside top-pos-top-wrap", px + scale_x_continuous(position = "top") + facet_wrap(a~b))
  expect_doppelganger("xside bot-pos-top-grid", pxb + scale_x_continuous(position = "top") + facet_grid(vars(a), vars(b)))
  expect_doppelganger("xside top-noaxis", px + theme(axis.text.x = element_blank()))
})

test_that("ggside y-axis plotting",{
  expect_doppelganger("yside right", py)
  pyl <- py + ggside(y.pos = "left")
  expect_doppelganger("yside left", pyl)
  expect_doppelganger("yside right-pos-right", py + scale_y_continuous(position = "right"))
  expect_doppelganger("yside left-pos-right", pyl + scale_y_continuous(position = "right"))
  expect_doppelganger("yside right-pos-right-wrap", py + scale_y_continuous(position = "right") + facet_wrap(a~b))
  expect_doppelganger("yside left-pos-right-grid", pyl + scale_y_continuous(position = "right") + facet_grid(vars(a), vars(b)))
  expect_doppelganger("yside right-noaxis", py + theme(axis.text.y = element_blank()))
})

pxy <- p + geom_xsidecol() + geom_ysidecol()

test_that("ggside xy-axis plotting", {
  expect_doppelganger("xyside", pxy)
  pxy_l <- pxy + ggside(y.pos = "left")
  pxy_b <- pxy + ggside(x.pos = "bottom")
  expect_doppelganger("xyside yl", pxy_l)
  expect_doppelganger("xyside yl-pos-right", pxy_l + scale_y_continuous(position = "right"))
  expect_doppelganger("xyside xb", pxy_b)
  expect_doppelganger("xyside xb-pos-top", pxy_b + scale_x_continuous(position = "top"))
  pxy_lb <- pxy + ggside(y.pos = "left", x.pos = "bottom")
  expect_doppelganger("xyside lb", pxy_lb)
  expect_doppelganger("xyside lb-pos-swap", pxy_lb + scale_x_continuous(position = "top")+scale_y_continuous(position = "right"))
  expect_doppelganger("xyside no-x-text", pxy + theme(axis.text.x = element_blank()))
  expect_doppelganger("xyside no-y-text", pxy + theme(axis.text.y = element_blank()))
  expect_doppelganger("xyside facet-Grid", pxy + facet_grid(vars(a), vars(b)))
})


test_that("ggside FacetGrid strip option works", {

  expect_doppelganger("xyside strip main", pxy + facet_grid(vars(a), vars(b)) + ggside(strip = "main"))
})

df <- data.frame(
  x = 1:10, y = 21:30,
  a = rep(c("g1", "g2"), 5),
  b = rep(c("t1", "t2"), each = 5)
)
p <- ggplot(df, aes(x, y)) +
  geom_point() +
  facet_wrap(a ~ b)
px <- p + geom_xsidecol(width = 0.9)
py <- p + geom_ysidecol(width = 0.9)
pxy <- px + geom_ysidecol(width = 0.9)
test_that("ggside.panel.scale facetWrap", {
  expect_doppelganger("wrap xside ggside.panel.scale.x .5", px + theme(ggside.panel.scale.x = .5))
  expect_doppelganger("wrap xside ggside.panel.scale.y .5", px + theme(ggside.panel.scale.y = .5))
  expect_doppelganger("wrap xside ggside.panel.scale .5", px + theme(ggside.panel.scale = .5))
  expect_doppelganger("wrap yside ggside.panel.scale.x .5", py + theme(ggside.panel.scale.x = .5))
  expect_doppelganger("wrap yside ggside.panel.scale.y .5", py + theme(ggside.panel.scale.y = .5))
  expect_doppelganger("wrap yside ggside.panel.scale .5", py + theme(ggside.panel.scale = .5))
  expect_doppelganger("wrap xyside ggside.panel.scale.x .5", pxy + theme(ggside.panel.scale.x = .5))
  expect_doppelganger("wrap xyside ggside.panel.scale.y .5", pxy + theme(ggside.panel.scale.y = .5))
  expect_doppelganger("wrap xyside ggside.panel.scale .5", pxy + theme(ggside.panel.scale = .5))
})

test_that("ggside.panel.spacing facetWrap", {
  expect_doppelganger("wrap xside ggside.panel.spacing.x 10pt", px + theme(ggside.panel.spacing.x = unit(10, "pt")))
  expect_doppelganger("wrap xside ggside.panel.spacing.y 10pt", px + theme(ggside.panel.spacing.y = unit(10, "pt")))
  expect_doppelganger("wrap xside ggside.panel.spacing 10pt", px + theme(ggside.panel.spacing = unit(10, "pt")))
  expect_doppelganger("wrap yside ggside.panel.spacing.x 10pt", py + theme(ggside.panel.spacing.x = unit(10, "pt")))
  expect_doppelganger("wrap yside ggside.panel.spacing.y 10pt", py + theme(ggside.panel.spacing.y = unit(10, "pt")))
  expect_doppelganger("wrap yside ggside.panel.spacing 10pt", py + theme(ggside.panel.spacing = unit(10, "pt")))
  expect_doppelganger("wrap xyside ggside.panel.spacing.x 10pt", pxy + theme(ggside.panel.spacing.x = unit(10, "pt")))
  expect_doppelganger("wrap xyside ggside.panel.spacing.y 10pt", pxy + theme(ggside.panel.spacing.y = unit(10, "pt")))
  expect_doppelganger("wrap xyside ggside.panel.spacing 10pt", pxy + theme(ggside.panel.spacing = unit(10, "pt")))
})

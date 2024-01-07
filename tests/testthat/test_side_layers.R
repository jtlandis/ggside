library(vdiffr)

base_plot <- ggplot(
  data.frame(x = 1:3, y = 1:3),
  aes(x = x, y = y)
) + geom_point()

cont <- base_plot + ggside() +
  scale_xsidey_continuous() +
  scale_ysidex_continuous()

disc <- base_plot + ggside() +
  scale_ysidex_discrete() +
  scale_ysidex_discrete()

test_that("plot abline", {
  abline <- cont + geom_xsideabline(slope = 1/3) +
    geom_ysideabline(slope = 3)

  expect_doppelganger("abline", abline)
})

test_that("plot bar", {
  bar <- cont + geom_xsidebar() + geom_ysidebar()

  expect_doppelganger("bar", bar)
})

test_that("plot boxplot", {
  boxplot1 <- cont +
    geom_xsideboxplot(aes(group = 1L), orientation = "y") +
    geom_ysideboxplot(aes(group = 1L), orientation = "x")

  expect_doppelganger("boxplot", boxplot1)

  boxplot2 <- cont +
    geom_xsideboxplot(aes(group = 1L), orientation = "x") +
    geom_ysideboxplot(aes(group = 1L), orientation = "y")

  expect_doppelganger("boxplot", boxplot2)
})

test_that("plot col", {
  col <- cont +
    geom_xsidecol() + geom_ysidecol()
  expect_doppelganger("col", col)
})

test_that("plot density", {
  density <- cont +
    geom_xsidedensity() +
    geom_ysidedensity()
  expect_doppelganger("density", density)
})

test_that("plot freqpoly", {
  freqpoly <- cont +
    geom_ysidefreqpoly() +
    geom_xsidefreqpoly()
  expect_doppelganger("freqpoly", freqpoly)
})

test_that("plot Function", {
  Function <- cont +
    geom_xsidefunction(fun = dnorm, args = list(mean = 2, sd = .5)) +
    geom_ysidefunction(fun = dnorm, args = list(mean = 2, sd = .5))
  expect_doppelganger("Function", Function)
})

test_that("plot histogram", {
  histogram <- cont +
    geom_xsidehistogram(bins = 6) +
    geom_ysidehistogram(bins = 6)
  expect_doppelganger("histogram", histogram)
})

test_that("plot hline", {
  hline <- cont +
    geom_xsidehline(yintercept = 2) +
    geom_ysidehline(yintercept = 2)
  expect_doppelganger("hline", hline)
})

test_that("plot label", {
  label <- cont +
    geom_xsidelabel(aes(y = 2, label=letters[1:3])) +
    geom_ysidelabel(aes(x = 2, label=letters[1:3]))
  expect_doppelganger("label", label)
})

test_that("plot line", {
  line <- cont +
    geom_xsideline() +
    geom_ysideline()
  expect_doppelganger("line", line)
})

test_that("plot path", {
  path <- cont +
    geom_xsidepath() +
    geom_ysidepath()
  expect_doppelganger("path", path)
})

test_that("plot point", {
  point <- cont +
    geom_xsidepoint() +
    geom_ysidepoint()
  expect_doppelganger("point", point)
})


test_that("plot segment", {
  segment <- cont +
    geom_xsidesegment(aes(xend = x, yend = y + 1)) +
    geom_ysidesegment(aes(xend = x, yend = y + 1))
  expect_doppelganger("segment", segment)
})

test_that("plot text", {
  text <- cont +
    geom_xsidetext(aes(y = 2, label=letters[1:3])) +
    geom_ysidetext(aes(x = 2, label=letters[1:3]))
  expect_doppelganger("text", text)
})

test_that("plot tile", {
  tile <- disc +
    geom_xsidetile(aes(y = 2, fill = 1:3)) +
    geom_ysidetile(aes(x = 2, fill = 1:3))
  expect_doppelganger("tile", tile)
})

test_that("plot violin", {
  violin <- cont +
    geom_xsideviolin() +
    geom_ysideviolin()
  expect_doppelganger("violin", violin)
})

test_that("plot vline", {
  vline <- cont +
    geom_xsidevline(xintercept = 2) +
    geom_ysidevline(xintercept = 2)
  expect_doppelganger("vline", vline)
})

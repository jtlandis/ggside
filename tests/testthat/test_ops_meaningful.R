

p <- mtcars |>
  dplyr::mutate(cyl = as.factor(cyl)) |>
  ggplot(aes(mpg, hp)) +
  geom_point()



test_that("non-mapping parameter works (alpha)", {
  .addSide <- function(p, my_alpha) {
    p + geom_ysideboxplot(
      aes(x = cyl, y = hp, fill = cyl), orientation = "x",
      alpha = my_alpha
    )
  }
  expect_doppelganger("alpha-0.5", p + geom_ysideboxplot(
    aes(x = cyl, y = hp, fill = cyl), orientation = "x",
    alpha = 0.5
  ))
  expect_doppelganger("alpha-0.5-from-function", .addSide(p, 0.5))
})


test_that("No Ops meaningful warning", {

  p2 <- p + geom_ysideboxplot(
    aes(x = cyl, y = hp, fill = cyl), orientation = "x"
  )
  expect_no_warning(invisible(ggplot_build(p2)))

})

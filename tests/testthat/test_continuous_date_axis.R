suppressMessages(library(dplyr))
library(vdiffr)

set.seed(1234)

df <- data.frame(
  year = sample(2000:2020, 300, replace = T),
  month = sample(1:12, 300, replace = T),
  day = sample(1:28, 300, replace = T),
  temperature = rnorm(300, 70, 8)
) %>%
  mutate(
    month_name = month.name[month],
    date = as.Date(sprintf("%04d-%02d-%02d", year, month, day))
  )

test_that("default ggplot2 warning", {
  p <- ggplot(df, aes(y = temperature)) +
    geom_line(aes(x = date)) +
    geom_histogram(orientation = "y", binwidth = 0.5)
  expect_warning(ggplot_build(p), regexp = "A <numeric> value was passed to a Date scale")
})

p <- ggplot(df, aes(x = date, y = temperature)) +
  geom_line() +
  geom_point(aes(color = month_name))
p_yside <- p + geom_ysidehistogram(bins = 30)

test_that("ggside work-around works", {
  expect_doppelganger("date_x_yside_no_scale", p_yside)
})

p_yside <- p_yside + scale_ysidex_continuous()

test_that("ggside adding ysidex continuous scale", {
  expect_doppelganger("date_x_yside", p_yside)
})

p_xside <- p + geom_xsidehistogram(bins = 30) + scale_xsidey_continuous(trans = "sqrt", breaks = c(0, 5, 10, 20))

test_that("ggside xsidey scales", {
  expect_doppelganger("date_x_xside", p_xside)
})

p_both <- p_yside + geom_xsidehistogram(bins = 30)

test_that("ggside xsidey and ysidex scales", {
  expect_doppelganger("date_x_both", p_both)
})

p_wrap <- p_both + facet_wrap(~month) + ggside(collapse = "all")


test_that("ggside xsidey and ysidex no message", {
  expect_no_message(p_wrap)
})

suppressMessages(library(dplyr))
library(vdiffr)

set.seed(1234)

df <- data.frame(year = sample(2000:2020, 300, replace = T),
                 month = sample(1:12, 300, replace = T),
                 day = sample(1:28,300, replace = T),
                 temperature = rnorm(300, 70, 8)) %>%
  mutate(
    month_name = month.name[month],
    date = as.Date(sprintf("%04d-%02d-%02d",year, month, day))
  )

test_that("default ggplot2 error",{
  p <- ggplot(df, aes(y = temperature)) +
    geom_line(aes(x = date)) +
    geom_histogram(orientation = "y",binwidth = 0.5)
  expect_error(ggplot_build(p), regexp = "date_trans works with objects of class Date only")
})

test_that("ggside work-around works",{
  p <- ggplot(df, aes(x = date, y = temperature)) +
    geom_line() +
    geom_point(aes(color = month_name))
  p_yside <- p + geom_ysidehistogram(bins = 30)
  expect_error(ggplot_build(p_yside), regexp = "date_trans works with objects of class Date only")
  p_yside <- p_yside + scale_ysidex_continuous()
  expect_doppelganger("date_x_yside", p_yside)
  p_xside <- p + geom_xsidehistogram(bins = 30) + scale_xsidey_continuous(trans = "sqrt") +theme(ggside.panel.scale = .3)
  expect_doppelganger("date_x_xside", p_xside)
  p_both <- p_yside + geom_xsidehistogram(bins = 30)
  expect_doppelganger("date_x_both", p_both)
  p_wrap <- p_both + facet_wrap(~month) + ggside(collapse = "all")
  expect_message(p_wrap, regexp = NA)

})

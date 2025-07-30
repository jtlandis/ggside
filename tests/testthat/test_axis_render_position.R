library(vdiffr)

test_that("axis may be rendered in between plots", {
  p <- ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
    geom_point() +
    geom_xsidedensity(aes(color = Species)) +
    geom_ysidedensity()

  expect_doppelganger("default", p)

  expect_doppelganger("x-on-side", p + ggside(draw_x_on = "side"))

  expect_doppelganger("x-on-main-top", p + ggside(draw_x_on = "main") +
                        scale_x_continuous(position = "top"))
  expect_doppelganger("x-on-side-pos-bot", p + ggside(draw_x_on = "side", x.pos = "bottom") +
                        scale_x_continuous(position = "top"))
  expect_doppelganger("x-on-main-pos-bot", p + ggside(draw_x_on = "main", x.pos = "bottom"))

  expect_doppelganger("y-on-side", p + ggside(draw_y_on = "side"))

  expect_doppelganger("y-on-main-right", p + ggside(draw_y_on = "main") +
                        scale_y_continuous(position = "right"))
  expect_doppelganger("y-on-side-pos-left", p + ggside(draw_y_on = "side", y.pos = "left") +
                        scale_y_continuous(position = "right"))
  expect_doppelganger("y-on-main-pos-left", p + ggside(draw_y_on = "main", y.pos = "left"))

  #Facet-wrap
  .p <- p
  p <- p + facet_wrap(~Species)
  expect_doppelganger("wrap-default", p)

  expect_doppelganger("wrap-x-on-side", p + ggside(draw_x_on = "side"))

  expect_doppelganger("wrap-x-on-main-top", p + ggside(draw_x_on = "main") +
                        scale_x_continuous(position = "top"))
  expect_doppelganger("wrap-x-on-side-pos-bot", p + ggside(draw_x_on = "side", x.pos = "bottom") +
                        scale_x_continuous(position = "top"))
  expect_doppelganger("wrap-x-on-main-pos-bot", p + ggside(draw_x_on = "main", x.pos = "bottom"))

  expect_doppelganger("wrap-y-on-side", p + ggside(draw_y_on = "side"))

  expect_doppelganger("wrap-y-on-main-right", p + ggside(draw_y_on = "main") +
                        scale_y_continuous(position = "right"))
  expect_doppelganger("wrap-y-on-side-pos-left", p + ggside(draw_y_on = "side", y.pos = "left") +
                        scale_y_continuous(position = "right"))
  expect_doppelganger("wrap-y-on-main-pos-left", p + ggside(draw_y_on = "main", y.pos = "left"))

  #Facet-grid
  p <- .p + facet_grid(rows = vars(Species)) + ggside(collapse = "all")
  expect_doppelganger("grid-default", p)

  expect_doppelganger("grid-x-on-side", p + ggside(draw_x_on = "side"))

  expect_doppelganger("grid-x-on-main-top", p + ggside(draw_x_on = "main") +
                        scale_x_continuous(position = "top"))
  expect_doppelganger("grid-x-on-side-pos-bot", p + ggside(draw_x_on = "side", x.pos = "bottom") +
                        scale_x_continuous(position = "top"))
  expect_doppelganger("grid-x-on-main-pos-bot", p + ggside(draw_x_on = "main", x.pos = "bottom"))

  expect_doppelganger("grid-y-on-side", p + ggside(draw_y_on = "side"))

  expect_doppelganger("grid-y-on-main-right", p + ggside(draw_y_on = "main") +
                        scale_y_continuous(position = "right"))
  expect_doppelganger("grid-y-on-side-pos-left", p + ggside(draw_y_on = "side", y.pos = "left") +
                        scale_y_continuous(position = "right"))
  expect_doppelganger("grid-y-on-main-pos-left", p + ggside(draw_y_on = "main", y.pos = "left"))
})

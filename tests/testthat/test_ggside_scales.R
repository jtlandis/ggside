library(vdiffr)

ggproto_members <- function(proto) {
  members <- ls(envir = proto)
  super <- proto$super
  while(!is.null(super)) {
    members <- union(members, ls(envir = super()))
    super <- super()$super
  }
  setdiff(members, "super")
}

extract_ggproto_members <- function(proto, members) {
  objs <- lapply(members, function(mem, x) x[[mem]], x = proto)
  for (i in seq_along(objs)) {
    obj <- objs[[i]]
    if (is.ggproto(obj))
      objs[[i]] <- extract_ggproto_members(obj, ggproto_members(obj))
    if (is.function(obj))
      objs[[i]] <- environment(obj)$f
  }
  objs
}

expect_ggproto_id <- function(object, expected) {
  act <- quasi_label(enquo(object), arg = "object")
  exp <- quasi_label(enquo(expected), arg = "expected")

  members_act <- ggproto_members(act$val)
  members_exp <- ggproto_members(exp$val)
  identical_members <- all(members_exp %in% members_act) &&
    all(members_act %in% members_exp)

  vals_act <- extract_ggproto_members(act$val, members_exp)
  vals_exp <- extract_ggproto_members(exp$val, members_exp)

  comp <- waldo::compare(x = vals_act, y = vals_exp,
                         x_arg = "object", y_arg = "expected")
  expect(length(comp) == 0,
         sprintf("%s (%s) not %s to %s (%s).\n\n%s",
                 act$lab, "`actual`", "identical", exp$lab, "`expected`",
                 paste0(comp,collapse = "\n\n")),
         info = NULL, trace_env = parent.frame())

}

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

  expect_ggproto_id(p$ggside$xsidey, xsidey_scale)

  ysidex_scale <- scale_ysidex_continuous(breaks = NULL, labels = NULL)
  p <- p + ysidex_scale

  expect_ggproto_id(p$ggside$ysidex, ysidex_scale)

  expect_doppelganger("xsidey-ysidex-FacetNull", p)

  p <- p + ggside(collapse = "all")

  expect_doppelganger("xsidey-ysidex-FacetWrap", p + facet_wrap(~manufacturer))

  expect_doppelganger("xsidey-ysidex-FacetGrid", p + facet_grid(rows = vars(class)))

})



test_that("xsidey and ysidex no warning", {


  p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Species)) +
    geom_point(aes(color = Species)) +
    geom_xsidedensity(alpha = .3, position = "stack") +
    geom_ysideboxplot(aes(x = Species), orientation = "x")

  # adding a scale no longer converts the scale object
  #expect_false(inherits(p$scales, "ggsideScalesList"))

  p <- p +
    scale_ysidex_discrete(guide = guide_axis(angle = 45))

  #expect_true(inherits(p$scales, "ggsideScalesList"))

  expect_warning(p, NA)

  p <- ggplot(iris, aes(Species, Sepal.Length, color = Species)) +
    geom_boxplot() +
    geom_ysidepoint(aes(x = Petal.Length)) + scale_ysidex_continuous()

  expect_warning(p, NA)

})

test_that("side scales can use transforms", {
  df <- data.frame(
    x = seq(from = 20, to = 60, by = 1),
    y1 = seq(from = 0.001, to = 1, length.out = 41),
    y2 = seq(from = 5000, to = 1, length.out = 41)
  )

  p <- ggplot(data = df) +
    geom_line(mapping = aes(x = x, y = y1, group = "obs1")) +
    geom_xsideline(mapping = aes(x = x, y = y2)) +
    scale_xsidey_log10(expand = c(0, 0))

  expect_doppelganger("transformation works", p)
})

test_that("side scales can use guide's argument", {
  p <- ggplot(mpg, aes(displ, hwy, colour = class)) +
    geom_point(size = 2) +
    geom_xsideboxplot(aes(y =class), orientation = "y") +
    geom_ysidedensity(aes(x = after_stat(density)), position = "stack") +
    theme(ggside.panel.scale = .3) +
    scale_xsidey_discrete() +
    scale_ysidex_continuous(guide = guide_axis(angle = 90), minor_breaks = NULL)
  expect_warning(p, NA)
})

test_that("coord_cartesian(xlim = <limits>, ylim = <limits>) works", {
  p <- ggplot(mpg, aes(displ, hwy, colour = class)) +
    geom_point(size = 2) +
    geom_xsidedensity(aes(y = after_stat(density)), position = "stack") +
    geom_ysidedensity(aes(x = after_stat(density)), position = "stack") +
    theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
    coord_cartesian(xlim = c(3, 6), ylim = c(20, 30))
  expect_doppelganger("coord_cartesian-no-zoom", p)
})

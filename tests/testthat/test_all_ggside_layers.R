library(vdiffr)
df <- data.frame(
  x = 1:10, y = 21:30,
  a = rep(c("g1", "g2"), 5),
  b = rep(c("t1", "t2"), each = 5)
)
df <- rbind(
  df,
  df[3:8, ],
  df[4:7, ],
  df[5:6, ],
  df[5:6, ]
)
p <- ggplot(df, aes(x, y)) +
  geom_point()

expect_side_ <- function(plot, context, layers, name) {
  expect_doppelganger(
    sprintf("%s %s", context, name),
    plot + layers
  )
}

expect_side_abline <- function(plot, context) {
  expect_side_(
    plot,
    context,
    list(
      geom_xsideabline(slope = 1 / 10),
      geom_ysideabline(slope = 10, intercept = 20)
    ),
    "abline"
  )
}

expect_side_bar <- function(plot, context) {
  expect_side_(
    plot,
    context = sprintf("%s with bar", context),
    list(geom_xsidebar(), geom_ysidebar()),
    "bar"
  )
}

expect_side_boxplot <- function(plot, context) {
  expect_side_(
    plot,
    context,
    # aes(group = ...) is necessary
    # for no warning
    list(
      geom_xsideboxplot(
        aes(group = 1L)
      ),
      geom_ysideboxplot(
        aes(group = 1L)
      )
    ),
    "boxplot default"
  )
  expect_side_(
    plot,
    context,
    list(
      geom_xsideboxplot(aes(group = 1L), orientation = "y"), geom_ysideboxplot(aes(group = 1L), orientation = "x")
    ),
    "boxplot oriented"
  )

  expect_side_(
    plot,
    context,
    list(
      geom_xsideboxplot(
        aes(y = a),
        orientation = "y"
      ),
      geom_ysideboxplot(
        aes(x = b),
        orientation = "x"
      )
    ),
    "boxplot oriented groups"
  )
}

expect_side_col <- function(plot, context) {
  expect_side_(
    plot,
    context,
    list(geom_xsidecol(), geom_ysidecol()),
    "col"
  )
}

expect_side_density <- function(plot, context) {
  expect_side_(
    plot,
    context,
    list(geom_xsidedensity(), geom_ysidedensity()),
    "density"
  )
}

expect_side_freqpoly <- function(plot, context) {
  expect_side_(
    plot,
    context,
    list(
      geom_ysidefreqpoly(bins = 5),
      geom_xsidefreqpoly(bins = 19)
    ),
    "freqpoly"
  )
}

expect_side_function <- function(plot, context) {
  expect_side_(
    plot,
    context,
    list(
      geom_xsidefunction(fun = dnorm, args = list(mean = 5.5, sd = 1)),
      geom_ysidefunction(fun = dnorm, args = list(mean = 26, sd = 1.5))
    ),
    "function"
  )
}

expect_side_histogram <- function(plot, context) {
  expect_side_(
    plot,
    context,
    list(
      geom_xsidehistogram(bins = 5),
      geom_ysidehistogram(bins = 5)
    ),
    "histogram"
  )
}

expect_side_hline <- function(plot, context) {
  expect_side_(
    plot,
    context,
    list(
      geom_xsidehline(yintercept = 2),
      geom_ysidehline(yintercept = 25)
    ),
    "hline"
  )
}

expect_side_label <- function(plot, context) {
  expect_side_(
    plot,
    context,
    list(
      geom_xsidelabel(
        aes(y = 2, label = a)
      ),
      geom_ysidelabel(
        aes(x = 2, label = b)
      )
    ),
    "label"
  )
}

expect_side_line <- function(plot, context) {
  expect_side_(
    plot,
    context,
    list(
      geom_xsideline(),
      geom_ysideline()
    ),
    "line"
  )
}

expect_side_path <- function(plot, context) {
  expect_side_(
    plot,
    context,
    list(
      geom_xsidepath(),
      geom_ysidepath()
    ),
    "path"
  )
}

expect_side_point <- function(plot, context) {
  expect_side_(
    plot,
    context,
    list(
      geom_xsidepoint(),
      geom_ysidepoint()
    ),
    "point"
  )
}

expect_side_segment <- function(plot, context) {
  expect_side_(
    plot,
    context,
    list(
      geom_xsidesegment(aes(xend = x + 1, yend = y)),
      geom_ysidesegment(aes(xend = x, yend = y + 1))
    ),
    "segment"
  )
}

expect_side_text <- function(plot, context) {
  expect_side_(
    plot,
    context,
    list(
      geom_xsidetext(aes(y = 2, label = a)),
      geom_ysidetext(aes(x = 2, label = b))
    ),
    "text"
  )
}

expect_side_tile <- function(plot, context) {
  expect_side_(
    plot,
    context,
    list(
      geom_xsidetile(aes(y = 2, fill = x)),
      geom_ysidetile(aes(x = 2, fill = y))
    ),
    "tile"
  )
  expect_side_(
    plot,
    context,
    list(
      geom_xsidetile(aes(y = 2, xfill = x)),
      geom_ysidetile(aes(x = 2, yfill = y))
    ),
    "tile x/yfill"
  )
}

expect_side_violin <- function(plot, context) {
  expect_side_(
    plot,
    context,
    list(
      geom_xsideviolin(),
      geom_ysideviolin()
    ),
    "violin"
  )
}

expect_side_vline <- function(plot, context) {
  expect_side_(
    plot,
    context,
    list(
      geom_xsidevline(xintercept = 5),
      geom_ysidevline(xintercept = 2)
    ),
    "vline"
  )
}


test_side_layers <- function(plot, context) {
  testthat::test_that(context, {
    expect_side_abline(plot, context)
    expect_side_bar(plot, context)
    expect_side_boxplot(plot, context)
    expect_side_col(plot, context)
    expect_side_density(plot, context)
    expect_side_freqpoly(plot, context)
    expect_side_function(plot, context)
    expect_side_histogram(plot, context)
    expect_side_hline(plot, context)
    expect_side_label(plot, context)
    expect_side_line(plot, context)
    expect_side_path(plot, context)
    expect_side_point(plot, context)
    expect_side_segment(plot, context)
    expect_side_text(plot, context)
    expect_side_tile(plot, context)
    expect_side_violin(plot, context)
    expect_side_vline(plot, context)
  })
}


test_side_layers(p, "plain ggplot")
test_side_layers(p + facet_wrap(~a), "facet_wrap")
test_side_layers(p + facet_grid(vars(a), vars(b)), "facet_grid")
test_side_layers(
  p +
    facet_wrap(~a) +
    ggside(collapse = "all"),
  "facet_wrap collapsed"
)
test_side_layers(
  p +
    facet_grid(vars(a), vars(b)) +
    ggside(collapse = "all"),
  "facet_grid collapsed"
)

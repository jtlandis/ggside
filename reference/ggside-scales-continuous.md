# Position scales for continuous data ggside scales

The [xside](https://jtlandis.github.io/ggside/reference/xside.md) and
[yside](https://jtlandis.github.io/ggside/reference/yside.md) variants
of
[scale_x_continuous](https://ggplot2.tidyverse.org/reference/scale_continuous.html)/
[scale_y_continuous](https://ggplot2.tidyverse.org/reference/scale_continuous.html).
scale_xsidey_continuous enables better control on how the y-axis is
rendered on the xside panel and scale_ysidex_continuous enables better
control on how the x-axis is rendered on the yside panel.

## Usage

``` r
scale_xsidey_continuous(
  name = waiver(),
  breaks = waiver(),
  minor_breaks = waiver(),
  n.breaks = NULL,
  labels = waiver(),
  limits = NULL,
  expand = waiver(),
  oob = scales::censor,
  na.value = NA_real_,
  transform = "identity",
  guide = waiver(),
  position = "left",
  sec.axis = waiver()
)

scale_xsidey_log10(...)

scale_xsidey_reverse(...)

scale_xsidey_sqrt(...)

scale_ysidex_log10(...)

scale_ysidex_reverse(...)

scale_ysidex_sqrt(...)

scale_ysidex_log10(...)

scale_ysidex_reverse(...)

scale_ysidex_sqrt(...)
```

## Arguments

- name:

  The name of the scale. Used as the axis or legend title. If
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html), the
  default, the name of the scale is taken from the first mapping used
  for that aesthetic. If `NULL`, the legend title will be omitted.

- breaks:

  One of:

  - `NULL` for no breaks

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    for the default breaks computed by the [transformation
    object](https://scales.r-lib.org/reference/new_transform.html)

  - A numeric vector of positions

  - A function that takes the limits as input and returns breaks as
    output (e.g., a function returned by
    [`scales::extended_breaks()`](https://scales.r-lib.org/reference/breaks_extended.html)).
    Note that for position scales, limits are provided after scale
    expansion. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

- minor_breaks:

  One of:

  - `NULL` for no minor breaks

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    for the default breaks (none for discrete, one minor break between
    each major break for continuous)

  - A numeric vector of positions

  - A function that given the limits returns a vector of minor breaks.
    Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation. When the function has two arguments, it will be
    given the limits and major break positions.

- n.breaks:

  An integer guiding the number of major breaks. The algorithm may
  choose a slightly different number to ensure nice break labels. Will
  only have an effect if `breaks = waiver()`. Use `NULL` to use the
  default number of breaks given by the transformation.

- labels:

  One of the options below. Please note that when `labels` is a vector,
  it is highly recommended to also set the `breaks` argument as a vector
  to protect against unintended mismatches.

  - `NULL` for no labels

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    for the default labels computed by the transformation object

  - A character vector giving labels (must be same length as `breaks`)

  - An expression vector (must be the same length as breaks). See
    ?plotmath for details.

  - A function that takes the breaks as input and returns labels as
    output. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

- limits:

  One of:

  - `NULL` to use the default scale range

  - A numeric vector of length two providing limits of the scale. Use
    `NA` to refer to the existing minimum or maximum

  - A function that accepts the existing (automatic) limits and returns
    new limits. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation. Note that setting limits on positional scales
    will **remove** data outside of the limits. If the purpose is to
    zoom, use the limit argument in the coordinate system (see
    [`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)).

- expand:

  For position scales, a vector of range expansion constants used to add
  some padding around the data to ensure that they are placed some
  distance away from the axes. Use the convenience function
  [`expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html)
  to generate the values for the `expand` argument. The defaults are to
  expand the scale by 5% on each side for continuous variables, and by
  0.6 units on each side for discrete variables.

- oob:

  One of:

  - Function that handles limits outside of the scale limits (out of
    bounds). Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

  - The default
    ([`scales::censor()`](https://scales.r-lib.org/reference/oob.html))
    replaces out of bounds values with `NA`.

  - [`scales::squish()`](https://scales.r-lib.org/reference/oob.html)
    for squishing out of bounds values into range.

  - [`scales::squish_infinite()`](https://scales.r-lib.org/reference/oob.html)
    for squishing infinite values into range.

- na.value:

  Missing values will be replaced with this value.

- transform:

  For continuous scales, the name of a transformation object or the
  object itself. Built-in transformations include "asn", "atanh",
  "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p",
  "log2", "logit", "modulus", "probability", "probit", "pseudo_log",
  "reciprocal", "reverse", "sqrt" and "time".

  A transformation object bundles together a transform, its inverse, and
  methods for generating breaks and labels. Transformation objects are
  defined in the scales package, and are called `transform_<name>`. If
  transformations require arguments, you can call them from the scales
  package, e.g.
  [`scales::transform_boxcox(p = 2)`](https://scales.r-lib.org/reference/transform_boxcox.html).
  You can create your own transformation with
  [`scales::new_transform()`](https://scales.r-lib.org/reference/new_transform.html).

- guide:

  A function used to create a guide or its name. See
  [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html) for
  more information.

- position:

  For position scales, The position of the axis. `left` or `right` for y
  axes, `top` or `bottom` for x axes.

- sec.axis:

  [`sec_axis()`](https://ggplot2.tidyverse.org/reference/sec_axis.html)
  is used to specify a secondary axis.

- ...:

  Other arguments passed on to scale\_(y\|x)side(x\|y)\_continuous()

## Value

ggside_scale object inheriting from ggplot2::ScaleContinuousPosition

## Examples

``` r
library(ggside)
library(ggplot2)
# adding continuous y-scale to the x-side panel, when main panel mapped to discrete data
ggplot(mpg, aes(hwy, class, colour = class)) +
  geom_boxplot() +
  geom_xsidedensity(position = "stack") +
  theme(ggside.panel.scale = .3) +
  scale_xsidey_continuous(minor_breaks = NULL, limits = c(NA, 1))


# If you need to specify the main scale, but need to prevent this from
# affecting the side scale. Simply add the appropriate `scale_*side*_*()` function.
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_xsidehistogram() +
  geom_ysidehistogram() +
  scale_x_continuous(
    breaks = seq(1, 6, 1),
    # would otherwise remove the histogram
    # as they have a lower value of 0.
    limits = (c(1, 6))
  ) +
  scale_ysidex_continuous() # ensures the x-axis of the y-side panel has its own scale.
#> `stat_xsidebin()` using `bins = 30`. Pick better value `binwidth`.
#> `stat_ysidebin()` using `bins = 30`. Pick better value `binwidth`.
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_xsidebar()`).
```

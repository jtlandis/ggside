# Side Violin plots

The [xside](https://jtlandis.github.io/ggside/reference/xside.md) and
[yside](https://jtlandis.github.io/ggside/reference/yside.md) variants
of
[geom_violin](https://ggplot2.tidyverse.org/reference/geom_violin.html)

## Usage

``` r
geom_xsideviolin(
  mapping = NULL,
  data = NULL,
  stat = "ydensity",
  position = "dodge",
  ...,
  trim = TRUE,
  bounds = c(-Inf, Inf),
  quantile.colour = NULL,
  quantile.color = NULL,
  quantile.linetype = 0L,
  quantile.linewidth = NULL,
  scale = "area",
  na.rm = FALSE,
  orientation = "x",
  show.legend = NA,
  inherit.aes = TRUE,
  draw_quantiles = lifecycle::deprecated()
)

geom_ysideviolin(
  mapping = NULL,
  data = NULL,
  stat = "ydensity",
  position = "dodge",
  ...,
  trim = TRUE,
  bounds = c(-Inf, Inf),
  quantile.colour = NULL,
  quantile.color = NULL,
  quantile.linetype = 0L,
  quantile.linewidth = NULL,
  scale = "area",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  orientation = "y",
  draw_quantiles = lifecycle::deprecated()
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- stat:

  Use to override the default connection between
  [`geom_violin()`](https://ggplot2.tidyverse.org/reference/geom_violin.html)
  and
  [`stat_ydensity()`](https://ggplot2.tidyverse.org/reference/geom_violin.html).

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- trim:

  If `TRUE` (default), trim the tails of the violins to the range of the
  data. If `FALSE`, don't trim the tails.

- bounds:

  Known lower and upper bounds for estimated data. Default c(-Inf, Inf)
  means that there are no (finite) bounds. If any bound is finite,
  boundary effect of default density estimation will be corrected by
  reflecting tails outside bounds around their closest edge. Data points
  outside of bounds are removed with a warning

- quantile.colour, quantile.color, quantile.linewidth,
  quantile.linetype:

  Default aesthetics for the quantile lines. Set to `NULL` to inherit
  from the data's aesthetics. By default, quantile lines are hidden and
  can be turned on by changing `quantile.linetype`.

- scale:

  if "area" (default), all violins have the same area (before trimming
  the tails). If "count", areas are scaled proportionally to the number
  of observations. If "width", all violins have the same maximum width.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- orientation:

  The orientation of the layer. The default (`NA`) automatically
  determines the orientation from the aesthetic mapping. In the rare
  event that this fails it can be given explicitly by setting
  `orientation` to either `"x"` or `"y"`. See the *Orientation* section
  for more detail.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- draw_quantiles:

  **\[deprecated\]** Previous specification of drawing quantiles.

## Value

XLayer or YLayer object to be added to a ggplot object

## See also

[geom\_\*sideboxplot](https://jtlandis.github.io/ggside/reference/geom_xsideboxplot.md)

## Examples

``` r
df <- expand.grid(UpperCase = LETTERS, LowerCase = letters)
df$Combo_Index <- as.integer(df$UpperCase) * as.integer(df$LowerCase)

p1 <- ggplot(df, aes(UpperCase, LowerCase)) +
  geom_tile(aes(fill = Combo_Index))

# sideviolins
# Note - Mixing discrete and continuous axis scales
# using xsideviolins when the y aesthetic was previously
# mapped with a continuous varialbe will prevent
# any labels from being plotted. This is a feature that
# will hopefully be added to ggside in the future.

p1 + geom_xsideviolin(aes(y = Combo_Index)) +
  geom_ysideviolin(aes(x = Combo_Index))


# sideviolins with swapped orientation
# Note - Discrete before Continuous
# If you are to mix Discrete and Continuous variables on
# one axis, ggplot2 prefers the discrete variable to be mapped
# BEFORE the continuous.
ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_xsideviolin(aes(y = Species), orientation = "y") +
  geom_point()


# Alternatively, you can recast the value as a factor and then
# a numeric

ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point() +
  geom_xsideviolin(aes(y = as.numeric(Species)), orientation = "y") +
  geom_ysideviolin(aes(x = as.numeric(Species)), orientation = "x")

```

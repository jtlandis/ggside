# ggside options

Set characteristics of side panels

## Usage

``` r
ggside(
  x.pos = NULL,
  y.pos = NULL,
  scales = NULL,
  collapse = NULL,
  draw_x_on = NULL,
  draw_y_on = NULL,
  strip = NULL,
  respect_side_labels = NULL
)
```

## Arguments

- x.pos:

  x side panel can either take "top" or "bottom"

- y.pos:

  y side panel can either take "right" or "left"

- scales:

  Determines side panel's unaligned axis scale. Inputs are similar to
  facet\_\* scales function. Default is set to "fixed", but "free_x",
  "free_y" and "free" are acceptable inputs. For example, xside panels
  are aligned to the x axis of the main panel. Setting "free" or
  "free_y" will cause all y scales of the x side Panels to be
  independent.

- collapse:

  Determines if side panels should be collapsed into a single panel. Set
  "x" to collapse all x side panels, set "y" to collapse all y side
  panels, set "all" to collapse both x and y side panels.

- draw_x_on, draw_y_on:

  Determines where the axis is rendered. For example: By default, the
  bottom x-axis is rendered on the bottom most panel per column. If set
  to "main", then the axis is rendered on the bottom of the bottom most
  main panel. If set to "side", then the x-axis is rendered on the
  bottom of the bottom most side panel(s). You may apply this logic to
  all axis positions.

- strip:

  Determines if the strip should be rendered on the main plot or on
  their default locations. Only has an effect on `facet_grid`.

- respect_side_labels:

  Valid arguments are "default", "x", "y", "all", and "none" Indicates
  if panel spacing should respect the axis labels. The default is to
  respect side panel labels except when xside labels are on the same
  side as the yside panel. Note: setting this parameter to "x" is to
  "respect the labels of the xside panel" and consequently the yside
  labels, if present, are not respected.

## Value

a object of class 'ggside_options' or to be added to a ggplot

## See also

For more information regarding the ggside api: see
[xside](https://jtlandis.github.io/ggside/reference/xside.md) or
[yside](https://jtlandis.github.io/ggside/reference/yside.md)

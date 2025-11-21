# Rescale x or y onto new range in margin

Take the range of the specified axis and rescale it to a new range about
a midpoint. By default the range will be calculated from the associated
main plot axis mapping. The range will either be the resolution or 5% of
the axis range, depending if original data is discrete or continuous
respectively. Each layer called with position_rescale will possess an
instance value that indexes with axis rescale. By default, each
position_rescale will dodge the previous call unless instance is
specified to a previous layer.

## Usage

``` r
position_rescale(
  rescale = "y",
  midpoint = NULL,
  range = NULL,
  location = NULL,
  instance = NULL
)

position_yrescale(
  rescale = "y",
  midpoint = NULL,
  range = NULL,
  location = NULL,
  instance = NULL
)

position_xrescale(
  rescale = "x",
  midpoint = NULL,
  range = NULL,
  location = NULL,
  instance = NULL
)
```

## Format

An object of class `PositionRescale` (inherits from `Position`,
`ggproto`, `gg`) of length 10.

## Arguments

- rescale:

  character value of "x" or "y". specifies which mapping data will be
  rescaled

- midpoint:

  default set to NULL. Center point about which the rescaled x/y values
  will reside.

- range:

  default set to NULL and auto generates from main mapping range.
  Specifies the size of the rescaled range.

- location:

  specifies where position_rescale should try to place midpoint. If
  midpoint is specified, location is ignored and placed at the specified
  location.

- instance:

  integer that indexes rescaled axis calls. instance may be specified
  and if a previous layer with the same instance exists, then the same
  midpoint and range are used for rescaling. x and y are indexed
  independently.

## Value

a ggproto object inheriting from 'Position' and can be added to a ggplot

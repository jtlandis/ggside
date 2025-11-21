# Extending base ggproto classes for ggside

These ggproto classes are slightly modified from their respective
inherited
[ggproto](https://ggplot2.tidyverse.org/reference/ggproto.html) class.
The biggest difference is exposing 'x/yfill', 'x/ycolour', and
'x/ycolor' as viable aesthetic mappings.

## Usage

``` r
parse_side_aes(data, params)
```

## Arguments

- data:

  data passed internally

- params:

  params available to ggproto object

## Value

ggproto object that is usually passed to
[layer](https://ggplot2.tidyverse.org/reference/layer.html)

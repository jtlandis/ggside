# Explicit conversion to ggside object

Function is only exported for possible extensions to ggside. ggplot2
objects are implicitly converted to ggside objects by 'adding' a ggside
object such as a `ggside_layer` object.

## Usage

``` r
as_ggside(x, ...)

# Default S3 method
as_ggside(x, ...)

# S3 method for class 'ggplot'
as_ggside(x, ggside = NULL, ...)

# S3 method for class '`ggside::ggside`'
as_ggside(x, ggside = NULL, ...)

# S3 method for class 'ggside'
as_ggside(x, ggside = NULL, ...)
```

## Arguments

- x:

  an object to convert

- ...:

  unused argument

- ggside:

  new ggside object to add

# Extending base ggproto classes for ggside

`check_scales_collapse` is a helper function that is meant to be called
after the inherited Facet's compute_layout method

`sidePanelLayout` is a helper function that is meant to be called after
the inherited Facet's compute_layout method and after
`check_scales_collapse`

S3 class that converts old Facet into one that is compatible with
ggside. Can also update ggside on the object. Typically, the new ggproto
will inherit from the object being replaced.

## Usage

``` r
check_scales_collapse(data, params)

sidePanelLayout(layout, ggside)

ggside_facet(facet, ggside)
```

## Arguments

- data:

  data passed through ggproto object

- params:

  parameters passed through ggproto object

- layout:

  layout computed by inherited ggproto Facet compute_layout method

- ggside:

  ggside object to update

- facet:

  Facet ggproto Object to replace

## Value

ggproto object that can be added to a ggplot object

## Extended Facets

The following is a list
[ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
facets that are available to use by ggside base.

- [FacetNull](https://ggplot2.tidyverse.org/reference/Facet.html) -\>
  FacetSideNull

- [FacetGrid](https://ggplot2.tidyverse.org/reference/Facet.html) -\>
  FacetSideGrid

- [FacetWrap](https://ggplot2.tidyverse.org/reference/Facet.html) -\>
  FacetSideWrap

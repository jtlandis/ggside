# The xside geometries

`xside` refers to the api of ggside. Any `geom_` with `xside` will plot
its respective geometry along the x-axis per facet panel. By default the
xside panel will plot above the main panel. This xside panel will always
share the same scale as it's main panel, but is expected to have a
separate y-axis scaling.

## Value

geom_xside\* return a XLayer object to be added to a ggplot

## New Aesthetics

All `xside` Geometries have `xfill`, `xcolour`/`xcolor` available for
aesthetic mappings. These mappings behave exactly like the default
counterparts except that they are considered separate scales. All
`xside` geometries will use `xfill` over `fill`, but will default to
`fill` if `xfill` is not provided. The same goes for `xcolour` in
respects to `colour`. This comes in handy if you wish to map both `fill`
to one geometry as continuous, you can still map `xfill` for a separate
`xside` geometry without conflicts. See more information in
`vignette("ggside")`.

## Exported Geometries

The following are the `xside` variants of the
[ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
Geometries

- [geom_xsidebar](https://jtlandis.github.io/ggside/reference/geom_xsidebar.md)

- [geom_xsideboxplot](https://jtlandis.github.io/ggside/reference/geom_xsideboxplot.md)

- [geom_xsidecol](https://jtlandis.github.io/ggside/reference/geom_xsidebar.md)

- [geom_xsidedensity](https://jtlandis.github.io/ggside/reference/geom_xsidedensity.md)

- [geom_xsidefreqpoly](https://jtlandis.github.io/ggside/reference/geom_xsidefreqpoly.md)

- [geom_xsidehistogram](https://jtlandis.github.io/ggside/reference/geom_xsidehistogram.md)

- [geom_xsideline](https://jtlandis.github.io/ggside/reference/geom_xsideline.md)

- [geom_xsidepath](https://jtlandis.github.io/ggside/reference/geom_xsideline.md)

- [geom_xsidepoint](https://jtlandis.github.io/ggside/reference/geom_xsidepoint.md)

- [geom_xsidetext](https://jtlandis.github.io/ggside/reference/geom_xsidetext.md)

- [geom_xsidetile](https://jtlandis.github.io/ggside/reference/geom_xsidetile.md)

- [geom_xsideviolin](https://jtlandis.github.io/ggside/reference/geom_xsideviolin.md)

## See also

[yside](https://jtlandis.github.io/ggside/reference/yside.md)

# The yside geometries

`yside` refers to the api of ggside. Any `geom_` with `yside` will plot
its respective geometry along the y-axis per facet panel. The yside
panel will plot to the right of the main panel by default. This yside
panel will always share the same scale as it's main panel, but is
expected to have a separate x-axis scaling.

## Value

geom_yside\* return a YLayer object to be added to a ggplot

## New Aesthetics

All `yside` Geometries have `yfill`, `ycolour`/`ycolor` available for
aesthetic mappings. These mappings behave exactly like the default
counterparts except that they are considered separate scales. All
`yside` geometries will use `yfill` over `fill`, but will default to
`fill` if `yfill` is not provided. The same goes for `ycolour` in
respects to `colour`. This comes in handy if you wish to map both `fill`
to one geometry as continuous, you can still map `yfill` for a separate
`yside` geometry without conflicts. See more information in
`vignette("ggside")`.

\#' @section Exported Geometries:

The following are the `yside` variants of the
[ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
Geometries

- [geom_ysidebar](https://jtlandis.github.io/ggside/reference/geom_xsidebar.md)

- [geom_ysideboxplot](https://jtlandis.github.io/ggside/reference/geom_xsideboxplot.md)

- [geom_ysidecol](https://jtlandis.github.io/ggside/reference/geom_xsidebar.md)

- [geom_ysidedensity](https://jtlandis.github.io/ggside/reference/geom_xsidedensity.md)

- [geom_ysidefreqpoly](https://jtlandis.github.io/ggside/reference/geom_xsidefreqpoly.md)

- [geom_ysidehistogram](https://jtlandis.github.io/ggside/reference/geom_xsidehistogram.md)

- [geom_ysideline](https://jtlandis.github.io/ggside/reference/geom_xsideline.md)

- [geom_ysidepath](https://jtlandis.github.io/ggside/reference/geom_xsideline.md)

- [geom_ysidepoint](https://jtlandis.github.io/ggside/reference/geom_xsidepoint.md)

- [geom_ysidetext](https://jtlandis.github.io/ggside/reference/geom_xsidetext.md)

- [geom_ysidetile](https://jtlandis.github.io/ggside/reference/geom_xsidetile.md)

- [geom_ysideviolin](https://jtlandis.github.io/ggside/reference/geom_xsideviolin.md)

## See also

[xside](https://jtlandis.github.io/ggside/reference/xside.md)

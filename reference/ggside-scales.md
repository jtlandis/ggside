# Specifying side scales

The [xside](https://jtlandis.github.io/ggside/reference/xside.md) and
[yside](https://jtlandis.github.io/ggside/reference/yside.md) variants
of [geoms](https://ggplot2.tidyverse.org/reference/Geom.html) are
plotted along the x-axis and y-axis respectively of their main panel's
data mapping. The positional scale here is shared between the main panel
and the side panel. The related positional scale type of the side panel,
i.e. the y axis of the xside panel (xsidey) or the x axis of the yside
panel (ysidex), is determened automatically by `ggplot2` default scales.
However, you can override this by using the
[continuous](https://jtlandis.github.io/ggside/reference/ggside-scales-continuous.md)
or
[discrete](https://jtlandis.github.io/ggside/reference/ggside-scales-discrete.md)
variants within `ggside`. This allows the user to select the scale type
or transform most approprate for their side panels.

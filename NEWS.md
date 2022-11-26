
# ggside 0.2.2

### Updates

* fixed issue where `facet_wrap(..., scales = "free/free_x/free_y")` prevented the associated `scale_ysidex_*()`/`scale_xsidey_*()` functions from plotting the guides. Addresses issue #35
* `ggside` now provides an informative warning if the user provides free scales to facets but an incompatible argument to `ggside(collapse = ...)`. This warning will force the collapse parameter to something that will comply with the facet scales specification.
* `ggside` should be more resistant to `ggplot2`'s updates to their default aesthetics. `ggplot2 v3.4.0` has included a new default aesthetic that has caused `ggside` geoms to break. Addresses issue #36
* `ggside` now allows for the `trans` argument of the `scale_(x|y)side(y|x)_continuous(...)` functions to be used. Addresses issue #40. This feature is experimental and may be removed depending on how `ggplot2` develops in the future. Additional helper functions have been added:
  * `scale_xsidey_log10()`, `scale_ysidex_log10()`
  * `scale_xsidey_reverse()`, `scale_ysidex_reverse()`
  * `scale_xsidey_sqrt()`, `scale_ysidex_reverse()`


# ggside 0.2.1

### Breaking change

Certain `ggside` geometries have the capabilities to use `xfill/yfill` or `xcolour/ycolour` in place of the normal `fill` and `colour` aesthetics. This was always meant to provide a separate aesthetic scale to color by. The earlier version of `ggside` failed to consider the case when `fill`/`colour` was specified in the global mapping, and the user passes a new data frame to the `ggside` layer that excludes the quoted column. This has been addressed with #28 where if `xfill/yfill` are specified, then the global `fill` aesthetic is ignored (as well for `xcolour/ycolour` and `colour`). This fix, however, has exposed a potential breaking change, meaning plots with the following characteristics may not be exactly the same as compared to earlier versions of `ggside` (< 0.2.1) :

* `fill` or `colour` is used in global mapping
* `xfill/yfill` or `xcolour/ycolour` (respectively) is used in a `ggside` layer
* both quoted columns for these aesthetics were available in whatever data given to the layer

How this would have worked in the past is that the computed mapping groups would be made on both aesthetics, but only filling or coloring by the `ggside` aesthetic. Now, the layer will only fill or color by the `ggside` aesthetic - potentially reducing the number of groups made by the plot.

### Updates

* `ggside()` gains a `strip` argument, allowing for `facet_grid` to plot strips on the main panels as opposed to the default of the side panels. Addresses issue #26
* fixed annoying warnings from plot scales when mixing discrete and continuous scales. Addresses issue #33
* added the following `geom_(x|y)side*` variants
  * `geom_xsidefunction()`, `geom_ysidefunction()`
  * `geom_xsideline()`, `geom_ysideline()` 
  * `geom_xsidelabel()`, `geom_ysidelabel()`
* `stat_xsidefunction()` and `stat_ysidefunction()` were added to accompany `geom_(x|y)sidefunction`. This seemed to be a special case in which a stat variant was also needed.
* `geom_abline()`, `geom_hline()`, and `geom_vline()` will no longer cause errors when used on a `ggside` object that has `facet_wrap()/facet_grid()` in place. Addresses issue #3


# ggside 0.2.0

* A set of new theme elements have been added to give users more control over how side panels are rendered. Please see `?ggside-theme` for more details. Addresses Feature request #10.
* To make way for this change, `ggside` now also modifies the `Coord` object. Currently only `CoordCartesian`, `CoordFixed` and `CoordTrans` are supported for `ggside`.
* new arguments added to `ggside`: `draw_x_on` and `draw_y_on` which will allow the user to specify if the respective axis should be rendered on the main panel or the side panel.

# ggside 0.1.3

* Properly imported `ggplot2::ggplot_add()` into `ggside` such that `ggside` methods were available to `ggplot2` calls. Addresses issue #24.
* Re-factored `XLayer` and `YLayer` to rely less on `ggplot2` internal `Layer` class. Hopefully fixing issue users may experience between `ggplot2 (3.3.2)` and `ggplot2 (3.3.5)`. Addresses issue #23.

# ggside 0.1.2

* Fixed bug where additional messages were printed to the console when using `ggplot2::facet_wrap()` or `ggplot2::facet_grid()`. Addresses issue #20.
* Fixed bug where `geom_xsidetext()` was using the incorrect `Geom` parameter. Addresses issue #19.

# ggside 0.1.1

* fixed bug in which `colour/color`/`fill` appear in guides when used as `params` instead of an `aes()` mapping. Now these aesthetics, and their `x/y` variants should behave like `ggplot2` API. Addresses issue #12.
* `ggside` should now work better with the `patchwork` package. Addresses issue #13.
* Fixed 'bug' in which computed aesthetics did not mix well with main panels of class `ContinuousScaleDate`. Extended better control to `XLayer` and `YLayer` to pull scales from the proper panels instead of using the prototype scales on `plot$scales`. Addresses issue #11.

# ggside 0.1.0

* Various `geom_*side*` have better default calling behavior. See `vignettes("ggside_aes_mapping")` for more details.
* Added various functions to help customize side panels. `xsidey` will affect the y-axis of the xside panel, and `ysidex` will affect the x-axis of the yside panel.
  * `scale_xsidey_continuous`
  * `scale_xsidey_discrete`
  * `scale_ysidex_continuous`
  * `scale_ysidex_discrete`
* Using proper semantic versioning
* Added documentation examples for the following side geometries:
  * `geom_*sidebar`
  * `geom_*sideboxplot`
  * `geom_*sidecol`
  * `geom_*sidedensity`
  * `geom_*sidefreqpoly`
  * `geom_*sideline`
  * `geom_*sidepoint`
  * `geom_*sidesegement`
  * `geom_*sidetile`
  * `geom_*sideviolin`
* Corrected bug where `geom_*sideviolin` was not returning `ggside_layer` class
* Updated `vignette(ggside_basic_usage)` 'How its done' section headers

# ggside 0.0.1.3

* Initial CRAN release version

# ggside 0.3.2.9999

## Details/Breaking Changes

As `ggplot2` prepares for its next major release, `ggside` will try to move in kind.
Below is a running log of changes that are required due to `ggplot2`'s welcome changes to S7.


* Double dispatch on ``S7::method(`+`, list(<ggside>, class_any))`` is now available through `ggplot2`, meaning **we no longer need to overload `+.gg` for our own purposes.**
* `ggplot2` still maintains some backward compatibility for R verions < 4.3, in that it will use their internal `add_gg()` function. This was always an internal function and I had to reimplement (and overload) as a hack to get `ggside` to behave correctly. Despite this, `ggside` will remove its own `add_gg()` function and solely rely on the S7 method dispatch for `+`. As a result, **`ggside > v0.3.1` will require R version >= 4.3.0** because we will no longer overload `+.gg` to maintain compatibility with older versions of R.
  * This decision is so that `ggside`'s code complexity can be reduced and lower the burden of maintaining the package.
* Additionally `ggside` (> v0.3.1) will now depend on `ggplot2 (> v3.5.2.xxx)`.
* Deprecating various `is.ggside*()` functions in favor of `is_ggside*()`.

# ggside 0.3.2

* This version will sync with `ggplot2` (3.5.2). It fixing warnings when using `is.ggproto()`. `ggside` Now uses `is_ggproto()` instead. Addresses issue #64
* `ggside` will now depend on R (>= 4.1.0) as it already was doing so internally but not explicitly stated in the DESCRIPTION file.
* Fixing Notes from cran regarding missing doc links.

# ggside 0.3.1

* Hot fix for missed bug caused in `extraChIPs` package. Addresses #58
  * Variables scoped by a function (and not passed to an aesthetic mapping) are now found and evaluated correctly
  * Resurgence of an old warning has been suppressed correctly again #33

# ggside 0.3.0

### Breaking Changes

* The following classes are no longer exported.
 * `FacetSideNull`, `FacetSideGrid`, `FacetSideWrap`
* `ggplot2`'s GeomCol and GeomBar now compute resolution based on panels, and not the entire data as a whole. This may affect `ggside` plots that use these variant geometries. Set the `width` parameter manually to retain old figures.
* `ggside` now depends on `ggplot2 (>3.5.0)` as to make side positional scales work reliably, the `layer` slot of the `ggplot` object needs to be extended and modified. This slot is only available in the latest version of `ggplot2`. Attempting to render a `ggside` object whose Layout has not been subclassed by `ggside` may lead to unexpected results.
* with `ggplot2 (3.5.0)`, there is slightly more spacing between the y-axis title and the panels when the y-axis is plotted on the right side. This may be attributed to the new guide's system.
* `ggside`'s adjacent side scales now operate independently (in support for issue #40, see details in updates section). For example, applying `scale_y_log10()` on a `ggside` plot with a `xside` geometry will NOT apply the transform to the `xsidey` scale. This is an experimental feature and is subject to change in the future if `ggside` can move to a more consistent set of expectations for the side scales.

### Updates

* Using `coord_cartesian(xlim = <limits>, ylim = <limits>)` will only apply to the main plotting scales and not side panel scales. For instance, using `p + coord_cartesian(xlim = c(0,1))` where `p` is a ggside object with a yside geometry will NOT set the limits on the ysidex scale. If the user wants to control the limits on the side scales, they should use `scale_(x|y)side(y|x)_*()` functions. Addresses issue #55
* The `ggside_options` object now inherits from `ggproto` instead of a list.
* The `ggside_options` defaults arguments are now `NULL`. This will help update the object appropriately without overriding previous options specified through `ggside()`.
* `ggside` now subclasses `Layout` allowing for better control of how side scales are trained. This feature may be relatively unstable depending on the version of `ggplot2` installed.
* `ggside_options` object now has a new argument `respect_side_labels` which will control spacing given between panels for their axes text labels. See `?ggside` for argument options. Note, if theme option `ggside.panel.spacing` is smaller than space required for labels, then priority is given to the `respect_side_labels` option until `ggside.panel.spacing` is greater.
* `ggside` now allows for the `trans` argument of the `scale_(x|y)side(y|x)_continuous(...)` functions to be used. Addresses issue #40. This feature is experimental and may be removed depending on how `ggplot2` develops in the future. Additional helper functions have been added:
  * `scale_xsidey_log10()`, `scale_ysidex_log10()`
  * `scale_xsidey_reverse()`, `scale_ysidex_reverse()`
  * `scale_xsidey_sqrt()`, `scale_ysidex_reverse()`
* all `ggside` Geom ggproto objects have their aesthetics changed. For example, `GeomXsidebar$required_aes` is set to `c("x", "xsidey")` instead of `c("x", "y")`. This is to ensure side panel's respective axis generates its own default scale. With this change, users no longer need to preemptively provide a `scale_(x|y)side(y|x)_*()` function.
* The following functions have been deprecated
  * `as_ggsideFacet` -> `ggside_facet`
  * `as_ggsideCoord` -> `ggside_coord`
* Much of the syntax for creating a compatible `ggside` layer has been wrapped into `ggside_layer`, which is similar to `ggplot2::layer` except with an additional argument `side`. This will allow users to attempt to make a compatible `ggside_layer` object with custom Geom and Stat combinations. Ideally, the user should not have to modify the Geom or Stat directly, as this is handled by subclassing the Layer ggproto class directly.
* many of the `ggside` internals have been trimmed down for redundancy, resulting in a smaller package overall.

# ggside 0.2.3

### Updates

* fixes issue where `facet_grid(..., scales = "free/free_x/free_y")` was not allowed with certain `ggside(collaps = "all/x/y")`. This restriction makes sense with `facet_wrap` but not `facet_grid`. Addresses issue #45
* fixes tests so `ggside` is compatible with `scales (v1.3.0)`.
* adding `cli` as a dependency
* added `xside` and `yside` variants for `geom_abline`, `geom_hline` and `geom_vline`. Addresses issue #48

# ggside 0.2.2

### Updates

* The `ggside_options` object now inherits from `ggproto` instead of a list.
* fixed issue where `facet_wrap(..., scales = "free/free_x/free_y")` prevented the associated `scale_ysidex_*()`/`scale_xsidey_*()` functions from plotting the guides. Addresses issue #35
* `ggside` now provides an informative warning if the user provides free scales to facets but an incompatible argument to `ggside(collapse = ...)`. This warning will force the collapse parameter to something that will comply with the facet scales specification.
* `ggside` should be more resistant to `ggplot2`'s updates to their default aesthetics. `ggplot2 v3.4.0` has included a new default aesthetic that has caused `ggside` geoms to break. Addresses issue #36


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

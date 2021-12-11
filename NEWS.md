
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

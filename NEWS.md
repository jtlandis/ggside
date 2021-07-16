
# ggside 0.1.0.9000

* fixed bug in which `colour/color`/`fill` appear in guides when used as `params` instead of an `aes()` mapping. Now these aesthetics, and their `x/y` variants should behave like `ggplot2` API. Addresses issue #12.
* `ggside` should now work better with the `patchwork` package. Addreseses issue #13.
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

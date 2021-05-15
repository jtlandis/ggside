
# ggside 0.0.2.9001

* Various `geom_*side*` have better default calling behavior. See `vignettes("ggside_aes_mapping")` for more details.
* Added various functions to help customize side panels. `xsidey` will affect the y-axis of the xside panel, and `ysidex` will affect the x-axis of the yside panel.
  * `scale_xsidey_continuous`
  * `scale_xsidey_discrete`
  * `scale_ysidex_continuous`
  * `scale_ysidex_discrete`

# ggside 0.0.2

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

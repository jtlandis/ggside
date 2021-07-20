This is a minor patch to the `ggside` package. Fixed outstanding bug where `geom_xsidetext()` did not use the proper `geom` parameter, causing an error. Fixed bug introduced in v0.1.1 where many messages were printed to the console when using `ggside` with `facet_wrap()` or `facet_grid()`.

## Test environments
* local macOS Big Sur install, R 4.1.0
* Ubuntu 16.04.6 (on travis-ci), R 4.0.2
* win-builder (devel, release)


## R CMD checks results

0 errors | 0 warnings | 0 note

## Downstream dependencies

There are no reverse dependencies for package `ggside` yet.

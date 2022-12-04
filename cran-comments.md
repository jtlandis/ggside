This is a minor content update to the `ggside` package to keep up with `ggplot 3.4.0` recent update. The main intention behind this release is to better protect `ggside` from `ggplot2`'s API changes. There were some minor bugs patched as well as a few extra functions exported to the NAMESPACE.

## Test environments
* local macOS Big Sur install, R 4.2.0
* Github Actions (on travis-ci; devel, release)
  * MacOS-latest (release)
  * Windows-latest (release)
  * ubuntu-latest (devel)
  * ubuntu-latest (release)
  * ubuntu-latest (oldrel-1)
* win-builder (devel, release)


## R CMD checks results

0 errors | 0 warnings | 0 note

## Downstream dependencies

We checked 6 reverse dependencies (3 from CRAN + 3 from Bioconductor) with `revdepcheck::revdep_check()`, comparing R CMD check results across CRAN and dev versions of `ggside`.

 * We saw 0 new problems
 * We failed to check 0 packages

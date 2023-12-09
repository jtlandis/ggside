This is a minor content update to the `ggside` package to address issues with CRAN checks, specifically playing catch up with `scales (v1.3.0)` recent update. There were some minor bugs patched as well.

## Test environments
* local macOS Big Sur install, R 4.3.0
* Github Actions (on travis-ci; devel, release)
  * MacOS-latest (release)
  * Windows-latest (release)
  * ubuntu-latest (devel)
    * `ggside` failed to pass several tests due to .svg comparisons from `vdiffr::expect_doppelganger()` failing. Upon reviewing artifacts exported from github actions, all changes were minimal relating to strokes of lines, unnoticeable to the human eye.
  * ubuntu-latest (release)
  * ubuntu-latest (oldrel-1)
* win-builder (devel, release)


## R CMD checks results

0 errors | 0 warnings | 0 note

## Downstream dependencies

We checked 6 reverse dependencies (2 from CRAN + 4 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

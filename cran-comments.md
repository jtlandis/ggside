This is a patch to `ggside` in that we depend on ggplot2 4.0.0 and greater.
The ggplot2 4.0.0 release was anticipated to have breaking changes. This patch
fixes `ggside` to be compatible with the latest release. We now adopt the
S7 framework and now use proper double dispatch for adding custom layers.

## Test environments

-   local macOS 15.5, R 4.5.0
-   Github Actions (on travis-ci; devel, release)
    -   MacOS-latest (release)
    -   Windows-latest (release)
    -   ubuntu-latest (release)
    -   ubuntu-latest (oldrel-1)
-   win-builder (devel, release)

## R CMD checks results

0 errors | 0 warnings | 0 note

## Downstream dependencies
### revdepcheck results

We checked 7 reverse dependencies (3 from CRAN + 4 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

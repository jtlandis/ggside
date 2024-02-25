This is a major patch update that fixes lingering issues with `ggside` that were made possible thanks to the latest `ggplot2` update. Most changes were focused on subclassing the Layout ggproto object. `ggside` had a major rewrite in how it constructed its layer objects, once again in an effort to depend more closely on `ggplot2` internals.

## Test environments

-   local macOS Big Sur install, R 4.3.2
-   Github Actions (on travis-ci; devel, release)
    -   MacOS-latest (release)
    -   Windows-latest (release)
    -   ubuntu-latest (release)
    -   ubuntu-latest (oldrel-1)
-   win-builder (devel, release)

## R CMD checks results

0 errors | 0 warnings | 0 note

## Downstream dependencies

We checked 6 reverse dependencies (2 from CRAN + 4 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

-   We saw 0 new problems
-   We failed to check 0 packages

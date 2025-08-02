This is a minor update to `ggside` in that we depend on ggplot2 3.5.2 and greater as they introduced a deprecation warning with `ggplot2::is.ggproto()`.
Additionally, I added some missing documentation links specified by [cran checks](https://cran.r-project.org/web/checks/check_results_ggside.html).
Finally, `ggside` now explicitly depends on R 4.1.0 or greater, since ggplot2 3.5.2 requires R 4.1.0 or greater.

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

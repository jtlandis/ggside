This is a minor patch to `ggside`. A small error has been found in the plotting
of some side geometries and facet combinations. It has been fixed with this 
update and additional tests have been added to check for this regression in 
the future.

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

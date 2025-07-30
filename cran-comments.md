This is a minor hotfix for the `ggside` package. Previous `revdepcheck` had failed to catch a bug (introduced and found in reverse dependency `extraChIPs` on bioconductor) due to the `R CMD check` timing out. A `R CMD check` was ran locally to verify the hotfix removed the bug and resulted in a successful check in `extraChIPs`. I apologize for the inconvenience of a resubmission.

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
### revdepcheck results

We checked 6 reverse dependencies (2 from CRAN + 4 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

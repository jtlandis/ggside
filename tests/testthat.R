library(testthat)
library(ggplot2)
library(ggside)

if ((nzchar(Sys.getenv("CI")) ||
     !nzchar(Sys.getenv("NOT_CRAN"))) &&
    identical(Sys.getenv("VDIFFR_RUN_TESTS"), 'false')) {
  #if we are running tests remotely AND
  # we are opting out of using vdiffr
  # assigning a dummy function

  expect_doppelganger <- function(...) {
    testthat::skip("`VDIFFR_RUN_TESTS` set to false on this remote check")
  }
} else {
  expect_doppelganger <- vdiffr::expect_doppelganger
}

test_check("ggside")

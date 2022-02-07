testthat::test_that("`check_rational` works as expected", {

  xwo <- 25
  xwith <- structure(xwo, rational = TRUE)

  fpattern <- "^Setting `rational` to FALSE$"
  tpattern <- "^Setting `rational` to TRUE$"

  testthat::expect_error(
    check_rational(NULL, xwith),
    "^is.logical\\(rational) is not TRUE$"
  )

  testthat::expect_false(
    testthat::expect_warning(
      check_rational(FALSE, xwith),
      "^Conflict detected.*Setting to FALSE based on internal testing.$"
    )
  )

  testthat::expect_true(check_rational(TRUE, xwith))

  testthat::expect_false(
    testthat::expect_warning(check_rational(NULL, xwo), fpattern)
  )

  testthat::expect_false(check_rational(NULL, 25L))
  testthat::expect_false(
    testthat::expect_warning(check_rational(NULL, 25), fpattern)
  )

  testthat::expect_true(
    testthat::expect_warning(check_rational(NULL, 25.5), tpattern)
  )

  testthat::expect_false(
    testthat::expect_warning(check_rational(NULL, 25), fpattern)
  )

  testthat::expect_false(
    testthat::expect_warning(check_rational(NULL, Sys.time()), fpattern)
  )

  testthat::expect_error(
    check_rational(NA, Sys.time()),
    "^isTRUE\\(is.logical\\(rational.*is.na\\(rational.*not TRUE$"
  )

})

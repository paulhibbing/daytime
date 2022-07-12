testthat::test_that("`structure_daytime` works as expected", {

  testthat::expect_warning(structure_daytime(25.5, 5), "Setting `rational` to TRUE")
  suppressWarnings(testthat::expect_true(attr(structure_daytime(25.5, 5), "rational")))

  testthat::expect_warning(structure_daytime(25, 5), "Setting `rational` to FALSE")
  suppressWarnings(testthat::expect_false(attr(structure_daytime(25, 5), "rational")))

  testthat::expect_error(
    testthat::expect_warning(
      structure_daytime(Sys.time(), as.character(Sys.time())),
      "Setting `rational` to FALSE"
    ),
    "^Detected 1 non-missing element\\(s) of `x`.*\\[0, 1439]\\.$"
  )

  testthat::expect_error(
    testthat::expect_warning(
      structure_daytime(
        structure(Sys.time(), rational = TRUE),
        as.character(Sys.time()),
        FALSE
      ),
      "^Conflict detected.*Setting to FALSE based on internal testing.$"
    ),
    "^Detected 1 non-missing element\\(s) of `x`.*\\[0, 1439]\\.$"
  )

  testthat::expect_warning(
    structure_daytime(
      structure(25.5, rational = TRUE),
      as.character(Sys.time()),
      FALSE
    ),
    "^Conflict detected.*Setting to TRUE based on internal testing.$"
  )

})

testthat::test_that("range testing works as expected", {

  #* Range examine

    testthat::expect_equal(
      range_examine(c(-1:0, 1440:1441)),
      c(FALSE, TRUE, FALSE, FALSE)
    )

    testthat::expect_equal(
      range_examine(
        c(-1:0, 1440:1441),
        inc_lower = FALSE, inc_upper = TRUE
      ),
      c(FALSE, FALSE, TRUE, FALSE)
    )

    testthat::expect_warning(
      range_examine(
        c(-1:0, 1440:1441), 1440, 0
      ),
      "lower is greater than upper; they will be swapped"
    )

    suppressWarnings(testthat::expect_equal(
      range_examine(
        c(-1:0, 1440:1441), 1440, 0
      ),
      c(FALSE, TRUE, FALSE, FALSE)
    ))

    testthat::expect_equal(
      range_examine(
        c(-1, NA, NA, 0, NA, 1440, NA, NA, 1441, NA)
      ),
      c(FALSE, NA, NA, TRUE, NA, FALSE, NA, NA, FALSE, NA)
    )

  #* Range test on non-daytime input

    testthat::expect_error(
      range_test(
        c(-1:0, 1440:1441), 0, 1440,
        rational_adjust = FALSE
      ),
      paste0(
        "Detected 3 non-missing element\\(s) of `x` that fall",
        " outside the expected range of \\[0, 1440)"
      )
    )

    testthat::expect_error(
      range_test(
        c(-1:0, 1440:1441), 0, 1440,
        inc_lower = FALSE, inc_upper = TRUE,
        rational_adjust = FALSE
      ),
      paste0(
        "Detected 3 non-missing element\\(s) of `x` that fall",
        " outside the expected range of \\(0, 1440]"
      )
    )

    testthat::expect_warning(
      range_test(
        c(0, 720, 1439), 1440, 0,
        rational_adjust = FALSE
      ),
      "lower is greater than upper; they will be swapped"
    )

    testthat::expect_warning(
      range_test(c(0, 720, 1439), inc_upper = TRUE),
      "Skipping rational_adjust \\(no `rational` attribute)"
    )

    suppressWarnings(testthat::expect_true(
      range_test(c(0, 720, 1439), inc_upper = TRUE)
    ))

  #* Tests with rational==TRUE

    test_val <- structure(319.5, rational = TRUE)

    testthat::expect_true(range_test(test_val, 300, 319))
    testthat::expect_error(
      range_test(test_val, 300, 319, rational_adjust = FALSE),
      "range of \\[300, 319)"
    )

  #* Tests with rational==FALSE

    test_val <- structure(319.5, rational = FALSE)

    testthat::expect_true(range_test(floor(test_val), 300, 319))
    testthat::expect_error(
      range_test(test_val, 300, 319),
      "range of \\[300, 319]"
    )
    testthat::expect_error(
      range_test(floor(test_val), 300, 319, rational_adjust = FALSE),
      "range of \\[300, 319)"
    )

  #* Other settings

    testthat::expect_error(
      range_test(1450, 2000, 2500, TRUE, FALSE, FALSE),
      "^Detected 1 non-missing element\\(s) of `x`.*of \\[2000, 2500).$"
    )

})

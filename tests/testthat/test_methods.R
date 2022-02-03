testthat::test_that("daytime methods work as expected", {

  x <- as_daytime(
    c("23:59:00", "23:59:59", "00:00:00", "00:00:59"),
    TRUE,
    format = "%H:%M:%S"
  )

  m <- mean(x)
  s <- sd(x)

  testthat::expect_s3_class(m, "daytime")
  testthat::expect_equal(
    m, 1439.992, tolerance = 0.001, ignore_attr = TRUE
  )

  testthat::expect_equal(tod(m), "23:59:59", ignore_attr = TRUE)
  testthat::expect_equal(s, 0.701239, ignore_attr = TRUE, tolerance = 0.0001)

})

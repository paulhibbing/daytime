testthat::test_that("daytime arithmetic works as expected", {

  x <- expand.grid(
    earlier = seq(0L, 1439L, 300L),
    later = seq(0L, 1439L, 300L)
  )

  earlier <- as_daytime(x$earlier)
  later <- as_daytime(x$later)

  testthat::expect_equal(
    daytime_modulo(earlier, later),
    c(0, 1140, 840, 540, 240, 300, 0, 1140, 840, 540, 600, 300, 0,
      1140, 840, 900, 600, 300, 0, 1140, 1200, 900, 600, 300, 0)
  )

})

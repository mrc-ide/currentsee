test_that("simulate produces tidy transition data", {
  set.seed(1)
  df <- simulate(3)

  expect_s3_class(df, "tbl_df")
  expect_setequal(names(df), c("id", "-1", "0", "1", "2", "3", "current"))
  expect_equal(nrow(df), 3)

  expect_error(simulate(0), "positive integer")
  expect_error(simulate(1, current = c("cm", "zzz")), "present")
  expect_error(simulate(1, weights = c(foo = 0.5)), "correspond")
})


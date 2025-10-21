test_that("simulate produces tidy transition data", {
  set.seed(1)
  df <- simulate(3)

  expect_s3_class(df, "tbl_df")
  expect_setequal(names(df), c("step", "package", "changed", "id", "current"))
  expect_equal(length(unique(df$id)), 3)

  steps_by_id <- split(df$step, df$id)
  expect_true(all(vapply(steps_by_id, function(x) all(diff(x) >= 0), logical(1))))

  expect_error(simulate(0), "positive integer")
  expect_error(simulate(1, current = c("cm", "zzz")), "present")
  expect_error(simulate(1, weights = c(foo = 0.5)), "correspond")
})


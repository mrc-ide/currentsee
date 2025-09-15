test_that("check_step_data validates columns", {
  df <- data.frame(step = 1, package = "A", n = 1, prop = 1, grp = "a")
  expect_silent(check_step_data(df, group_cols = "grp"))
  expect_error(check_step_data(df, group_cols = "missing"), "Missing columns")
})

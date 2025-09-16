test_that("launch_step_app validates inputs", {
  df <- simulate(2)
  expect_error(launch_step_app(df[, c("step", "package")]), "must contain")
  expect_error(launch_step_app(df, group_cols = "missing"), "not present")
})

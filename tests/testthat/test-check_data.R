test_that("check_ce_data validates columns", {
  df <- data.frame(cost = 1, effect = 1, grp = "a")
  expect_silent(check_ce_data(df, "cost", "effect", "grp"))
  expect_error(check_ce_data(df, "cost", "effect", "missing"), "Missing columns")
})

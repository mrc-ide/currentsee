test_that("summarise_step_packages computes proportions", {
  df <- data.frame(
    step = c(0, 0, 0, -1, -1),
    package = c("A", "A", "B", "A", "B"),
    grp = "G",
    stringsAsFactors = FALSE
  )
  res <- summarise_step_packages(df, group_cols = "grp")
  expect_equal(res$prop[res$step == 0 & res$package == "A"], 2 / 3)
  expect_equal(res$prop[res$step == 0 & res$package == "B"], 1 / 3)
  sums <- res |> dplyr::group_by(grp, step) |> dplyr::summarise(t = sum(prop), .groups = "drop")
  expect_true(all(abs(sums$t - 1) < 1e-8))
})

test_that("simulate_step_data generates expected structure", {
  set.seed(1)
  df <- simulate_step_data(n = 2)
  expect_true(all(c("pfpr", "seasonality", "id", "step", "package") %in% names(df)))
  expect_s3_class(df, "data.frame")
  expect_true(is.integer(df$step))
  chk <- df |> dplyr::group_by(pfpr, seasonality, id) |> dplyr::summarise(has0 = any(step == 0), .groups = "drop")
  expect_true(all(chk$has0))
})

test_that("build_step_sankey_inputs returns nodes and links", {
  set.seed(1)
  df <- simulate_step_data(n = 2, pfpr = "low", seasonality = "low")
  inputs <- build_step_sankey_inputs(df, group_cols = c("pfpr", "seasonality"), id_col = "id")
  expect_named(inputs, c("nodes", "links"))
  expect_true(all(c("step", "label", "x") %in% names(inputs$nodes)))
  expect_true(all(c("source", "target", "value") %in% names(inputs$links)))
  expect_true(any(grepl("\n", inputs$nodes$label)))
})

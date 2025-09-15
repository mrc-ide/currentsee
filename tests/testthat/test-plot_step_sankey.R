test_that("plot_step_sankey returns networkD3 widget", {
  set.seed(1)
  df <- simulate_step_data(n = 2, pfpr = "low", seasonality = "low")
  p <- plot_step_sankey(df, group_cols = c("pfpr", "seasonality"), id_col = "id")
  expect_s3_class(p, "htmlwidget")
})

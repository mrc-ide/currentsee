test_that("plot_step_sankey returns networkD3 widget", {
  set.seed(1)
  df <- simulate_step_data(n = 2, pfpr = "low", seasonality = "low")
  p <- plot_step_sankey(df, group_cols = c("pfpr", "seasonality"), id_col = "id", engine = "networkD3")
  expect_s3_class(p, "htmlwidget")
})

test_that("plot_step_sankey returns ggplot", {
  skip_if_not_installed("ggsankey")
  set.seed(1)
  df <- simulate_step_data(n = 2, pfpr = "low", seasonality = "low")
  p <- plot_step_sankey(df, group_cols = c("pfpr", "seasonality"), id_col = "id", engine = "ggsankey")
  expect_s3_class(p, "ggplot")
})

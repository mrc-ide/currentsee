test_that("plot_step_sankey returns networkD3 widget", {
  df <- data.frame(grp = "G", step = c(-1, -1), package = c("A", "B"), n = c(1, 2), prop = c(1/3, 2/3))
  p <- plot_step_sankey(df, group_cols = "grp", engine = "networkD3")
  expect_s3_class(p, "htmlwidget")
})

test_that("plot_step_sankey returns ggplot", {
  skip_if_not_installed("ggsankey")
  df <- data.frame(grp = "G", step = c(-1, -1), package = c("A", "B"), n = c(1, 2), prop = c(1/3, 2/3))
  p <- plot_step_sankey(df, group_cols = "grp", engine = "ggsankey")
  expect_s3_class(p, "ggplot")
})

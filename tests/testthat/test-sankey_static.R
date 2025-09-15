test_that("plot_ce_sankey returns networkD3 widget", {
  df <- data.frame(cost = c(1, 2), effect = c(1, 2), g1 = c("A", "B"), g2 = c("X", "Y"))
  p <- plot_ce_sankey(df, "cost", "effect", c("g1", "g2"), engine = "networkD3")
  expect_s3_class(p, "htmlwidget")
})

test_that("plot_ce_sankey returns ggplot", {
  skip_if_not_installed("ggsankey")
  df <- data.frame(cost = c(1, 2), effect = c(1, 2), g1 = c("A", "B"), g2 = c("X", "Y"))
  p <- plot_ce_sankey(df, "cost", "effect", c("g1", "g2"), engine = "ggsankey")
  expect_s3_class(p, "ggplot")
})

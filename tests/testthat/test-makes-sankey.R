test_that("make_sankey returns an ggplot", {
  df <- simulate(3)
  up <- nodes_up(df)
  sankey <- make_sankey(up$up, up$up_nodes)

  expect_s3_class(sankey, "ggplot")
})


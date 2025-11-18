test_that("make_sankey returns an ggplot", {
  df <- simulate(3)
  sankey <- make_sankey(df, "up")
  expect_s3_class(sankey, "ggplot")

  sankey <- make_sankey(df, "down")
  expect_s3_class(sankey, "ggplot")
})


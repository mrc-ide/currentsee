test_that("make_sankey returns an ggplot", {
  df <- simulate(3)
  sankey <- make_sankey(df[,c(paste(0:3))])
  expect_s3_class(sankey, "ggplot")

  sankey <- make_sankey(df[,c(paste(-1:0))])
  expect_s3_class(sankey, "ggplot")
})


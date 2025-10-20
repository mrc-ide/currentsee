test_that("make_sankey returns an htmlwidget", {
  df <- simulate(3)
  nodes <- make_nodes(df)
  links <- make_links(df, nodes)
  widget <- make_sankey(nodes, links)

  expect_s3_class(widget, "htmlwidget")
  expect_equal(widget$x$links$tooltip, dplyr::filter(links, !is.na(next_package))$tooltip)
})

test_that("make_sankey validates alpha range", {
  df <- simulate(1)
  nodes <- make_nodes(df)
  links <- make_links(df, nodes)

  expect_error(make_sankey(nodes, links, alpha_range = c(0.1, 0.5, 0.9)))
})

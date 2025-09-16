test_that("makes_sankey returns an htmlwidget", {
  df <- simulate(3)
  nodes <- make_nodes(df)
  links <- make_links(df, nodes)
  colours <- make_colours(nodes$id)

  widget <- makes_sankey(nodes, links, colours)

  expect_s3_class(widget, "htmlwidget")
  expect_equal(widget$x$links$tooltip, dplyr::filter(links, !is.na(next_package))$tooltip)
})

test_that("makes_sankey validates alpha range", {
  df <- simulate(1)
  nodes <- make_nodes(df)
  links <- make_links(df, nodes)
  colours <- make_colours(nodes$id)

  expect_error(makes_sankey(nodes, links, colours, alpha_range = c(0.1, 0.5, 0.9)))
})

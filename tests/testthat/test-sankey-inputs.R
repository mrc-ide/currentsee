test_that("make_nodes creates labelled nodes", {
  df <- simulate(4)
  nodes <- nodes_up(df)

  expect_true(all(c("node", "label") %in% names(nodes$up_nodes)))
  expect_equal(nrow(nodes$up), 4)
  expect_true(all(!duplicated(nodes$name)))

  nodes <- nodes_down(df)

  expect_true(all(c("node", "label") %in% names(nodes$down_nodes)))
  expect_equal(nrow(nodes$down), 4)
  expect_true(all(!duplicated(nodes$name)))
})


test_that("make_nodes creates labelled nodes", {
  df <- simulate(4)
  nodes <- make_nodes(df)

  expect_true(all(c("name", "node_name", "package_id") %in% names(nodes)))
  expect_equal(nrow(nodes), length(unique(df$package)) + 1)
  expect_true(all(!duplicated(nodes$name)))
})

test_that("make_links summarises transitions", {
  df <- simulate(4)
  nodes <- make_nodes(df)
  links <- make_links(df, nodes)

  expect_true(all(c(
    "package", "next_package", "step", "value", "source", "target",
    "label", "id", "change", "tooltip"
  ) %in% names(links)))

  expect_true(all(links$value >= 1))
  expect_match(links$tooltip[1], "add")

  expect_error(make_links(df[, c("step", "package")]), "must contain")
})


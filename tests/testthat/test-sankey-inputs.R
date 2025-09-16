test_that("make_nodes creates labelled nodes", {
  df <- simulate(4)
  nodes <- make_nodes(df)

  expect_true(all(c("name", "node_name", "id") %in% names(nodes)))
  expect_equal(nrow(nodes), length(unique(df$package)))
  expect_identical(nodes$id, as.character(seq_len(nrow(nodes)) - 1L))
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
  expect_true(all(na.omit(links$source) %in% as.integer(nodes$id)))
  expect_true(all(na.omit(links$target) %in% as.integer(nodes$id)))
  expect_match(links$tooltip[1], "\\u2192: add")

  expect_error(make_links(df[, c("step", "package")]), "must contain")
})

test_that("make_colours returns a d3 scale", {
  nodes <- make_nodes(simulate(2))
  scale <- make_colours(nodes$id)

  expect_type(scale, "character")
  expect_match(scale, "scaleOrdinal")
  expect_true(all(vapply(nodes$id, grepl, logical(1), x = scale, fixed = TRUE)))
})

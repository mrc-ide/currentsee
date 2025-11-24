# Test file: test-sankey-functions.R

# Simple test data -------------------------------------------------------------
test_data_simple <- data.frame(
  `-1` = c("A", "B", "A"),
  `0` = c("A", "A", "B"),
  `1` = c("B", "B", "B"),
  check.names = FALSE
)

test_data_with_na <- data.frame(
  `0` = c("A", NA, "B"),
  `1` = c("A", "B", NA),
  check.names = FALSE
)

test_nodes_simple <- data.frame(
  node = c("A", "B"),
  ymin = c(-5, 0),
  ymax = c(5, 10),
  xmin = c(0.95, 1.95),
  xmax = c(1.05, 2.05),
  ycenter = c(0, 5)
)

# Tests for make_nodes() -------------------------------------------------------

test_that("make_nodes returns a data frame with correct columns", {
  result <- make_nodes(test_data_simple)

  expect_s3_class(result, "data.frame")
  expect_true("node" %in% names(result))
  expect_true("Freq" %in% names(result))
  expect_true("xmin" %in% names(result))
  expect_true("xmax" %in% names(result))
  expect_true("xcenter" %in% names(result))
})

test_that("make_nodes calculates node width correctly", {
  result <- make_nodes(test_data_simple, node_width = 0.1)

  # Check that width is applied correctly
  first_node <- result[1, ]
  expect_equal(first_node$xmax - first_node$xmin, 0.2)  # 2 * node_width
})

test_that("make_nodes handles empty columns", {
  empty_col_data <- data.frame(
    `0` = c("A", "B"),
    `1` = c(NA, NA),  # Empty column
    `2` = c("A", "A"),
    check.names = FALSE
  )

  result <- make_nodes(empty_col_data)

  # Should only have nodes from columns 1 and 3 (not column 2)
  expect_true(all(result$xcenter %in% c(1, 3)))
})

test_that("make_nodes counts frequencies correctly", {
  freq_data <- data.frame(
    `0` = c("A", "A", "A", "B"),
    check.names = FALSE
  )

  result <- make_nodes(freq_data)

  expect_equal(result$Freq[result$node == "A"], 3)
  expect_equal(result$Freq[result$node == "B"], 1)
})

# Tests for make_flows() -------------------------------------------------------

test_that("make_flows returns a data frame with correct columns", {
  result <- make_flows(test_data_simple, test_nodes_simple)

  expect_s3_class(result, "data.frame")
  expect_true("flow_start" %in% names(result))
  expect_true("flow_end" %in% names(result))
  expect_true("direction" %in% names(result))
  expect_true("n" %in% names(result))
})

test_that("make_flows identifies up and down directions", {
  result <- make_flows(test_data_simple, test_nodes_simple)

  expect_true("up" %in% result$direction)
  expect_true("down" %in% result$direction)
})

test_that("make_flows sets hjust correctly for directions", {
  result <- make_flows(test_data_simple, test_nodes_simple)

  # Up flows should be left-aligned (hjust = 0)
  up_flows <- result[result$direction == "up", ]
  expect_true(all(up_flows$hjust == 0))

  # Down flows should be right-aligned (hjust = 1)
  down_flows <- result[result$direction == "down", ]
  expect_true(all(down_flows$hjust == 1))
})

test_that("make_flows handles only positive columns", {
  up_only_data <- data.frame(
    `0` = c("A", "B"),
    `1` = c("A", "A"),
    check.names = FALSE
  )

  result <- make_flows(up_only_data, test_nodes_simple)

  expect_true(all(result$direction == "up"))
})

test_that("make_flows handles only negative columns", {
  down_only_data <- data.frame(
    `-1` = c("A", "B"),
    `0` = c("A", "A"),
    check.names = FALSE
  )

  result <- make_flows(down_only_data, test_nodes_simple)

  expect_true(all(result$direction == "down"))
})

test_that("make_flows excludes NA values", {
  result <- make_flows(test_data_with_na, test_nodes_simple)

  expect_false(any(is.na(result$flow_start)))
  expect_false(any(is.na(result$flow_end)))
})

test_that("make_flows calculates correct flow counts", {
  count_data <- data.frame(
    `0` = c("A", "A", "B"),
    `1` = c("A", "B", "B"),
    check.names = FALSE
  )

  result <- make_flows(count_data, test_nodes_simple)

  # Should have A->A (1), A->B (1), B->B (1)
  a_to_a <- result[result$flow_start == "A" & result$flow_end == "A", ]
  expect_equal(a_to_a$n, 1)

  a_to_b <- result[result$flow_start == "A" & result$flow_end == "B", ]
  expect_equal(a_to_b$n, 1)

  b_to_b <- result[result$flow_start == "B" & result$flow_end == "B", ]
  expect_equal(b_to_b$n, 1)
})

test_that("make_flows positions flows within node boundaries", {
  result <- make_flows(test_data_simple, test_nodes_simple)

  for(i in 1:nrow(result)) {
    start_node <- test_nodes_simple[test_nodes_simple$node == result$flow_start[i], ]
    end_node <- test_nodes_simple[test_nodes_simple$node == result$flow_end[i], ]

    expect_gte(result$flow_start_ymin[i], start_node$ymin)
    expect_lte(result$flow_start_ymax[i], start_node$ymax)
    expect_gte(result$flow_end_ymin[i], end_node$ymin)
    expect_lte(result$flow_end_ymax[i], end_node$ymax)
  }
})

# Simple edge cases ------------------------------------------------------------

test_that("Error on single column", {
  single_col <- data.frame(`0` = c("A", "B"), check.names = FALSE)
  expect_error(make_sankey(single_col))
})



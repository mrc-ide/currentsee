# Test data setup
setup_test_data <- function() {
  data.frame(
    current = c("A", "A", "B", "B"),
    region = c("North", "South", "North", "South"),
    intervention = c("ITN", "IRS", "ITN", "SMC"),
    `-2` = c("baseline", "baseline", "ITN", "ITN"),
    `-1` = c("baseline", "ITN", "ITN", "ITN+IRS"),
    `0` = c("ITN", "ITN", "ITN+IRS", "ITN+IRS+SMC"),
    `1` = c("ITN+IRS", "ITN+IRS", "ITN+IRS+SMC", NA),
    `2` = c("ITN+IRS+SMC", NA, NA, NA),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# Mock input object for testing
mock_input <- function(selections = list()) {
  structure(selections, class = "reactivevalues")
}

# Tests for get_filter_vars() ------------------------------------------------

test_that("get_filter_vars returns correct filter variables", {
  test_data <- setup_test_data()

  result <- get_filter_vars(test_data)
  expected <- c("current", "region", "intervention")

  expect_equal(result, expected)
  expect_type(result, "character")
})

test_that("get_filter_vars handles data with only core columns", {
  core_only_data <- data.frame(
    current = c("A", "B"),
    `-1` = c("x", "y"),
    `0` = c("y", "z"),
    `1` = c("z", "w"),
    check.names = FALSE
  )

  result <- get_filter_vars(core_only_data)
  expect_equal(result, "current")
})

test_that("get_filter_vars handles empty data frame", {
  empty_data <- data.frame()
  result <- get_filter_vars(empty_data)
  expect_equal(result, "current")
})

# Tests for apply_filters() --------------------------------------------------

test_that("apply_filters works with single filter", {
  test_data <- setup_test_data()
  filter_vars <- c("region")
  input <- list(region = "North")

  result <- apply_filters(test_data, filter_vars, input)

  expect_equal(nrow(result), 2)
  expect_true(all(result$region == "North"))
})

test_that("apply_filters works with multiple filters", {
  test_data <- setup_test_data()
  filter_vars <- c("region", "intervention")
  input <- list(region = "North", intervention = "ITN")

  result <- apply_filters(test_data, filter_vars, input)

  expect_equal(nrow(result), 2)
  expect_equal(result$region, c("North", "North"))
  expect_equal(result$intervention, c("ITN", "ITN"))
})

test_that("apply_filters ignores 'All' selections", {
  test_data <- setup_test_data()
  filter_vars <- c("region", "intervention")
  input <- list(region = "All", intervention = "ITN")

  result <- apply_filters(test_data, filter_vars, input)

  expect_equal(nrow(result), 2)  # Only filtered by intervention
  expect_true(all(result$intervention == "ITN"))
})

test_that("apply_filters ignores NULL selections", {
  test_data <- setup_test_data()
  filter_vars <- c("region", "intervention")
  input <- list(region = NULL, intervention = "ITN")

  result <- apply_filters(test_data, filter_vars, input)

  expect_equal(nrow(result), 2)  # Only filtered by intervention
  expect_true(all(result$intervention == "ITN"))
})

test_that("apply_filters returns original data when no filters applied", {
  test_data <- setup_test_data()
  filter_vars <- c("region", "intervention")
  input <- list(region = "All", intervention = "All")

  result <- apply_filters(test_data, filter_vars, input)

  expect_equal(result, test_data)
})

test_that("apply_filters returns empty data frame when no matches", {
  test_data <- setup_test_data()
  filter_vars <- c("region")
  input <- list(region = "NonExistent")

  result <- apply_filters(test_data, filter_vars, input)

  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), ncol(test_data))
})

# Tests for validate_current_selection() -------------------------------------

test_that("validate_current_selection passes with valid single current", {
  test_data <- data.frame(current = c("A", "A", "A"))

  # Should not throw an error
  expect_silent(validate_current_selection(test_data))
})

test_that("validate_current_selection fails with multiple current values", {
  test_data <- data.frame(current = c("A", "B", "A"))

  expect_error(
    validate_current_selection(test_data),
    "Choose a value for 'current'"
  )
})

test_that("validate_current_selection fails with 'All' current value", {
  test_data <- data.frame(current = c("All", "All"))

  expect_error(
    validate_current_selection(test_data),
    "Choose a value for 'current'"
  )
})

test_that("validate_current_selection fails with empty data", {
  test_data <- data.frame(current = character(0))

  expect_error(
    validate_current_selection(test_data),
    "No matching pathways"
  )
})

test_that("validate_current_selection handles NA values correctly", {
  test_data <- data.frame(current = c("A", "A", NA))

  expect_silent(validate_current_selection(test_data))
})

# Tests for calculate_plot_width() -------------------------------------------

test_that("calculate_plot_width calculates correctly with default width", {
  test_data <- data.frame(
    `-1` = c("a", "b"),
    `0` = c("c", "d"),
    `1` = c("e", "f"),
    `2` = c(NA, NA),  # This column should be excluded
    check.names = FALSE
  )

  result <- calculate_plot_width(test_data, -1:2, 220)
  expected <- 3 * 220  # 3 non-empty columns

  expect_equal(result, expected)
})

test_that("calculate_plot_width works with custom width per column", {
  test_data <- data.frame(
    `-1` = c("a", "b"),
    `0` = c("c", "d"),
    check.names = FALSE
  )

  result <- calculate_plot_width(test_data, -1:0, 100)
  expected <- 2 * 100

  expect_equal(result, expected)
})

test_that("calculate_plot_width handles no matching columns", {
  test_data <- data.frame(
    other_col = c("a", "b")
  )

  result <- calculate_plot_width(test_data, -1:1, 220)
  expect_equal(result, 0)
})

test_that("calculate_plot_width excludes all-NA columns", {
  test_data <- data.frame(
    `-1` = c("a", "b"),
    `0` = c(NA, NA),
    `1` = c("c", "d"),
    check.names = FALSE
  )

  result <- calculate_plot_width(test_data, -1:1, 220)
  expected <- 2 * 220  # Only 2 non-empty columns

  expect_equal(result, expected)
})

# Tests for create_coverage_box() --------------------------------------------

test_that("create_coverage_box calculates percentage correctly", {
  result <- create_coverage_box(100, 75)

  # Check that the result contains the percentage
  expect_true(grepl("75%", as.character(result)))
})

test_that("create_coverage_box handles zero original rows", {
  result <- create_coverage_box(0, 0)

  expect_true(grepl("0%", as.character(result)))
})

test_that("create_coverage_box shows warning for low coverage", {
  result <- create_coverage_box(100, 5)  # 5% coverage

  result_str <- as.character(result)
  expect_true(grepl("5%", result_str))
  expect_true(grepl("very few records matched", result_str))
})

test_that("create_coverage_box doesn't show warning for good coverage", {
  result <- create_coverage_box(100, 50)  # 50% coverage

  result_str <- as.character(result)
  expect_true(grepl("50%", result_str))
  expect_false(grepl("very few records matched", result_str))
})

test_that("create_coverage_box uses correct colors for different percentages", {
  # High coverage (80%+) - should be green
  high_result <- create_coverage_box(100, 85)
  expect_true(grepl("#28a745", as.character(high_result)))

  # Low coverage (<15%) - should be red
  low_result <- create_coverage_box(100, 10)
  expect_true(grepl("#dc3545", as.character(low_result)))
})

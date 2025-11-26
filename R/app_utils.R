#' Get Filter Variables
#'
#' Identifies which columns in the data should be used as filter variables
#' by excluding core Sankey construction columns.
#'
#' @param data A data frame containing the pathway data
#' @return A character vector of column names to be used as filter variables
get_filter_vars <- function(data) {
  core_cols <- c("-4", "-3", "-2", "-1", "0", "1", "2", "3", "4")
  c("current", setdiff(names(data), c("current", core_cols)))
}

#' Apply Filter Selections
#'
#' Subsets the data based on user filter selections from Shiny inputs.
#' Filters are only applied when selection is not NULL or "All".
#'
#' @param data A data frame to be filtered
#' @param filter_vars Character vector of column names to filter on
#' @param input Shiny input object containing filter selections
#' @return A filtered data frame
apply_filters <- function(data, filter_vars, input) {
  filtered_data <- data

  for (var in filter_vars) {
    selection <- input[[var]]
    if (!is.null(selection) && selection != "All") {
      filtered_data <- filtered_data[filtered_data[[var]] == selection, , drop = FALSE]
    }
  }

  filtered_data
}

#' Validate Current Selection
#'
#' Validates that the 'current' column has exactly one non-NA value
#' that is not "All", which is required for Sankey plot generation.
#'
#' @param data A data frame containing a 'current' column
#' @return NULL (function validates or throws error via shiny::validate)
validate_current_selection <- function(data) {
  current_vals <- unique(as.character(data$current))
  current_vals <- current_vals[!is.na(current_vals)]

  validate(
    need(
      length(current_vals) == 1 && current_vals != "All",
      "Choose a value for 'current' in the left panel to show the Sankey."
    ),
    need(
      nrow(data) > 0,
      "No matching pathways for this combination of filters."
    )
  )
}

#' Create Sankey Plot
#'
#' Generates a Sankey diagram with standardized parameters after
#' validating the input data.
#'
#' @param data A data frame containing pathway data
#' @param step_columns Character vector of column names representing steps
#' @return A Sankey plot object from currentsee::make_sankey()
create_sankey_plot <- function(data, step_columns) {
  validate_current_selection(data)

  validate(
    need(
      nrow(data) > 0,
      "No matching pathways for this combination of filters."
    )
  )

  currentsee::make_sankey(
    data[, step_columns],
    node_width = 0.2,
    flow_label_font_size = 4,
    node_label_font_size = 5
  )
}

#' Calculate Plot Width
#'
#' Calculates dynamic plot width based on the number of non-empty columns
#' within a specified step range.
#'
#' @param data A data frame containing the plot data
#' @param step_range Numeric vector of step values to include (e.g., -20:20)
#' @param width_per_col Numeric, width in pixels per column (default: 220)
#' @return Numeric value representing total plot width in pixels
calculate_plot_width <- function(data, step_range, width_per_col = 220) {
  n_cols <- sum(names(data[, !sapply(data, function(x) all(is.na(x)))]) %in% paste(step_range))
  n_cols * width_per_col
}

#' Create Coverage Value Box
#'
#' Generates a styled HTML div showing data coverage percentage with
#' color-coded background and optional warning for low coverage.
#'
#' @param original_rows Numeric, total number of rows in original dataset
#' @param filtered_rows Numeric, number of rows after filtering
#' @return HTML div element with coverage information and styling
create_coverage_box <- function(original_rows, filtered_rows) {
  percentage <- if (original_rows == 0) 0 else round((filtered_rows / original_rows) * 100, 1)

  # Dynamic color based on coverage percentage
  bg_color <- case_when(
    percentage >= 80 ~ "#28a745",
    percentage >= 60 ~ "#6cb04a",
    percentage >= 40 ~ "#9bc53d",
    percentage >= 25 ~ "#ffc107",
    percentage >= 15 ~ "#fd7e14",
    TRUE ~ "#dc3545"
  )

  # Warning for very low coverage
  subtitle_text <- if (percentage < 10) {
    div(
      style = "font-size: 0.9em; margin-top: 8px; font-weight: bold;",
      "\u26a0 very few records matched"
    )
  } else {
    NULL
  }

  div(
    style = paste0(
      "background: linear-gradient(135deg, ", bg_color, " 0%, ",
      adjustcolor(bg_color, alpha.f = 0.8), " 100%); ",
      "border-radius: 12px; padding: 20px; text-align: center; ",
      "color: white; box-shadow: 0 4px 15px rgba(0,0,0,0.2); ",
      "margin-bottom: 20px; transition: all 0.3s ease;"
    ),
    div(
      style = "display: flex; align-items: center; justify-content: center; margin-bottom: 10px;",
      bsicons::bs_icon("funnel", size = "1.2em", style = "margin-right: 15px;"),
      span("Records Matched", style = "font-size: 1.2em; font-weight: 500; margin-left: 8px;")
    ),
    div(
      style = "font-size: 2.5em; font-weight: bold; margin-bottom: 5px;",
      paste0(percentage, "%")
    ),
    subtitle_text
  )
}

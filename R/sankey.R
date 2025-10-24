#' @importFrom rlang .data
NULL

#' Create nodes for a step-package Sankey diagram
#'
#' @param x A data frame containing at least a `package` column where each
#'   entry describes the package combination at a given step.
#'
#' @return A data frame with columns `name`, `node_name`, and `id` suitable for
#'   use with [networkD3::sankeyNetwork()].
#'
#' @examples
#' df <- simulate(3)
#' make_nodes(df)
#'
#' @export
make_nodes <- function(x) {
  stopifnot("package" %in% names(x))

  # Get unique packages and create base nodes
  unique_packages <- sort(unique(x$package))

  denominator <- max(table(x$package))

  # Calculate proportions more efficiently
  package_props <- x |>
    dplyr::count(.data$step, .data$package, name = "n") |>
    dplyr::mutate(
      tooltip = paste0(round(100 * .data$n / denominator, 1), "%")
    ) |>
    dplyr::select("package", "tooltip") |>
    dplyr::distinct()

  # Build final nodes data frame
  nodes <- tibble::tibble(
    name = unique_packages,
    node_name = gsub(", ", "\n", unique_packages, fixed = TRUE)
  ) |>
    dplyr::left_join(package_props, by = c("name" = "package")) |>
    dplyr::left_join(currentsee::package_id, by = c("name" = "package")) |>
    dplyr::bind_rows(tibble::tibble(name = "ghost", tooltip = "ghost")) |>
    as.data.frame()

  nodes
}


#' Calculate package differences between two package sets
#'
#' Computes the difference between two sets of comma-separated package strings,
#' identifying which packages were added or removed when transitioning from one
#' package combination to another. Useful for tracking package changes in
#' step-wise analyses or Sankey diagram workflows.
#'
#' @param from_pkg Character vector. Source package combinations as comma-separated
#'   strings (e.g., "pkg1, pkg2, pkg3").
#' @param to_pkg Character vector. Target package combinations as comma-separated
#'   strings. Must be the same length as `from_pkg`.
#' @param direction Character string. Direction of comparison, either `"add"`
#'   (default) to find packages in `to_pkg` but not in `from_pkg`, or `"remove"`
#'   to find packages in `from_pkg` but not in `to_pkg`.
#'
#' @return Character vector of the same length as input vectors. Each element
#'   contains the comma-separated list of package differences, or `NA_character_`
#'   if `to_pkg` contains `NA` values.
calc_package_diff <- function(from_pkg, to_pkg, direction = "add") {
  purrr::map2_chr(
    strsplit(from_pkg, ",\\s*"),
    strsplit(to_pkg, ",\\s*"),
    ~ {
      if (any(is.na(.y))) return(NA_character_)

      diff_packages <- if (direction == "add") {
        setdiff(.y,.x)
      } else {
        setdiff(.x,.y)
      }

      paste(diff_packages, collapse = ", ")
    }
  )
}

#' Build link definitions between Sankey nodes
#'
#' @param x A data frame containing at least `id`, `step`, and `package`
#'   columns describing package transitions for each simulation run.
#' @param nodes Optional nodes data frame as returned by [make_nodes()]. If
#'   omitted the nodes are derived from `x`.
#' @param down Logical indicator if moving up or down frontier
#'
#' @return A tibble describing links between nodes including `source`,
#'   `target`, and tooltip metadata.
#'
#' @export
make_links <- function(x, nodes = make_nodes(x), down = FALSE) {
  # Validation
  required_cols <- c("id", "step", "package")
  missing <- setdiff(required_cols, names(x))
  if (length(missing) > 0) {
    stop("`x` must contain columns: ", paste(missing, collapse = ", "))
  }

  if (!"name" %in% names(nodes)) {
    stop("`nodes` must contain a `name` column.")
  }

  # Prepare data
  node_lookup <- nodes$name

  processed_data <- x |>
    dplyr::mutate(step = if (down) abs(.data$step) else .data$step) |>
    dplyr::arrange(.data$id, .data$step) |>
    dplyr::mutate(
      next_package = dplyr::lead(.data$package, 1, default = "ghost"),
      .by = "id"
    ) |>
    dplyr::count(.data$package, .data$next_package, .data$step, name = "value") |>
    dplyr::mutate(
      source = match(.data$package, node_lookup) - 1L,
      target = match(.data$next_package, node_lookup) - 1L,
      label = gsub(", ", "\n", .data$package, fixed = TRUE),
      id = as.character(source)
    )

  # Add direction-specific tooltips
  result <- processed_data |>
    dplyr::mutate(
      change = if (down) {
        # For decreasing spend: what was removed (in package but not in next_package)
        calc_package_diff(.data$package, .data$next_package, "remove")
      } else {
        # For increasing spend: what was added (in next_package but not in package)
        calc_package_diff(.data$package, .data$next_package, "add")
      },
      tooltip = dplyr::case_when(
        next_package == "ghost" ~ "NA",
        down ~ paste0("\u2192: remove ", change),
        TRUE ~ paste0("\u2192: add ", change)
      ),
      p = paste0(round(100 * .data$value / sum(.data$value), 1), "%"), .by = "step"
    ) |>
    dplyr::left_join(currentsee::package_id, by = "package") |>
    as.data.frame()

  result
}

#' Render a step-package Sankey diagram
#'
#' @param nodes Data frame describing Sankey nodes as produced by
#'   [make_nodes()].
#' @param links Data frame describing connections between nodes, typically the
#'   output of [make_links()].
#' @param ... Addtional arguments to pass to [networkD3::sankeyNetwork()]
#'
#' @return An htmlwidgets object produced by
#'   [networkD3::sankeyNetwork()].
#' @export
make_sankey <- function(
    nodes,
    links,
    ...
) {

  networkD3::sankeyNetwork(
    Links  = links,
    Nodes  = nodes,
    Source = "source",
    Target = "target",
    Value  = "value",
    NodeID = "node_name",
    NodeGroup = "package_id",
    LinkGroup = "package_id",
    colourScale = make_colour_scale(),
    ...
  ) |>
    add_linebreaks_in_labels() |>
    center_node_labels() |>
    add_link_tooltips(links$tooltip) |>
    add_node_tooltips(nodes$tooltip) |>
    remove_ghost()
}

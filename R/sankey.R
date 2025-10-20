#' @importFrom rlang .data
NULL

check_x <- function(x){
  stopifnot("id" %in% names(x))
  stopifnot("step" %in% names(x))
  stopifnot("package" %in% names(x))
  grouping <- setdiff(names(x), c("id", "step", "package"))
  print(paste0("Grouping cols: ", paste(grouping, collapse = ", ")))
}

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

  nodes <- data.frame(
    name = sort(unique(x$package)),
    stringsAsFactors = FALSE
  )

  nodes$node_name <- vapply(
    nodes$name,
    function(value) gsub(", ", "\n", value, fixed = TRUE),
    character(1)
  )

  denominator = max(table(x$package))

  prop <-  x |>
    dplyr::summarise(
      n = dplyr::n(),
      .by = c("step", "package")
    ) |>
    dplyr::mutate(
      tooltip = paste0(round(100 * .data$n / denominator, 1), "%"),
      .by = c("step")
    ) |>
    dplyr::rename("name" = "package") |>
    dplyr::select("name", "tooltip")

  nodes <- nodes |>
    dplyr::left_join(prop, by = "name") |>
    dplyr::left_join(package_id, by = c("name" = "package"))

  nodes <- dplyr::bind_rows(nodes,
                 data.frame(name = "ghost", tooltip = "ghost"))

  nodes
}

#' Build link definitions between Sankey nodes
#'
#' @param x A data frame containing at least `id`, `step`, and `package`
#'   columns describing package transitions for each simulation run.
#' @param nodes Optional nodes data frame as returned by [make_nodes()]. If
#'   omitted the nodes are derived from `x`.
#'
#' @return A tibble describing links between nodes including `source`,
#'   `target`, and tooltip metadata.
#'
#' @export
make_links <- function(x, nodes = make_nodes(x), down = FALSE) {
  required_cols <- c("id", "step", "package")
  missing <- setdiff(required_cols, names(x))
  if (length(missing) > 0) {
    stop("`x` must contain columns: ", paste(missing, collapse = ", "))
  }

  if (!"name" %in% names(nodes)) {
    stop("`nodes` must contain a `name` column.")
  }

  node_lookup <- nodes$name

  if(down){
    x$step = abs(x$step)
  }

  x <- x |>
    dplyr::arrange(id, step) |>
    dplyr::mutate(
      next_package = dplyr::lead(.data$package),
      next_step = dplyr::lead(.data$step),
      .by = "id"
    ) |>
    dplyr::mutate(
      next_package = ifelse(is.na(next_package), "ghost", next_package)
    ) |>
    dplyr::summarise(
      value = dplyr::n(),
      .by = c("package", "next_package", "step")
    ) |>
    dplyr::mutate(
      source = match(.data$package, node_lookup) - 1L,
      target = match(.data$next_package, node_lookup) - 1L,
      label = gsub(", ", "\n", .data$package, fixed = TRUE),
      id = as.character(.data$source)
    )

  if(down){
    x <- x |>
      dplyr::mutate(
      change = purrr::map2_chr(
        strsplit(.data$next_package, ",\\s*"),
        strsplit(.data$package, ",\\s*"),
        ~ {
          if (any(is.na(.y))) {
            return(NA_character_)
          }
          new <- setdiff(.y, .x)
          paste(new, collapse = ", ")
        }
      )
    ) |>
      dplyr::mutate(
        tooltip = paste0("\u2192: remove ", .data$change),
        tooltip = ifelse(next_package == "ghost", "NA", tooltip)
      )
  } else {
    x <- x |>
      dplyr::mutate(
        change = purrr::map2_chr(
          strsplit(.data$package, ",\\s*"),
          strsplit(.data$next_package, ",\\s*"),
          ~ {
            if (any(is.na(.y))) {
              return(NA_character_)
            }
            new <- setdiff(.y, .x)
            paste(new, collapse = ", ")
          }
        )
      ) |>
      dplyr::mutate(
        tooltip = paste0("\u2192: add ", .data$change),
        tooltip = ifelse(next_package == "ghost", "NA", tooltip)
      )
  }

  x |>
    dplyr::mutate(
      p = paste0(round(100 * .data$value / sum(.data$value), 1), "%"),
      .by = "step"
    )  |>
    dplyr::left_join(package_id, by = c("package")) |>
    as.data.frame()
}

#' Render a step-package Sankey diagram
#'
#' @param nodes Data frame describing Sankey nodes as produced by
#'   [make_nodes()].
#' @param links Data frame describing connections between nodes, typically the
#'   output of [make_links()].
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

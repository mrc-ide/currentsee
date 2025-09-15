#' Build nodes and links for step Sankey diagram
#'
#' Prepare the `nodes` and `links` data frames required by
#' [networkD3::sankeyNetwork()] from step-package data.
#'
#' @param df Data frame containing step package data.
#' @param step_col Name of the step column.
#' @param package_col Name of the package column.
#' @param group_cols Character vector of grouping columns excluding `id_col`.
#' @param id_col Name of the identifier column defining unique sequences.
#'
#' @return A list with two data frames: `nodes` and `links`. The `nodes` data
#'   frame includes a `step` column indicating the x-position and a `label`
#'   column with interventions separated by new lines.
#' @export
build_step_sankey_inputs <- function(df,
                                     step_col = "step",
                                     package_col = "package",
                                     group_cols = character(),
                                     id_col = "id") {
  required <- c(step_col, package_col, group_cols, id_col)
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  step_vals <- sort(unique(df[[step_col]]))
  if (length(step_vals) < 2) {
    stop("Need at least two distinct steps to build a Sankey diagram.", call. = FALSE)
  }

  seq_df <- dplyr::select(df, dplyr::all_of(c(group_cols, id_col, step_col, package_col)))
  wide_df <- tidyr::pivot_wider(
    seq_df,
    names_from = !!rlang::sym(step_col),
    values_from = !!rlang::sym(package_col)
  )

  path_df <- dplyr::count(
    wide_df,
    dplyr::across(dplyr::all_of(c(group_cols, as.character(step_vals)))),
    name = "value"
  )

  links_list <- lapply(seq_len(length(step_vals) - 1), function(i) {
    col_src <- as.character(step_vals[i])
    col_tgt <- as.character(step_vals[i + 1])
    dplyr::group_by(path_df,
                    source_pkg = .data[[col_src]],
                    target_pkg = .data[[col_tgt]]) %>%
      dplyr::summarise(value = sum(.data$value), .groups = "drop") %>%
      dplyr::mutate(
        source_step = step_vals[i],
        target_step = step_vals[i + 1]
      ) %>%
      dplyr::select(source_step, source_pkg, target_step, target_pkg, value)
  })

  links <- dplyr::bind_rows(links_list)

  nodes <- dplyr::bind_rows(
    dplyr::select(links, step = source_step, package = source_pkg),
    dplyr::select(links, step = target_step, package = target_pkg)
  ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      label = gsub(" & ", "\n", package),
      x = match(step, step_vals) - 1
    )

  links <- links %>%
    dplyr::mutate(
      source = match(paste(source_step, source_pkg),
                     paste(nodes$step, nodes$package)) - 1,
      target = match(paste(target_step, target_pkg),
                     paste(nodes$step, nodes$package)) - 1
    ) %>%
    dplyr::select(source, target, value)

  nodes <- dplyr::select(nodes, step, label, x)

  list(nodes = nodes, links = links)
}


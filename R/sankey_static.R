#' Plot a step-package Sankey diagram
#'
#' Build a Sankey diagram from summarised step package data using either
#' `networkD3` or `ggsankey`.
#'
#' @param df Data frame containing step package summaries.
#' @param step_col Name of the step column.
#' @param package_col Name of the package column.
#' @param n_col Name of the count column.
#' @param group_cols Ordered character vector of additional grouping columns.
#' @param engine Rendering engine: either "networkD3" or "ggsankey".
#'
#' @return An `htmlwidget` (networkD3) or `ggplot` (ggsankey) object.
#' @export
plot_step_sankey <- function(df,
                             step_col = "step",
                             package_col = "package",
                             n_col = "n",
                             group_cols = character(),
                             engine = c("networkD3", "ggsankey")) {
  engine <- match.arg(engine)
  check_step_data(df, step_col, package_col, n_col, "prop", group_cols)

  all_cols <- c(group_cols, step_col, package_col)
  if (length(all_cols) < 2) {
    stop("`group_cols` must define a sequence including step and package.", call. = FALSE)
  }

  if (engine == "networkD3") {
    links_list <- lapply(seq_len(length(all_cols) - 1), function(i) {
      dplyr::group_by(df, .data[[all_cols[i]]], .data[[all_cols[i + 1]]]) %>%
        dplyr::summarise(value = sum(.data[[n_col]]), .groups = "drop") %>%
        dplyr::mutate(
          source = paste(all_cols[i], .data[[all_cols[i]]], sep = ": "),
          target = paste(all_cols[i + 1], .data[[all_cols[i + 1]]], sep = ": ")
        ) %>%
        dplyr::select(source, target, value)
    })

    links <- dplyr::bind_rows(links_list)
    nodes <- data.frame(name = unique(c(links$source, links$target)), stringsAsFactors = FALSE)
    links$source <- match(links$source, nodes$name) - 1
    links$target <- match(links$target, nodes$name) - 1

    networkD3::sankeyNetwork(
      Links = links,
      Nodes = nodes,
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name"
    )
  } else {
    long_df <- ggsankey::make_long(df, !!!rlang::syms(all_cols), value = !!rlang::sym(n_col))
    ggplot2::ggplot(
      long_df,
      ggplot2::aes(
        x = x,
        next_x = next_x,
        node = node,
        next_node = next_node,
        value = value,
        fill = node
      )
    ) +
      ggsankey::geom_sankey() +
      ggsankey::geom_sankey_label(ggplot2::aes(label = node), size = 3) +
      ggplot2::theme_minimal()
  }
}

#' Plot a cost-effectiveness Sankey diagram
#'
#' Build a Sankey diagram from cost-effectiveness outputs using either
#' `networkD3` or `ggsankey`.
#'
#' @param df Data frame containing cost-effectiveness outputs.
#' @param cost_col Name of the cost column.
#' @param effect_col Name of the effect column (unused, kept for future use).
#' @param group_cols Ordered character vector of grouping column names.
#' @param engine Rendering engine: either "networkD3" or "ggsankey".
#'
#' @return An `htmlwidget` (networkD3) or `ggplot` (ggsankey) object.
#' @export
plot_ce_sankey <- function(df,
                           cost_col,
                           effect_col,
                           group_cols,
                           engine = c("networkD3", "ggsankey")) {
  engine <- match.arg(engine)
  check_ce_data(df, cost_col, effect_col, group_cols)

  if (engine == "networkD3") {
    links_list <- lapply(seq_len(length(group_cols) - 1), function(i) {
      dplyr::group_by(df, .data[[group_cols[i]]], .data[[group_cols[i + 1]]]) %>%
        dplyr::summarise(value = sum(.data[[cost_col]]), .groups = "drop") %>%
        dplyr::mutate(
          source = paste(group_cols[i], .data[[group_cols[i]]], sep = ": "),
          target = paste(group_cols[i + 1], .data[[group_cols[i + 1]]], sep = ": ")
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
    long_df <- ggsankey::make_long(df, !!!rlang::syms(group_cols), value = !!rlang::sym(cost_col))
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

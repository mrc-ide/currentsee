#' Plot a step-package Sankey diagram
#'
#' Build a Sankey diagram showing how intervention packages change across
#' sequential steps. Each step forms a stage in the diagram and packages are
#' displayed as nodes within those stages.
#'
#' @param df Data frame containing step package data.
#' @param step_col Name of the step column.
#' @param package_col Name of the package column.
#' @param group_cols Character vector of grouping columns excluding `id_col`.
#' @param id_col Name of the identifier column defining unique sequences.
#' @param engine Rendering engine: either "networkD3" or "ggsankey".
#'
#' @return An `htmlwidget` (networkD3) or `ggplot` (ggsankey) object.
#' @export
plot_step_sankey <- function(df,
                             step_col = "step",
                             package_col = "package",
                             group_cols = character(),
                             id_col = "id",
                             engine = c("networkD3", "ggsankey")) {
  engine <- match.arg(engine)

  required <- c(step_col, package_col, group_cols, id_col)
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  step_vals <- sort(unique(df[[step_col]]))
  if (length(step_vals) < 2) {
    stop("Need at least two distinct steps to build a Sankey diagram.", call. = FALSE)
  }

  step_syms <- rlang::syms(as.character(step_vals))

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

  if (engine == "networkD3") {
    links_list <- lapply(seq_len(length(step_vals) - 1), function(i) {
      dplyr::group_by(path_df, .data[[as.character(step_vals[i])]], .data[[as.character(step_vals[i + 1])]]) %>%
        dplyr::summarise(value = sum(.data$value), .groups = "drop") %>%
        dplyr::mutate(
          source = paste(step_vals[i], .data[[as.character(step_vals[i])]], sep = "|"),
          target = paste(step_vals[i + 1], .data[[as.character(step_vals[i + 1])]], sep = "|")
        ) %>%
        dplyr::select(source, target, value)
    })

    links <- dplyr::bind_rows(links_list)
    nodes <- data.frame(name = unique(c(links$source, links$target)), stringsAsFactors = FALSE)
    nodes$label <- sub("^[^|]+\|", "", nodes$name)
    nodes$group <- sub("\|.*$", "", nodes$name)
    links$source <- match(links$source, nodes$name) - 1
    links$target <- match(links$target, nodes$name) - 1

    networkD3::sankeyNetwork(
      Links = links,
      Nodes = nodes,
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "label",
      NodeGroup = "group"
    )
  } else {
    long_df <- ggsankey::make_long(path_df, !!!step_syms, value = value)
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

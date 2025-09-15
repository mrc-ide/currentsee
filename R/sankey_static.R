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
#'
#' @return An `htmlwidget`.
#' @export
plot_step_sankey <- function(df,
                             step_col = "step",
                             package_col = "package",
                             group_cols = character(),
                             id_col = "id") {
  inputs <- build_step_sankey_inputs(
    df = df,
    step_col = step_col,
    package_col = package_col,
    group_cols = group_cols,
    id_col = id_col
  )

  networkD3::sankeyNetwork(
    Links = inputs$links,
    Nodes = inputs$nodes,
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "label",
    NodeGroup = "step"
  )
}

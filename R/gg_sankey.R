#' Create bidirectional Sankey diagram
#'
#' Generates a Sankey diagram that can flow both left (down/removing) and right (up/adding)
#' from a central current state, with direction-appropriate flow labels and alignment.
#'
#' @param x Data frame where each column represents a step and each row represents a flow path
#' @param flow_labels Data frame with columns 'flow_start', 'flow_end', 'flow_label' for custom flow labels (optional)
#' @param colours Data frame with columns 'node' and 'colour' for custom node colours, or named vector (optional)
#' @param node_width Numeric. Width of node rectangles (default: 0.05)
#' @param node_label_font_size Numeric. Font size for node labels (default: 3)
#' @param flow_label_font_size Numeric. Font size for flow labels (default: 2.5)
#' @param x_label_font_size Numeric. Font size for x-axis tick labels
#' @param x_names Character vector. Labels for x-axis (column names by default)
#' @param flow_alpha Numeric. Transparency of flow ribbons (default: 0.3)
#' @param gradient_res Integer. Resolution of flow gradients for smoothness (default: 3000)
#'
#' @return ggplot2 object representing the Sankey diagram
#' @export
make_sankey <- function(
    x,
    flow_labels = NULL,
    colours = NULL,
    node_width = 0.05,
    node_label_font_size = 3,
    flow_label_font_size = 2.5,
    x_label_font_size = 12,
    x_names = NULL,
    flow_alpha = 0.3,
    gradient_res = 3000
) {

  # Remove any all NA cols
  x <- x[,apply(x, 2, function(x){
    !all(is.na(x))
  }), drop = FALSE]

  stopifnot(ncol(x) > 1)

  # Make node, flows and curves
  nodes <- make_nodes(x, node_width = node_width)
  flows <- make_flows(x, nodes)
  curves <- create_flow_curves(flows, nodes)

  if(is.null(x_names)){
    x_names <- make_x_labs(names(x))
  }
  x_name_colour <- rep("#36454F", length(x_names))
  x_name_colour[x_names == "Current"] <- "black"
  x_name_size <- rep(x_label_font_size, length(x_names))
  x_name_size[x_names == "Current"] <- x_label_font_size * 1.4

  p <- ggplot2::ggplot() +
    # Flow curves
    ggplot2::geom_rect(
      data = curves,
      ggplot2::aes(xmin =.data$xmin, xmax =.data$xmax,
                   ymin =.data$ymin, ymax =.data$ymax,
                   fill =.data$color),
      color = NA,  # No borders on flow segments
      alpha = flow_alpha  # Use your flow transparency setting
    ) +
    ggplot2::scale_fill_identity() +

    # Need new scale for nodes since we used fill for flows
    ggnewscale::new_scale_fill() +

    # Nodes
    ggplot2::geom_rect(
      data = nodes,
      ggplot2::aes(xmin =.data$xmin, xmax =.data$xmax,
                   ymin =.data$ymin, ymax =.data$ymax,
                   fill =.data$colour),
      color = "black"
    ) +
    ggplot2::scale_fill_identity() +

    # Node labels
    ggplot2::geom_text(
      data = nodes,
      ggplot2::aes(x =.data$xcenter, y =.data$ycenter, label =.data$label),
      inherit.aes = FALSE,
      fontface = "bold",
      color = "black",
      size = node_label_font_size
    ) +

    # Flow labels
    ggplot2::geom_text(
      data = flows,
      ggplot2::aes(x =.data$label_x, y =.data$label_y,
                   label =.data$flow_label, hjust =.data$hjust),
      inherit.aes = FALSE,
      fontface = "italic",  # Changed to italic to distinguish from node labels
      color = "black",
      size = flow_label_font_size  # Use separate size parameter
    ) +

    # Scales and theme
    ggplot2::scale_x_continuous(breaks = seq_along(x_names), labels = x_names,
                                minor_breaks = NULL, name = "") +
    ggplot2::scale_y_continuous(breaks = NULL, labels = NULL) +
    ggplot2::theme_minimal() +
    suppressWarnings({
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = x_name_size, color = x_name_colour),
        legend.position = "none"
      )
    })

  return(p)
}

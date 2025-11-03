#' Create Sankey diagram
#'
#' Generates a Sankey diagram with customizable nodes, flows, colours, and labels.
#' Supports dead-end flows (NA values), custom labeling, and flexible styling options.
#'
#' @param dat Data frame where each column represents a step and each row represents a flow path
#' @param node_labels Data frame with columns 'node' and 'label' for custom node labels (optional)
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
    dat,
    node_labels = NULL,        # data.frame with columns: node, label
    flow_labels = currentsee::flow_labels,        # data.frame with columns: flow_start, flow_end, flow_label
    colours = currentsee::colours_df,             # data.frame with columns: node, colour (or named vector for backward compatibility)
    node_width = 0.05,         # Width of the node rectangles (default: 0.05)
    node_label_font_size = 3,  # Font size for node labels (default: 3)
    flow_label_font_size = 2.5, # Font size for flow labels (default: 2.5)
    x_label_font_size = 12,
    x_names = make_x_labs(names(dat)),
    flow_alpha = 0.3,
    gradient_res = 3000
) {

  # Get unique values for color mapping
  unique_values <- unique(unlist(dat[!is.na(dat)]))

  # Handle colours parameter - support both data.frame and named vector
  if(is.null(colours)) {
    # Default colours using RColorBrewer
    default_colours <- RColorBrewer::brewer.pal(name = "Set3", n = length(unique_values))
    df_c <- data.frame(var = levels(factor(unique_values)), color = default_colours)
  } else if(is.data.frame(colours)) {
    # User provided data.frame with node and colour columns
    df_c <- colours |>
      dplyr::rename(var = node, color = colour) |>
      # Ensure all unique values have colours, use default for missing
      dplyr::right_join(data.frame(var = levels(factor(unique_values))), by = "var") |>
      dplyr::mutate(color = ifelse(is.na(color),
                                   RColorBrewer::brewer.pal(name = "Set3", n = length(unique_values))[row_number()],
                                   color))
  } else {
    # Backward compatibility: assume it's a named vector or regular vector
    df_c <- data.frame(var = levels(factor(unique_values)), color = colours)
  }

  # Calculate node positions
  df_n <- calculate_node_positions(dat, node_width)

  # Calculate flow transitions
  df_f_base <- calculate_flow_transitions(dat)

  # Only proceed with flow visualization if there are actual flows
  if(nrow(df_f_base) > 0) {
    df_f_positioned <- df_f_base |>
      dplyr::left_join(df_c, by = c(Var1 = "var")) |>
      dplyr::left_join(df_c, by = c(Var2 = "var"),
                       suffix = c("_left", "_right")) |>
      dplyr::left_join(select(df_n, -Freq, -ymax), by = c(Var1 = "Var")) |>
      dplyr::left_join(select(df_n, -Freq, -ymax), by = c(Var2 = "Var"),
                       suffix = c("_left", "_right")) |>
      dplyr::mutate(ymin_left = ymin_left + c(0, head(cumsum(Freq), -1)),
                    ymax_left = ymin_left + Freq,.by = Var1) |>
      dplyr::mutate(ymin_right = ymin_right + c(0, head(cumsum(Freq), -1)),
                    ymax_right = ymin_right + Freq,.by = Var2)

    # Prepare flow labels
    df_f_labels <- prepare_flow_labels(df_f_positioned, flow_labels)

    # Create detailed flow curves
    df_f <- create_flow_curves(df_f_positioned, gradient_resolution = gradient_res)

  } else {
    # Create empty data frames if no flows exist
    df_f <- data.frame(xmin = numeric(0), xmax = numeric(0),
                       ymin = numeric(0), ymax = numeric(0), color = character(0))
    df_f_labels <- data.frame(Var1 = character(0), Var2 = character(0),
                              x_center = numeric(0), y_center = numeric(0),
                              display_label = character(0))
  }

  # Add color to node data and prepare labels
  df_n <- df_n |>
    dplyr::left_join(df_c, by = c(Var = "var")) |>
    prepare_node_labels(node_labels)

  # Create the plot with clean aesthetics
  p <- ggplot2::ggplot(df_f, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
    ggplot2::geom_rect(ggplot2::aes(fill = color), color = NA, alpha = flow_alpha) +
    ggplot2::scale_fill_identity() +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_rect(data = df_n, ggplot2::aes(fill = color), color = "black") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(breaks = seq_along(dat), labels = x_names,
                                minor_breaks = NULL, name = "") +
    ggplot2::scale_y_continuous(breaks = NULL, labels = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = x_label_font_size, color = "black"),
      legend.position = "none"
    )

  # Add node labels
  if(nrow(df_n) > 0) {
    p <- p +
      ggplot2::geom_text(
        data = df_n,
        ggplot2::aes(x = x_center, y = y_center, label = display_label),
        inherit.aes = FALSE,
        fontface = "bold",
        color = "black",
        size = node_label_font_size
      )
  }

  # Add flow labels
  if(!is.null(flow_labels) && nrow(df_f_labels) > 0) {
    p <- p +
      ggplot2::geom_text(
        data = df_f_labels,
        ggplot2::aes(x = x_center, y = y_center, label = display_label),
        inherit.aes = FALSE,
        fontface = "italic",
        color = "black",
        size = flow_label_font_size,
        hjust = 0
      )  # Left-align so text starts at the position
  }

  return(p)
}


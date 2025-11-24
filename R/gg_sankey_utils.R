#' @importFrom rlang .data
NULL

#' Calculate node positions and dimensions
#'
#' Computes the position, size, and center coordinates for each node in the Sankey diagram.
#' Handles NA values by excluding them from calculations and centers nodes vertically.
#'
#' @param x Data frame containing the flow data with columns representing steps
#' @param node_width Numeric. Width of the node rectangles (default: 0.05)
#'
#' @return Data frame with node positions including Var, Freq, xmin, xmax, ymin, ymax, x_center, y_center
make_nodes <- function(x, node_width = 0.05) {
  rows <- lapply(seq_along(x), function(i) {
    gap <- nrow(x) / 10
    valid_data <- x[[i]][!is.na(x[[i]])]

    if (length(valid_data) == 0) {
      return(data.frame(
        node = character(0), Freq = numeric(0),
        ymin = numeric(0), ymax = numeric(0),
        xmin = numeric(0), xmax = numeric(0),
        x_center = numeric(0), y_center = numeric(0)
      ))
    }

    df <- as.data.frame(table(valid_data))
    names(df) <- c("node", "Freq")
    n <- nrow(df)

    # gaps go BETWEEN bars:
    cum_prev <- c(0, utils::head(cumsum(df$Freq), -1))       # sum of previous Freqs
    gaps_prev <- (seq_len(n) - 1) * gap               # number of gaps before each bar
    df$ymin <- cum_prev + gaps_prev
    df$ymax <- df$ymin + df$Freq

    # centre vertically
    centre <- 0.5 * max(df$ymax)
    df$ymin <- df$ymin - centre
    df$ymax <- df$ymax - centre

    # x extents and label centres
    df$xmin <- i - node_width
    df$xmax <- i + node_width
    df$xcenter <- i
    df$ycenter <- (df$ymin + df$ymax) / 2

    df
  })

  nodes <- do.call("rbind", rows) |>
    dplyr::left_join(currentsee::colours_df, by = "node") |>
    dplyr::mutate(percent = round(100 * (.data$Freq / nrow(x)))) |>
    dplyr::mutate(label = paste0(.data$node, "\n(", .data$percent, "%)")) |>
    dplyr::mutate(label = add_line_breaks_smart(.data$label, width = 12))

  return(nodes)
}

#' Create flow transitions for bidirectional Sankey diagrams
#'
#' Processes intervention scaling data to generate flow transitions between nodes
#' in both upward (adding interventions) and downward (removing interventions)
#' directions. Calculates flow positions, coordinates, and label placements while
#' minimizing visual crossings through intelligent sorting algorithms.
#'
#' @param x Data frame containing intervention scaling data where columns represent
#'   different intervention states (named with numeric values: negative for removal
#'   steps, 0 for current state, positive for addition steps) and rows represent
#'   individual flow paths through the intervention scaling process
#' @param nodes Data frame containing node information
#'
#' @details
#' The function performs several key operations:
#' \itemize{
#'   \item Identifies upward transitions (positive column progressions) representing intervention additions
#'   \item Identifies downward transitions (negative column progressions) representing intervention removals
#'   \item Calculates flow frequencies and positions within source and destination nodes
#'   \item Applies intelligent sorting to minimize visual flow crossings
#'   \item Determines appropriate label positioning and alignment based on flow direction
#'   \item Joins with external flow_labels data for custom flow descriptions
#' }
#'
#' Flow positioning uses a two-pass sorting algorithm: first sorting flows within
#' each source node by destination position, then sorting flows within each
#' destination node by source position. This approach significantly reduces
#' visual crossing of flow ribbons in the final Sankey diagram.
#'
#' @return Data frame containing detailed flow information with columns:
#'   \itemize{
#'     \item \code{flow_start}, \code{flow_end}: Character. Source and destination node names
#'     \item \code{direction}: Character. Flow direction ("up" or "down")
#'     \item \code{n}: Integer. Number of individual transitions for this flow
#'     \item \code{*_start}, \code{*_end}: Numeric. Coordinate boundaries for flow start and end points
#'     \item \code{flow_start_center}, \code{flow_end_center}: Numeric. Center positions of flows
#'     \item \code{label_x}, \code{label_y}: Numeric. Optimal label positioning coordinates
#'     \item \code{hjust}: Numeric. Horizontal text alignment (0 for left, 1 for right)
#'     \item \code{flow_label}: Character. Custom flow descriptions (if available in flow_labels)
#'   }
#' @export
make_flows <- function(x, nodes){
  cols <- as.numeric(colnames(x))

  down_transitions <- list()
  if(min(cols)<= -1){
    down_transitions <- lapply(-1:min(cols), function(y){
      v12 <- x[,paste0(y:(y+1))]
      names(v12) <- c("flow_start", "flow_end")
      v12$direction <- "down"
      return(v12)
    }) |>
      dplyr::bind_rows()
  }

  up_transitions <- list()
  if(max(cols) >= 1){
    up_transitions <- lapply(1:max(cols), function(y){
      v12 <- x[,paste0((y-1):y)]
      names(v12) <- c("flow_start", "flow_end")
      v12$direction <- "up"
      return(v12)
    }) |>
      dplyr::bind_rows()
  }

  start_position <- nodes |>
    dplyr::select(dplyr::all_of(c("node", "ymin", "ymax", "xmin", "xmax")))
  names(start_position) = c("node", paste0(names(start_position)[-1], "_start"))

  end_position <- nodes |>
    dplyr::select(dplyr::all_of(c("node", "ymin", "ymax", "xmin", "xmax")))
  names(end_position) = c("node", paste0(names(end_position)[-1], "_end"))

  transitions <- down_transitions |>
    dplyr::bind_rows(up_transitions) |>
    tidyr::drop_na() |>
    dplyr::summarise(
      n = dplyr::n(),.by = c("flow_start", "flow_end", "direction")
    ) |>
    dplyr::left_join(start_position, by = c("flow_start" = "node")) |>
    dplyr::left_join(end_position, by = c("flow_end" = "node")) |>
    dplyr::mutate(
      label_start = ifelse(.data$direction == "down",.data$flow_end,.data$flow_start),
      label_end = ifelse(.data$direction == "down",.data$flow_start,.data$flow_end)
    ) |>
    dplyr::left_join(
      flow_labels,
      by = c("label_start"  = "flow_start", "label_end" = "flow_end")
    ) |>
    dplyr::select(-label_start, -label_end) |>
    # Flow starts and ends
    dplyr::mutate(
      flow_start_x = xmax_start,
      flow_end_x = xmin_end
    ) |>
    # ADD NODE CENTER POSITIONS FOR SORTING
    dplyr::left_join(
      dplyr::select(nodes, node, ycenter),
      by = c("flow_start" = "node")
    ) |>
    dplyr::rename(start_center = ycenter) |>
    dplyr::left_join(
      dplyr::select(nodes, node, ycenter),
      by = c("flow_end" = "node")
    ) |>
    dplyr::rename(end_center = ycenter) |>
    # Sort for start position
    dplyr::arrange(.data$flow_start,.data$end_center) |>  # Sort by start node, then by end node position
    # Calculate start positions
    dplyr::mutate(
      flow_start_ymin =.data$ymin_start + c(0, utils::head(cumsum(.data$n), -1)),
      flow_start_ymax =.data$flow_start_ymin +.data$n,
      .by = "flow_start"
    ) |>
    # sort for end position
    dplyr::arrange(.data$flow_end,.data$start_center) |>  # Sort by end node, then by start node position
    # Calculate end positions
    dplyr::mutate(
      flow_end_ymin =.data$ymin_end + c(0, utils::head(cumsum(.data$n), -1)),
      flow_end_ymax =.data$flow_end_ymin +.data$n,
      .by = "flow_end"
    ) |>
    # Clean up temporary columns
    dplyr::select(-start_center, -end_center) |>
    dplyr::mutate(
      flow_start_center =.data$flow_start_ymin + ((.data$flow_start_ymax -.data$flow_start_ymin) / 2),
      flow_end_center =.data$flow_end_ymin + ((.data$flow_end_ymax -.data$flow_end_ymin) / 2)
    ) |>
    # Labels
    dplyr::mutate(
      hjust = ifelse(direction == "down", 1, 0),
      label_x = ifelse(direction == "down",.data$flow_end_x,.data$flow_start_x),
      label_y = ifelse(direction == "down",.data$flow_end_center,.data$flow_start_center)
    )

  return(transitions)
}

#' Create detailed flow visualization data with node-based color gradients
#'
#' Generates smooth gradient flows between nodes using the actual node colors
#' from the nodes data frame for realistic color transitions.
#'
#' @param flows Data frame with flow data including flow_start and flow_end node names
#' @param nodes Data frame with node information including 'node' and 'colour' columns
#' @param gradient_resolution Integer. Number of segments for gradient smoothness (default: 2000)
#'
#' @return Data frame with detailed flow rendering data including color gradients
create_flow_curves <- function(flows, nodes, gradient_resolution = 2000) {

  if (nrow(flows) == 0) {
    return(data.frame(
      xmin = numeric(0), xmax = numeric(0),
      ymin = numeric(0), ymax = numeric(0),
      color = character(0)
    ))
  }

  # Join flows with node colors for start and end nodes
  flows_with_colors <- flows |>
    dplyr::left_join(
      dplyr::select(nodes, node, colour),
      by = c("flow_start" = "node")
    ) |>
    dplyr::rename(color_start = colour) |>
    dplyr::left_join(
      dplyr::select(nodes, node, colour),
      by = c("flow_end" = "node")
    ) |>
    dplyr::rename(color_end = colour)

  # Create the flow curves with color gradients
  flows_with_colors |>
    dplyr::rowwise() |>
    dplyr::reframe(
      # Create color gradient between start and end node colors
      color = if(!is.na(.data$color_start) && !is.na(.data$color_end)) {
        grDevices::colorRampPalette(
          c(.data$color_start,.data$color_end)
        )(gradient_resolution)
      } else {
        # Fallback to single color if one is missing
        rep(dplyr::coalesce(.data$color_start,.data$color_end, "#69b3a2"), gradient_resolution)
      },

      # Create x coordinates for the flow segments
      xmin = seq(.data$flow_start_x,.data$flow_end_x,
                 length.out = gradient_resolution) - 0.001,
      xmax =.data$xmin + 0.002,

      # Create smooth y coordinates using sigmoid interpolation
      ymin = sankey_sigmoid(.data$flow_start_x,.data$flow_end_x,.data$flow_start_ymin,.data$flow_end_ymin,
                            gradient_resolution
      ),
      ymax = sankey_sigmoid(.data$flow_start_x,.data$flow_end_x,.data$flow_start_ymax,.data$flow_end_ymax,
                            gradient_resolution
      )
    )
}

#' Sigmoid interpolation function for smooth flow curves
#'
#' Creates smooth sigmoid curves for Sankey diagram flows using normal distribution.
#' This function generates smooth transitions between flow start and end points.
#'
#' @param x1 Numeric. Starting x-coordinate of the flow
#' @param x2 Numeric. Ending x-coordinate of the flow
#' @param y1 Numeric. Starting y-coordinate of the flow
#' @param y2 Numeric. Ending y-coordinate of the flow
#' @param n_points Integer. Number of interpolation points for smoothness (default: 5000)
#'
#' @return Numeric vector of interpolated y-coordinates
sankey_sigmoid <- function(x1, x2, y1, y2, n_points = 5000) {
  y1 + (y2 - y1) * stats::pnorm(seq(x1, x2, len = n_points), (x1 + x2)/2, (x2 - x1)/6)
}

#' Add smart line breaks to text strings
#'
#' Intelligently wraps text strings by adding line breaks at appropriate positions.
#' Can break at word boundaries (default) or force character-based breaking for long words.
#' Handles NA values and strings shorter than the specified width gracefully.
#'
#' @param x Character vector. Text strings to wrap with line breaks
#' @param width Integer. Maximum line width in characters (default: 80)
#' @param break_words Logical. If TRUE, breaks long words at character boundaries.
#'   If FALSE (default), attempts to break at word boundaries and only forces
#'   character breaks for words longer than width
#'
#' @return Character vector of the same length as input with line breaks added
#'   using "\\n" characters. NA values are returned unchanged.
#'
#' @export
add_line_breaks_smart <- function(x, width = 80, break_words = FALSE) {
  sapply(x, function(string) {
    if (is.na(string) || nchar(string) <= width) {
      return(string)
    }

    if (break_words) {
      # Simple character-based breaking
      chunks <- substring(
        string,
        seq(1, nchar(string), width),
        seq(width, nchar(string), width)
      )
      return(paste(chunks, collapse = "\n"))
    }

    # Word-boundary aware breaking
    words <- strsplit(string, " ")[[1]]
    lines <- character()
    current_line <- ""

    for (word in words) {
      test_line <- if (current_line == "") word else paste(current_line, word)

      if (nchar(test_line) <= width) {
        current_line <- test_line
      } else {
        if (current_line != "") {
          lines <- c(lines, current_line)
          current_line <- word
        } else {
          # Word is longer than width, force break
          lines <- c(lines, word)
          current_line <- ""
        }
      }
    }

    if (current_line != "") {
      lines <- c(lines, current_line)
    }

    paste(lines, collapse = "\n")
  }, USE.NAMES = FALSE)
}

#' Create descriptive x-axis labels for intervention scaling
#'
#' Converts numeric intervention scaling values to descriptive labels for Sankey
#' diagram x-axes. Handles current state (0), adding interventions (positive values),
#' and removing interventions (negative values) with appropriate formatting.
#'
#' @param x Numeric vector representing intervention scaling steps where:
#'   \itemize{
#'     \item 0 = Current state
#'     \item Positive values = Number of interventions to add
#'     \item Negative values = Number of interventions to remove
#'   }
#'
#' @return Character vector of formatted labels with line breaks:
#'   \itemize{
#'     \item "Current" for value 0
#'     \item "Add X\\nInterventions" for positive values
#'     \item "Remove X\\nInterventions" for negative values
#'   }
#'
#' @export
make_x_labs <- function(x){
  x <- as.numeric(x)
  names <- character(length(x))
  names[x == 0] <- "Current"
  names[x == 1] <- "Add 1\nintervention"
  names[x == -1] <- "Remove 1\nintervention"
  names[x > 1] <- paste("Add ", x[x > 1], "\ninterventions")
  names[x < -1] <- paste("Remove ", abs(x[x < -1]), "\ninterventions")
  return(names)
}

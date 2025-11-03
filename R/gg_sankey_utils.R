#' @importFrom rlang .data
NULL

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


#' Calculate node positions and dimensions
#'
#' Computes the position, size, and center coordinates for each node in the Sankey diagram.
#' Handles NA values by excluding them from calculations and centers nodes vertically.
#'
#' @param dat Data frame containing the flow data with columns representing steps
#' @param node_width Numeric. Width of the node rectangles (default: 0.05)
#'
#' @return Data frame with node positions including Var, Freq, xmin, xmax, ymin, ymax, x_center, y_center
calculate_node_positions <- function(dat, node_width = 0.05) {
  rows <- lapply(seq_along(dat), function(i) {
    gap <- nrow(dat) / 10
    valid_data <- dat[[i]][!is.na(dat[[i]])]

    if (length(valid_data) == 0) {
      return(data.frame(
        Var = character(0), Freq = numeric(0),
        ymin = numeric(0), ymax = numeric(0),
        xmin = numeric(0), xmax = numeric(0),
        x_center = numeric(0), y_center = numeric(0)
      ))
    }

    df <- as.data.frame(table(valid_data))
    names(df) <- c("Var", "Freq")
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
    df$x_center <- i
    df$y_center <- (df$ymin + df$ymax) / 2

    df
  })

  do.call("rbind", rows)
}

#' Calculate flow transitions between columns
#'
#' Determines valid transitions between consecutive columns in the data,
#' excluding transitions involving NA values and computing frequencies.
#'
#' @param dat Data frame containing the flow data with columns representing steps
#'
#' @return Data frame with columns Var1, Var2, and Freq representing transitions
calculate_flow_transitions <- function(dat) {
  do.call("rbind", lapply(utils::head(seq_along(dat), -1), function(i) {
    # Create a data frame of transitions, excluding rows where either value is NA
    transitions <- data.frame(
      Var1 = dat[[i]],
      Var2 = dat[[i + 1]]
    )

    # Remove rows with NA in either column
    transitions <- transitions[!is.na(transitions$Var1) & !is.na(transitions$Var2), ]

    if(nrow(transitions) == 0) {
      return(data.frame(Var1 = character(0), Var2 = character(0), Freq = numeric(0)))
    }

    table(transitions$Var1, transitions$Var2) |>
      as.data.frame() |>
      dplyr::filter(.data$Freq > 0)  # Only keep actual transitions
  }))
}

#' Create detailed flow visualization data with smooth curves
#'
#' Generates the detailed geometric data needed to render smooth gradient flows
#' between nodes, including color interpolation and curve coordinates.
#'
#' @param df_f_positioned Data frame with positioned flow data including color and coordinate information
#' @param gradient_resolution Integer. Number of segments for gradient smoothness (default: 2000)
#'
#' @return Data frame with detailed flow rendering data including color, xmin, xmax, ymin, ymax
create_flow_curves <- function(df_f_positioned, gradient_resolution = 2000) {
  df_f_positioned |>
    dplyr::rowwise() |>
    dplyr::reframe(
      color = grDevices::colorRampPalette(
        c(.data$color_left, .data$color_right)
      )(gradient_resolution),

      xmin = seq(.data$xmax_left, .data$xmin_right,
                 length.out = gradient_resolution) - 0.001,
      xmax = .data$xmin + 0.002,

      ymin = sankey_sigmoid(
        .data$xmax_left, .data$xmin_right,
        .data$ymin_left, .data$ymin_right,
        gradient_resolution
      ),
      ymax = sankey_sigmoid(
        .data$xmax_left, .data$xmin_right,
        .data$ymax_left, .data$ymax_right,
        gradient_resolution
      )
    )
}

#' Prepare flow label positions
#'
#' Calculates optimal positions for flow labels and joins with user-provided
#' label data. Positions labels at the start of flows with proportional spacing.
#'
#' @param df_f_positioned Data frame with positioned flow data
#' @param flow_labels Data frame with columns flow_start, flow_end, flow_label (optional)
#'
#' @return Data frame with flow label positions and text
prepare_flow_labels <- function(df_f_positioned, flow_labels = NULL) {
  df_f_labels <- df_f_positioned |>
    dplyr::mutate(
      flow_distance = .data$xmin_right - .data$xmax_left,
      x_center = .data$xmax_left + (.data$flow_distance * 0.01),
      y_center = (.data$ymin_left + .data$ymax_left) / 2
    ) |>
    dplyr::select(dplyr::all_of(c("Var1", "Var2", "x_center", "y_center"))) |>
    dplyr::distinct()

  if (!is.null(flow_labels)) {
    df_f_labels <- df_f_labels |>
      dplyr::left_join(
        flow_labels,
        by = c("Var1" = "flow_start", "Var2" = "flow_end")
      ) |>
      dplyr::mutate(
        display_label = dplyr::coalesce(.data$flow_label,
                                        paste(.data$Var1, "\u2192", .data$Var2))
      )
  } else {
    df_f_labels <- df_f_labels |>
      dplyr::mutate(display_label = paste(.data$Var1, "\u2192", .data$Var2))
  }

  df_f_labels
}

#' Prepare node labels
#'
#' Joins node data with user-provided labels and creates display labels.
#' Falls back to original values when custom labels are not provided.
#'
#' @param df_n Data frame with node data
#' @param node_labels Data frame with columns node and label (optional)
#'
#' @return Data frame with display_label column added
prepare_node_labels <- function(df_n, node_labels = NULL) {
  # Join with node_labels if provided
  if(!is.null(node_labels)) {
    df_n <- df_n |>
      dplyr::left_join(node_labels, by = c("Var" = "node")) |>
      dplyr::mutate(display_label = dplyr::coalesce(.data$label, .data$Var))
  } else {
    df_n <- df_n |>
      dplyr::mutate(display_label = .data$Var)
  }

  return(df_n)
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

#' Process data for upward scaling Sankey diagram
#'
#' Processes intervention scaling data for upward flows (adding interventions).
#' Extracts columns for current state and 1-3 additional interventions, calculates
#' percentages, creates formatted node labels, and removes columns with all NA values.
#'
#' @param dat Data frame containing intervention scaling data with columns named
#'   "0", "1", "2", "3" representing current state and progressive intervention additions
#'
#' @return List containing two elements:
#'   \item{up}{Data frame with processed upward scaling data, NA-only columns removed}
#'   \item{up_nodes}{Data frame with columns 'node' and 'label' containing formatted
#'     node labels with percentages and line breaks for display}
#'
#' @export
nodes_up <- function(dat){
  up <- dat |>
    dplyr::select(dplyr::any_of(c("0", "1", "2", "3"))) |>
    dplyr::select(dplyr::where(~!all(is.na(.x))))

  up_nodes <- apply(up, 2, function(x){
    table(x) |>
      as.data.frame()
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(percent = round(100 * (.data$Freq / nrow(up)))) |>
    dplyr::mutate(label = paste0(.data$x, "\n(", .data$percent, "%)")) |>
    dplyr::mutate(label = add_line_breaks_smart(.data$label, width = 15)) |>
    dplyr::select("x", "label") |>
    dplyr::rename(node = "x")

  return(
    list(
      up = up,
      up_nodes = up_nodes
    )
  )
}

#' Process data for downward scaling Sankey diagram
#'
#' Processes intervention scaling data for downward flows (removing interventions).
#' Extracts columns for current state and 1-3 intervention removals, calculates
#' percentages, creates formatted node labels, and removes columns with all NA values.
#'
#' @param dat Data frame containing intervention scaling data with columns named
#'   "0", "-1", "-2", "-3" representing current state and progressive intervention removals
#'
#' @return List containing two elements:
#'   \item{down}{Data frame with processed downward scaling data, NA-only columns removed}
#'   \item{down_nodes}{Data frame with columns 'node' and 'label' containing formatted
#'     node labels with percentages and line breaks for display}
#'
#' @export
nodes_down <- function(dat){
  down <- dat |>
    dplyr::select(dplyr::any_of(c("0", "-1", "-2", "-3"))) |>
    dplyr::select(dplyr::where(~!all(is.na(.x))))

  down_nodes <- apply(down, 2, function(x){
    table(x) |>
      as.data.frame()
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(percent = round(100 * (.data$Freq / nrow(down)))) |>
    dplyr::mutate(label = paste0(.data$x, "\n(", .data$percent, "%)")) |>
    dplyr::mutate(label = add_line_breaks_smart(.data$label, width = 15)) |>
    dplyr::select("x", "label") |>
    dplyr::rename(node = "x")


  return(
    list(
      down = down,
      down_nodes = down_nodes
    )
  )
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

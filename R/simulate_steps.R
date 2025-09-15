#' Simulate prioritisation step data
#'
#' Generate example step and package data for testing Sankey diagrams.
#'
#' @param n Number of simulations per grouping combination.
#' @param all_interventions All possible interventions.
#' @param removable Interventions that can be added or removed.
#' @param pfpr Levels of PfPr grouping variable.
#' @param seasonality Levels of seasonality grouping variable.
#'
#' @return A data frame with columns for grouping variables, simulation id,
#'   step number, and package label.
#' @export
simulate_step_data <- function(n = 10,
                               all_interventions = c("ITN", "Chemoprevention", "Vaccine", "CM"),
                               removable = c("ITN", "Chemoprevention", "Vaccine"),
                               pfpr = c("low", "high"),
                               seasonality = c("low", "high")) {
  groups <- tidyr::crossing(pfpr = pfpr, seasonality = seasonality)
  res_list <- list()
  counter <- 1

  for (g in seq_len(nrow(groups))) {
    grp_vals <- groups[g, , drop = FALSE]
    for (id in seq_len(n)) {
      base_n <- sample(length(all_interventions), 1)
      current <- sort(sample(all_interventions, base_n))
      steps <- list(data.frame(step = 0L,
                               package = paste(current, collapse = " & "),
                               stringsAsFactors = FALSE))

      current_neg <- current
      for (s in seq_along(removable)) {
        removable_now <- intersect(current_neg, removable)
        if (length(removable_now) == 0) break
        rem <- sample(removable_now, 1)
        current_neg <- setdiff(current_neg, rem)
        pkg <- if (length(current_neg) > 0) paste(sort(current_neg), collapse = " & ") else "None"
        steps[[length(steps) + 1]] <- data.frame(step = -s,
                                                 package = pkg,
                                                 stringsAsFactors = FALSE)
      }

      current_pos <- current
      to_add <- setdiff(removable, current_pos)
      if (length(to_add) > 0) {
        add_order <- sample(to_add)
        for (s in seq_along(add_order)) {
          current_pos <- sort(c(current_pos, add_order[s]))
          pkg <- paste(current_pos, collapse = " & ")
          steps[[length(steps) + 1]] <- data.frame(step = s,
                                                   package = pkg,
                                                   stringsAsFactors = FALSE)
        }
      }

      step_df <- dplyr::bind_rows(steps)
      step_df$step <- as.integer(step_df$step)
      step_df$id <- id
      step_df <- cbind(step_df, grp_vals)
      res_list[[counter]] <- step_df
      counter <- counter + 1
    }
  }

  dplyr::bind_rows(res_list)
}

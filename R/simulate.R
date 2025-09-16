#' @keywords internal
simulate_single <- function(int, current, weights, id = NULL) {
  out <- data.frame(
    step = 0,
    package = paste(current, collapse = ", "),
    changed = NA_character_,
    stringsAsFactors = FALSE
  )

  package <- current
  i <- -1
  while (length(package) > 1) {
    opts <- setdiff(package, "cm")
    if (length(opts) == 0) {
      break
    }
    down_weights <- weights[opts]
    if (any(is.na(down_weights))) {
      stop("Missing `weights` for packages: ", paste(opts[is.na(down_weights)], collapse = ", "))
    }
    down <- sample(opts, 1, prob = 1 - down_weights)
    package <- setdiff(package, down)
    package <- package[order(match(package, int))]
    out_add <- data.frame(
      step = i,
      package = paste(package, collapse = ", "),
      changed = down,
      stringsAsFactors = FALSE
    )
    out <- rbind(out, out_add)
    i <- i - 1
  }

  package <- current
  i <- 1
  while (length(package) < length(int)) {
    opts <- setdiff(int, package)
    up_weights <- weights[opts]
    if (any(is.na(up_weights))) {
      stop("Missing `weights` for packages: ", paste(opts[is.na(up_weights)], collapse = ", "))
    }
    up <- sample(opts, 1, prob = up_weights)
    package <- c(as.character(package), up)
    package <- package[order(match(package, int))]
    out_add <- data.frame(
      step = i,
      package = paste(package, collapse = ", "),
      changed = up,
      stringsAsFactors = FALSE
    )
    out <- rbind(out, out_add)
    i <- i + 1
  }

  out$id <- id
  out <- out[order(out$step), , drop = FALSE]

  out
}

#' Simulate package transitions for Sankey examples
#'
#' Generate stochastic sequences of package adoption to help demonstrate Sankey
#' diagrams. Each simulation begins from `current` packages and explores
#' removals and additions until all packages in `int` have been visited.
#'
#' @param n Number of independent simulations to draw.
#' @param int Character vector giving the ordered set of all possible packages.
#' @param current Character vector of packages present at the baseline step.
#' @param weights Named numeric vector of selection probabilities for packages
#'   in `int`. Values should lie between 0 and 1.
#'
#' @return A tibble containing one row per step and simulation id.
#'
#' @examples
#' set.seed(1)
#' simulate(2)
#'
#' @export
simulate <- function(
    n,
    int = c("cm", "itn", "smc", "vx", "irs"),
    current = c("cm", "itn"),
    weights = c(itn = 0.6, smc = 0.4, vx = 0.2, irs = 0.5)
) {
  if (length(n) != 1 || !is.numeric(n) || is.na(n) || n < 1) {
    stop("`n` must be a positive integer of length 1.")
  }

  n <- as.integer(n)

  if (!all(current %in% int)) {
    stop("All `current` packages must be present in `int`.")
  }

  if (!all(names(weights) %in% int)) {
    stop("Names of `weights` must correspond to entries in `int`.")
  }

  if (any(weights < 0 | weights > 1, na.rm = TRUE)) {
    stop("`weights` must contain probabilities between 0 and 1.")
  }

  df <- lapply(seq_len(n), function(x, ...) {
    simulate_single(id = x, ...)
  }, int = int, current = current, weights = weights) |>
    dplyr::bind_rows()

  dplyr::as_tibble(df)
}

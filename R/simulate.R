simulate_single <- function(int, current, weights, id = NULL){
  out <- data.frame(
    step = 0,
    package = paste(current, collapse = ", "),
    changed = NA
  )

  package <- current
  i <- -1
  while(length(package) > 1){
    opts <- setdiff(package, "cm")
    down <- sample(opts, 1, prob = 1 - weights[opts])
    package <- setdiff(package, down)
    package <- package[order(match(package, int))]
    out_add <- data.frame(
      step = i,
      package =  paste(package, collapse = ", "),
      changed = down
    )
    out <- rbind(out, out_add)
    i <- i-1
  }

  package <- current
  i <- 1
  while(length(package) < length(int)){
    opts <- setdiff(int, package)
    up <- sample(opts, 1, prob = weights[opts])
    package <- c(as.character(package), up)
    package <- package[order(match(package, int))]
    out_add <- data.frame(
      step = i,
      package =  paste(package, collapse = ", "),
      changed = up
    )
    out <- rbind(out, out_add)
    i <- i+1
  }

  out$id <- id
  out <- out[order(out$step),]

  return(out)
}

simulate <- function(
    n,
    int = c("cm", "itn", "smc", "vx", "irs"),
    current = c("cm", "itn"),
    weights = c(itn = 0.6, smc = 0.4, vx = 0.2, irs = 0.5)
){
  df <- lapply(1:n, function(x, ...){
    simulate_single(id = x, ...)
  }, int = int, current = current, weights = weights) |>
    dplyr::bind_rows()
  return(df)
}

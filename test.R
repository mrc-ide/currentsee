sim <- function(id = NULL, int = c("cm", "itn", "smc", "vx"), current = c("cm", "itn"), weights = c(itn = 0.6, smc = 0.4, vx = 0.2)){
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
    package <- factor(c(as.character(package), up), levels = int)
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

df <- lapply(1:100, function(x){
  sim(x)
}) |>
  dplyr::bind_rows()


# nodes
nodes <- data.frame(name = sort(unique(df$package)))

# build consecutive - and valid - links
links <- df |>
  dplyr::arrange(id, step) |>
  dplyr::mutate(next_package = dplyr::lead(package),
                next_step    = dplyr::lead(step),
                .by = "id") |>
  dplyr::count(package, next_package, step, name = "value") |>
  dplyr::mutate(
    source = match(package, nodes$name) - 1L,
    target = match(next_package, nodes$name) - 1L,
    label = gsub(", ", "\n", package),
    x_discrete
  )


networkD3::sankeyNetwork(
  Links  = dplyr::filter(links, !is.na(next_package)),
  Nodes  = nodes,
  Source = "source",
  Target = "target",
  Value  = "value",
  NodeID = "name"
)

library(ggplot2)
library(ggsankey)

labels <- min(links$step):max(links$step)
labels[labels == 0] <- "Current"

ggplot(links,
       aes(
         x = step,
         next_x = step + 1,
         node = package,
         next_node = next_package,
         fill = factor(package),
         value = value,
         label = label
       )
) +
  geom_sankey(flow.alpha = 0.4, width = 0.2) +
  geom_sankey_text(size = 5, color = "black") +
  scale_x_continuous(labels = labels, name = "") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(colour = "black")
  )

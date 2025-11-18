flow_labels <- read.csv("data-raw/flow_labels.csv") |>
  unique()
usethis::use_data(flow_labels, overwrite = TRUE)

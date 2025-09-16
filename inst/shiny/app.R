opts <- getOption("currentsee.app")
df <- opts$df
group_cols <- opts$group_cols

library(shiny)
library(dplyr)
library(tidyr)
library(networkD3)

ui <- fluidPage(
  titlePanel("Step-package Sankey"),
  sidebarLayout(
    sidebarPanel(
      lapply(group_cols, function(gc) {
        selectInput(
          inputId = gc,
          label = gc,
          choices = c("All", sort(unique(df[[gc]]))),
          selected = "All"
        )
      })
    ),
    mainPanel(
      networkD3::sankeyNetworkOutput("sankey", height = "500px")
    )
  )
)

server <- function(input, output, session) {
  filtered <- reactive({
    d <- df
    for (gc in group_cols) {
      val <- input[[gc]]
      if (!is.null(val) && val != "All") {
        d <- d[d[[gc]] == val, , drop = FALSE]
      }
    }
    nodes <- make_nodes(d)
    links <- make_links(d, nodes)
    colours <- make_colours(nodes$id)
    return(list(
      nodes = nodes,
      links = links,
      colours = colours
    ))
  })

  output$sankey <- networkD3::renderSankeyNetwork({
    currentsee::makes_sankey(
      filtered()$nodes,
      filtered()$links,
      filtered()$colours
    )
  })
}

shinyApp(ui, server)

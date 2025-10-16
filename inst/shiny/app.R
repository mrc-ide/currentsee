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
      h4("Increasing spend"),
      networkD3::sankeyNetworkOutput("sankey_up", height = "500px"),
      h4("Decreasing spend"),
      networkD3::sankeyNetworkOutput("sankey_down", height = "500px")
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

    d_up <- d[d$step >=0, ]
    nodes_up <- make_nodes(d_up)
    links_up <- make_links(d_up, nodes_up)

    d_down <- d[d$step <=0, ]
    nodes_down <- make_nodes(d_down)
    links_down <- make_links(d_down, nodes_down, down = TRUE)

    return(list(
      nodes_down = nodes_down,
      links_down = links_down,
      nodes_up = nodes_up,
      links_up = links_up
    ))
  })

  output$sankey_up <- networkD3::renderSankeyNetwork({
    currentsee::make_sankey(
      filtered()$nodes_up,
      filtered()$links_up,
      nodeWidth = 30
    )
  })
  output$sankey_down <- networkD3::renderSankeyNetwork({
    currentsee::make_sankey(
      filtered()$nodes_down,
      filtered()$links_down,
      nodeWidth = 30
    )
  })
}

shinyApp(ui, server)

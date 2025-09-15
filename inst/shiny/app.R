opts <- getOption("currentsee.app")
df <- opts$df
cost_col <- opts$cost_col
effect_col <- opts$effect_col
group_cols <- opts$group_cols

library(shiny)
library(dplyr)
library(tidyr)
library(networkD3)

ui <- fluidPage(
  titlePanel("Cost-effectiveness Sankey"),
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
    d
  })

  output$sankey <- networkD3::renderSankeyNetwork({
    currentsee::plot_ce_sankey(
      filtered(),
      cost_col = cost_col,
      effect_col = effect_col,
      group_cols = group_cols,
      engine = "networkD3"
    )
  })
}

shinyApp(ui, server)

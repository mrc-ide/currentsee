opts <- getOption("currentsee.app")
df <- opts$df
step_col <- opts$step_col
package_col <- opts$package_col
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
    d
  })

  output$sankey <- networkD3::renderSankeyNetwork({
    currentsee::plot_step_sankey(
      filtered(),
      step_col = step_col,
      package_col = package_col,
      group_cols = group_cols
    )
  })
}

shinyApp(ui, server)

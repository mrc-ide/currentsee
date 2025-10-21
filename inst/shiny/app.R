opts <- getOption("currentsee.app")
df <- opts$df
group_cols <- opts$group_cols

library(shiny)
library(bslib)
library(shinythemes)
library(dplyr)
library(tidyr)
library(networkD3)

ui <- navbarPage(
  title = HTML(
    '<div style="display:flex; align-items:center;">
       <img src="M3CPI_transparent.png" style="height:35px; margin-right:10px;">
       <div>M3CPI<br><small>modelling to inform malaria intervention prioritisation</small></div>
     </div>'
  ),
  theme = shinytheme("sandstone"),

  # Page 1: Introduction -------------------------------------------------------
  tabPanel(
    "Introduction",
    fluidPage(
      bslib::card(
        bslib::card_header("Welcome"),
        p("This is a placeholder introduction. Briefly explain the purpose of the app,
          data sources, and what users can do here."),
        p("Add any context, scope, and caveats you want users to know before exploring.")
      )
    )
  ),
  # ----------------------------------------------------------------------------

  # Page 2: How to interpret output --------------------------------------------
  tabPanel(
    "How to interpret output",
    fluidPage(
      bslib::card(
        bslib::card_header("Reading the Sankey outputs"),
        p("Placeholder guidance on interpreting the visualisations and metrics."),
        tags$ul(
          tags$li("What each node and link represents."),
          tags$li("Direction of flow and sign conventions."),
          tags$li("Any thresholds, filters, or assumptions to keep in mind.")
        ),
        p("You can expand this section later with examples and screenshots.")
      )
    )
  ),
  # ----------------------------------------------------------------------------

  # Page 3: Output -------------------------------------------------------------
  tabPanel(
    "Explore",
    fluidPage(
      titlePanel("CE pathways"),
      sidebarLayout(
        sidebarPanel(
          # Optional CSV upload ------------------------------------------------
          fileInput(
            "csv_upload",
            "Upload CSV to replace default inputs",
            accept = c(".csv", "text/csv", "text/comma-separated-values")
          ),
          tags$hr(),
          # --------------------------------------------------------------------

          # Grouping column secection ------------------------------------------
          selectInput(
            "group_cols",
            "Select grouping columns:",
            choices = names(df),     # initial choices from default df
            selected = NULL,
            multiple = TRUE
          ),
          tags$hr(),
          # --------------------------------------------------------------------
          uiOutput("dynamic_filters"),
          width = 3
        ),
        mainPanel(
          bslib::card(
            height = "620px",
            bslib::navset_card_pill(
              # Sankey display for increasing spend ----------------------------
              bslib::nav_panel(
                "Decreasing spend",
                networkD3::sankeyNetworkOutput(
                  "sankey_down",
                  height = "500px",
                  width = "100%"
                ),
                h4("") # Empty line gets rid of unnecessary vertical scroll bar
              ),
              # ----------------------------------------------------------------

              # Sankey display for decreasing spend ----------------------------
              bslib::nav_panel(
                "Increasing spend",
                networkD3::sankeyNetworkOutput(
                  "sankey_up",
                  height = "500px",
                  width = "100%"
                ),
                h4("") # Empty line gets rid of unnecessary vertical scroll bar
              )
              # ----------------------------------------------------------------
            )
          )
        )
      )
    )
  )
  # ----------------------------------------------------------------------------
)


server <- function(input, output, session) {
  df_current <- reactiveVal(df)

  # User uploads a new csv input file ------------------------------------------
  observeEvent(input$csv_upload, {
    req(input$csv_upload)
    df_current(read.csv(input$csv_upload$datapath, check.names = FALSE))
  })
  # ----------------------------------------------------------------------------

  # If input df changes, update grouping column selection options --------------
  observeEvent(df_current(), {
    d <- df_current()
    cols <- names(d)
    default_select <- if ("current" %in% cols) "current" else NULL
    updateSelectInput(
      session,
      "group_cols",
      choices  = names(d),
      selected = default_select
    )
  }, ignoreInit = FALSE)
  # ----------------------------------------------------------------------------

  # Render selection drop downs for selected grouping cols ---------------------
  output$dynamic_filters <- renderUI({
    d <- df_current()
    gcs <- if (is.null(input$group_cols)) character() else input$group_cols
    lapply(gcs, function(gc) {
      vals <- if (gc %in% names(d)) d[[gc]] else character()
      choices <- c("All", sort(unique(as.character(vals))))
      selectInput(gc, gc, choices = choices, selected = "All", selectize = TRUE)
    })
  })
  #-----------------------------------------------------------------------------

  # When df and choices are made filter input df -------------------------------
  filtered <- reactive({
    d <- df_current()
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
  # ----------------------------------------------------------------------------

  # Plot sankeys ---------------------------------------------------------------
  output$sankey_up <- networkD3::renderSankeyNetwork({
    validate(
      need(!is.null(input$current) && nzchar(input$current) && input$current != "All",
           "Choose a value for ‘current’ to show the Sankey.")
    )
    currentsee::make_sankey(
      filtered()$nodes_up,
      filtered()$links_up,
      nodeWidth = 60,
      fontSize = 15
    )
  })
  output$sankey_down <- networkD3::renderSankeyNetwork({
    validate(
      need(!is.null(input$current) && nzchar(input$current) && input$current != "All",
           "Choose a value for ‘current’ to show the Sankey.")
    )
    currentsee::make_sankey(
      filtered()$nodes_down,
      filtered()$links_down,
      nodeWidth = 60,
      fontSize = 15
    )
  })
  # ----------------------------------------------------------------------------

  # Download Sankeys
  output$dl_up_png <- downloadHandler(
    filename = function() paste0("sankey_up_", Sys.Date(), ".png"),
    content = function(file){
      w <- currentsee::make_sankey(
        filtered()$nodes_up, filtered()$links_up,
        nodeWidth = 60, fontSize = 15
      )
      html_file <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(w, file = html_file, selfcontained = TRUE)
      webshot2::webshot(html_file, file = file, vwidth = 1200, vheight = 700)
    }
  )
}

shinyApp(ui, server)

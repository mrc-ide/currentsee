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
  ),
  # ----------------------------------------------------------------------------

  # Page 4: Modelling team -----------------------------------------------------
  tabPanel(
    "Modelling team",
    fluidPage(
      bslib::card(
        class = "mb-3",
        bslib::card_body(
          h3("Meet the team"),
          p(class = "text-muted", "Click to email, or view profiles.")
        )
      ),
      br(),
      br(),
      bslib::layout_column_wrap(
        width = 300, heights_equal = "all",
        contact_card(
          name = "Emilie Pothin",
          role = "Senior Modeller",
          org  = "SwissTPH",
          email = "emilie.pothin@swisstph.ch"
        ),
        contact_card(
          name = "Peter Winskill",
          role = "Senior Modeller",
          org  = "Imperial College London",
          email = "p.winskill@imperial.ac.uk"
        )
      )
    )
  ),
  # ----------------------------------------------------------------------------
)


server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # 1. Store the working data frame that the rest of the app will use.
  #    - Starts as the default df from global.
  #    - If user uploads a CSV, we replace it (after checking it looks valid).
  # ---------------------------------------------------------------------------
  df_current <- reactiveVal(df)


  # ---------------------------------------------------------------------------
  # 2. Handle CSV upload.
  #    - Read uploaded CSV.
  #    - Check for required columns before accepting it.
  #    - If something critical is missing, show a modal and DO NOT update df_current().
  # ---------------------------------------------------------------------------
  observeEvent(input$csv_upload, {
    req(input$csv_upload)

    d_new <- tryCatch(
      read.csv(input$csv_upload$datapath, check.names = FALSE),
      error = function(e) NULL
    )

    # Basic sanity checks
    required_cols <- c("step", "current")  # add more if you need them
    missing_cols <- setdiff(required_cols, names(d_new))

    if (is.null(d_new) || length(missing_cols) > 0) {

      showModal(modalDialog(
        title = "Upload error",
        paste0(
          "The uploaded file is missing required columns: ",
          paste(missing_cols, collapse = ", "),
          ". Please upload a file with at least these columns."
        ),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))

      return(NULL)
    }

    # Passed checks -> adopt this data
    df_current(d_new)
  })


  # ---------------------------------------------------------------------------
  # 3. Keep the 'group_cols' selector in sync with the current data.
  #    - Whenever df_current() changes (eg after upload), update choices.
  #    - If column 'current' exists, pre-select it.
  # ---------------------------------------------------------------------------
  observeEvent(df_current(), {
    d <- df_current()
    cols <- names(d)

    default_select <- if ("current" %in% cols) "current" else NULL

    updateSelectInput(
      session,
      "group_cols",
      choices  = cols,
      selected = default_select
    )
  }, ignoreInit = FALSE)


  # ---------------------------------------------------------------------------
  # 4. Enforce that 'current' stays selected in group_cols (if present in data).
  #    - Rationale: downstream plotting/validate expects input$current to exist.
  #    - If user deselects it, we sneak it back in.
  # ---------------------------------------------------------------------------
  observeEvent(input$group_cols, {
    d <- df_current()

    if ("current" %in% names(d)) {
      if (is.null(input$group_cols) || !"current" %in% input$group_cols) {
        updateSelectInput(
          session,
          "group_cols",
          selected = c("current", input$group_cols)
        )
      }
    }
  }, ignoreInit = TRUE)


  # ---------------------------------------------------------------------------
  # 5. Render the dynamic filter dropdowns in the sidebar.
  #    - For each selected grouping column (group_cols),
  #      create a selectInput with choices from that column in df_current().
  #    - Include "All" which means "don't filter on this column".
  #    - This UI will re-render whenever df_current() OR input$group_cols changes.
  # ---------------------------------------------------------------------------
  output$dynamic_filters <- renderUI({
    d <- df_current()
    gcs <- if (is.null(input$group_cols)) character() else input$group_cols

    lapply(gcs, function(gc) {
      vals <- if (gc %in% names(d)) d[[gc]] else character()
      choices <- c("All", sort(unique(as.character(vals))))
      selectInput(
        inputId   = gc,
        label     = gc,          # could map to a nicer label later
        choices   = choices,
        selected  = "All",
        selectize = TRUE
      )
    })
  })


  # ---------------------------------------------------------------------------
  # 6. Capture the *current values* chosen in each of the dynamic dropdowns.
  #    - Returns a named list like list(region = "West", year="2024", current="LLINs")
  #    - Crucial trick: because we explicitly touch input[[gc]] here,
  #      this reactive will update whenever ANY dropdown value changes.
  # ---------------------------------------------------------------------------
  filter_values <- reactive({
    gcs <- input$group_cols
    if (is.null(gcs) || length(gcs) == 0) {
      return(list())
    }

    vals <- lapply(gcs, function(gc) input[[gc]])
    names(vals) <- gcs
    vals
  })


  # ---------------------------------------------------------------------------
  # 7. Build the filtered dataset and all Sankey inputs.
  #    - Depends on df_current(), input$group_cols, and filter_values().
  #    - Applies each filter unless the chosen value is "All".
  #    - Splits data into "up" (step >= 0) and "down" (step <= 0).
  #    - Calls your helper functions to build nodes/links for each.
  #
  #    Returns a list with:
  #      $nodes_up, $links_up, $nodes_down, $links_down
  #
  #    Anything downstream (plots, downloads, etc) just calls filtered()$whatever.
  # ---------------------------------------------------------------------------
  filtered <- reactive({
    d <- df_current()

    gcs  <- input$group_cols
    vals <- filter_values()

    # Apply filters for each chosen grouping column,
    # skipping columns set to "All".
    if (!is.null(gcs) && length(gcs) > 0) {
      for (gc in gcs) {
        this_val <- vals[[gc]]
        if (!is.null(this_val) && this_val != "All") {
          d <- d[d[[gc]] == this_val, , drop = FALSE]
        }
      }
    }

    # Defensive check: make sure "step" exists before subsetting.
    # If it's missing, return empty structures so we don't hard-crash.
    if (!("step" %in% names(d))) {
      return(list(
        nodes_down = data.frame(),
        links_down = data.frame(),
        nodes_up   = data.frame(),
        links_up   = data.frame()
      ))
    }

    # Split into "increasing spend / impact" vs "decreasing"
    d_up   <- d[d$step >= 0, , drop = FALSE]
    d_down <- d[d$step <= 0, , drop = FALSE]

    # Build node/link objects for networkD3
    nodes_up   <- make_nodes(d_up)
    links_up   <- make_links(d_up,   nodes_up)

    nodes_down <- make_nodes(d_down)
    links_down <- make_links(d_down, nodes_down, down = TRUE)

    list(
      nodes_down = nodes_down,
      links_down = links_down,
      nodes_up   = nodes_up,
      links_up   = links_up
    )
  })


  # ---------------------------------------------------------------------------
  # 8. Render the "Increasing spend" Sankey.
  #    - We require that input$current exists AND is not "All".
  #      (This ties back to steps 3 and 4 where we keep 'current' around.)
  #    - If not satisfied, we show a friendly message instead of erroring.
  # ---------------------------------------------------------------------------
  output$sankey_up <- networkD3::renderSankeyNetwork({
    validate(
      need(
        !is.null(input$current) &&
          nzchar(input$current) &&
          input$current != "All",
        "Choose a value for ‘current’ in the left panel to show the Sankey."
      )
    )

    # 2. Get filtered Sankey inputs
    f <- filtered()

    # 3. Check that there is actually something to plot for the "up" view
    validate(
      need(
        nrow(f$links_up) > 0 && nrow(f$nodes_up) > 0,
        "No matching pathways for this combination of filters."
      )
    )

    # 4. Draw it
    currentsee::make_sankey(
      f$nodes_up,
      f$links_up,
      nodeWidth = 60,
      fontSize  = 15
    )
  })


  # ---------------------------------------------------------------------------
  # 9. Render the "Decreasing spend" Sankey.
  #    - Same validation logic as above.
  # ---------------------------------------------------------------------------
  output$sankey_down <- networkD3::renderSankeyNetwork({
    validate(
      need(
        !is.null(input$current) &&
          nzchar(input$current) &&
          input$current != "All",
        "Choose a value for ‘current’ in the left panel to show the Sankey."
      )
    )

    # 2. Get filtered Sankey inputs
    f <- filtered()

    # 3. Check that there is actually something to plot for the "down" view
    validate(
      need(
        nrow(f$links_down) > 0 && nrow(f$nodes_down) > 0,
        "No matching pathways for this combination of filters."
      )
    )

    # 4. Draw it
    currentsee::make_sankey(
      f$nodes_down,
      f$links_down,
      nodeWidth = 60,
      fontSize  = 15
    )
  })

}

shinyApp(ui, server)

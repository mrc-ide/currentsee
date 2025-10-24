opts <- getOption("currentsee.app")
df <- opts$df

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
          # Optional CSV upload ----------------------------------------------
          fileInput(
            "csv_upload",
            "Upload CSV to replace default inputs",
            accept = c(".csv", "text/csv", "text/comma-separated-values")
          ),
          tags$hr(),

          # Dynamic filter inputs --------------------------------------------
          uiOutput("dynamic_filters"),

          width = 3
        ),
        mainPanel(
          bslib::card(
            height = "620px",
            bslib::navset_card_pill(
              bslib::nav_panel(
                "Decreasing spend",
                networkD3::sankeyNetworkOutput(
                  "sankey_down",
                  height = "500px",
                  width = "100%"
                ),
                h4("") # spacer to avoid scrollbars
              ),
              bslib::nav_panel(
                "Increasing spend",
                networkD3::sankeyNetworkOutput(
                  "sankey_up",
                  height = "500px",
                  width = "100%"
                ),
                h4("") # spacer to avoid scrollbars
              )
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
          h3("Meet the team")
        )
      ),
      br(),
      br(),
      bslib::layout_column_wrap(
        width = 300, heights_equal = "all",
        contact_card(
          name = "Emilie Pothin",
          role = "Project lead",
          org  = "SwissTPH",
          email = "emilie.pothin@swisstph.ch"
        ),
        contact_card(
          name = "Peter Winskill",
          role = "Project lead",
          org  = "Imperial College London",
          email = "p.winskill@imperial.ac.uk"
        ),
        contact_card(
          name = "Monica Golumbeanu",
          role = "Senior Modeller",
          org  = "SwissTPH"
        ),
        contact_card(
          name = "Tom Brewer",
          role = "Senior Modeller",
          org  = "Imperial College London"
        ),
        contact_card(
          name = "Leandro Gandos Brito",
          role = "Methodology and implementation",
          org  = "SwissTPH"
        ),
        contact_card(
          name = "Dariya Nikitin",
          role = "Methodology and implementation",
          org  = "Imperial College London"
        ),
        contact_card(
          name = "Daniela Olivera Mesa",
          role = "App development",
          org  = "Imperial College London"
        )
      )
    )
  )
  # ----------------------------------------------------------------------------
)


server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # 0. Columns we consider "core" to Sankey construction.
  #    Everything NOT in this list will become a filter dropdown automatically.
  #    Adjust this list to match what make_nodes / make_links require.
  # ---------------------------------------------------------------------------
  core_cols <- c("id", "step", "package", "next_package", "cost", "impact")

  # ---------------------------------------------------------------------------
  # 1. Working dataset.
  #    Starts as df from opts, replaced on upload after validation.
  # ---------------------------------------------------------------------------
  df_current <- reactiveVal(df)

  # ---------------------------------------------------------------------------
  # 2. Handle CSV upload with sanity checks.
  # ---------------------------------------------------------------------------
  observeEvent(input$csv_upload, {
    req(input$csv_upload)

    d_new <- tryCatch(
      read.csv(input$csv_upload$datapath, check.names = FALSE),
      error = function(e) NULL
    )

    required_cols <- c("step", "current")  # minimal cols to proceed
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

    df_current(d_new)
  })


  # ---------------------------------------------------------------------------
  # 3. filter_vars(): which columns should get dropdowns?
  #    It's all non-core columns in the *current* data, excluding anything
  #    that looks continuous/too granular if you want to be picky.
  #    For now we include everything that's not in core_cols.
  # ---------------------------------------------------------------------------
  filter_vars <- reactive({
    d <- df_current()
    setdiff(names(d), core_cols)
  })


  # ---------------------------------------------------------------------------
  # 4. current_subset(): apply all active filter selections to df_current().
  #    For each filter var v:
  #       - if input[[v]] is NULL or "All", we don't filter on v
  #       - otherwise we keep only rows matching that value
  # ---------------------------------------------------------------------------
  current_subset <- reactive({
    d <- df_current()
    fvars <- filter_vars()

    if (length(fvars) == 0) {
      return(d)
    }

    for (v in fvars) {
      sel <- input[[v]]
      if (!is.null(sel) && sel != "All") {
        d <- d[d[[v]] == sel, , drop = FALSE]
      }
    }

    d
  })


  # ---------------------------------------------------------------------------
  # 5. Render the dynamic filter dropdowns initially.
  #    We create one selectInput per filter var.
  #    Each starts at "All".
  #    (We'll keep them in sync in the observer below.)
  # ---------------------------------------------------------------------------
  output$dynamic_filters <- renderUI({
    d <- df_current()
    fvars <- filter_vars()

    lapply(fvars, function(v) {
      vals <- sort(unique(as.character(d[[v]])))
      selectInput(
        inputId   = v,
        label     = v,
        choices   = c("All", vals),
        selected  = "All",
        selectize = TRUE
      )
    })
  })


  # ---------------------------------------------------------------------------
  # 6. Keep dropdowns mutually consistent.
  #
  #    Idea:
  #    - For each filter var v, we update its choices based on all the OTHER
  #      filters' current selections.
  #
  #    Why not just use current_subset() for all of them?
  #    Because that would lock v to its existing selection immediately.
  #    Instead, for each v we:
  #       1. start from full df_current()
  #       2. apply all filters EXCEPT v
  #       3. whatever unique values remain in column v become its allowed choices
  #
  #    We also try to keep your current selection if it's still valid.
  # ---------------------------------------------------------------------------
  observe({
    d_full <- df_current()
    fvars  <- filter_vars()

    # Nothing to do if we don't have any filter vars yet (e.g. before upload).
    if (length(fvars) == 0) {
      return(NULL)
    }

    for (v in fvars) {

      # Build "partial subset" applying all filters except v
      d_partial <- d_full
      for (u in fvars) {
        if (u == v) next
        sel_u <- input[[u]]
        if (!is.null(sel_u) && sel_u != "All") {
          d_partial <- d_partial[d_partial[[u]] == sel_u, , drop = FALSE]
        }
      }

      # Valid choices for v under other filters
      poss_vals <- sort(unique(as.character(d_partial[[v]])))
      poss_choices <- c("All", poss_vals)

      # Keep current selection if it's still valid, else "All"
      current_sel <- isolate(input[[v]])
      if (is.null(current_sel) || !(current_sel %in% poss_choices)) {
        current_sel <- "All"
      }

      updateSelectInput(
        session,
        inputId  = v,
        choices  = poss_choices,
        selected = current_sel
      )
    }
  })


  # ---------------------------------------------------------------------------
  # 7. Helper: given a subsetted df, build the Sankey node/link inputs.
  #    - Split into up/down by step sign
  #    - Call your helper fns
  #    Returns a list(nodes_up, links_up, nodes_down, links_down)
  # ---------------------------------------------------------------------------
  sankey_inputs <- reactive({
    d <- current_subset()

    # If "step" is missing entirely, bail gracefully
    if (!("step" %in% names(d))) {
      return(list(
        nodes_down = data.frame(),
        links_down = data.frame(),
        nodes_up   = data.frame(),
        links_up   = data.frame()
      ))
    }

    d_up   <- d[d$step >= 0, , drop = FALSE]
    d_down <- d[d$step <= 0, , drop = FALSE]

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
  # 8. Render "Increasing spend" Sankey.
  #
  #    We still require that "current" is pinned down sensibly,
  #    because the story you're telling needs a specific 'current'
  #    (or at least not "All").
  #
  #    current is in core_cols, not guaranteed to be in filter_vars,
  #    so we read it out of the *subset*.
  #
  #    We'll accept this if there's exactly one non-NA current in the subset
  #    and it's not "All".
  # ---------------------------------------------------------------------------
  output$sankey_up <- networkD3::renderSankeyNetwork({
    d_use <- current_subset()

    # Check current is defined sensibly in the subset
    current_vals <- unique(as.character(d_use$current))
    current_vals <- current_vals[!is.na(current_vals)]
    validate(
      need(
        length(current_vals) == 1 && current_vals != "All",
        "Choose a value for ‘current’ in the left panel to show the Sankey."
      ),
      need(
        nrow(d_use) > 0,
        "No matching pathways for this combination of filters."
      )
    )

    f <- sankey_inputs()
    validate(
      need(
        nrow(f$links_up) > 0 && nrow(f$nodes_up) > 0,
        "No matching pathways for this combination of filters."
      )
    )

    currentsee::make_sankey(
      f$nodes_up,
      f$links_up,
      nodeWidth = 60,
      fontSize  = 15
    )
  })


  # ---------------------------------------------------------------------------
  # 9. Render "Decreasing spend" Sankey (mirror logic).
  # ---------------------------------------------------------------------------
  output$sankey_down <- networkD3::renderSankeyNetwork({
    d_use <- current_subset()

    current_vals <- unique(as.character(d_use$current))
    current_vals <- current_vals[!is.na(current_vals)]
    validate(
      need(
        length(current_vals) == 1 && current_vals != "All",
        "Choose a value for ‘current’ in the left panel to show the Sankey."
      ),
      need(
        nrow(d_use) > 0,
        "No matching pathways for this combination of filters."
      )
    )

    f <- sankey_inputs()
    validate(
      need(
        nrow(f$links_down) > 0 && nrow(f$nodes_down) > 0,
        "No matching pathways for this combination of filters."
      )
    )

    currentsee::make_sankey(
      f$nodes_down,
      f$links_down,
      nodeWidth = 60,
      fontSize  = 15
    )
  })

}

shinyApp(ui, server)

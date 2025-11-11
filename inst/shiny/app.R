opts <- getOption("currentsee.app")
df <- opts$df

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)

ui <-
  fluidPage(
    # Include the external CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    navbarPage(
      title = div(
        class = "app-navbar-brand",
        img(src = "M3CPI_transparent.png", alt = "M3CPI Logo"),
        div(
          class = "app-brand-text",
          div("M3CPI", class = "app-brand-title"),
          div("modelling to inform malaria intervention prioritisation",
              class = "app-brand-subtitle")
        )
      ),
      theme = shinythemes::shinytheme("readable"),

      # Page 1: Introduction -------------------------------------------------------
      tabPanel(
        "Introduction",
        fluidPage(
          h2("Welcome to the M3CPI App"),

          p("This application provides guidance on malaria intervention strategies across representative epidemiological
                settings in sub-Saharan Africa. The recommended intervention packages correspond to the most cost-effective
                configurations for scaling interventions up or down in response to changing budgets. Optimal strategies explicitly
                account for uncertainty arising from the simulation process, model parameters, and cost assumptions.."),

          h3("Key aspects to keep in mind:"),

          # --- List of key messages ---
          tags$ul(
            tags$li(
              tags$b("Simulations are built on established models:"),
              tags$br(),
              "Results are based on two well-established malaria transmission models calibrated to historical data and representative of sub-Saharan African settings. ",
              "Both models were implemented in parallel, and their outputs were compared and combined to assess the robustness of the results.",
              tags$br(),
              "For further details on the modelling framework and methods, please see the Methods tab."),

            tags$br(),
            tags$li(
              tags$b("Epidemiological results are integrated with cost-effectiveness analysis:"),
              tags$br(),
              "Epidemiological outputs were combined with intervention and case-management costs, along with their associated uncertainties. ",
              "Cost-effectiveness was evaluated under varying budget constraints to identify the most efficient allocation of resources."
            ),

            tags$br(),
            tags$li(
              tags$b("Results shown here are applicable to archetypal settings:"),
              tags$br(),
              "The analyses provide a generic overview of how malaria interventions can be scaled up or down in settings with similar transmission intensity, ",
              "seasonality, and historical intervention coverage."
            )
          ),


          # --- Warning box (outside of the list, no bullet) ---
          tags$div(
            style = "background-color:#fff3cd; border-left:4px solid #ffcc00; padding:12px; margin:20px 0; border-radius:6px;",
            tags$b("⚠️ Important: "),
            "This tool provides general guidance. Outputs should not be interpreted as country-specific recommendations. ",
            "For detailed local decision-making, consult context-specific modelling studies."
          ),


          tags$ul(
            tags$li(
              tags$b("Results highlight uncertainty across simulations:"),
              tags$br(),
              "Multiple simulations were conducted for each scenario to capture uncertainty arising from model structure, parameter variation, ",
              "and stochastic processes. Results represent a range of possible strategies, highlighting both commonly optimal intervention packages ",
              "and alternative, less frequent outcomes."
            )
          )
        )
      ),
      # ----------------------------------------------------------------------------

      # Page 2: Methods -------------------------------------------------------

      tabPanel(
        "Methods",
        fluidPage(

          h2("Methods and modelling approaches"),

          # --- SECTION I ---
          tags$h3("I. Modelling Framework"),

          tags$p(
            "The analyses presented here rely on aggregated outputs from two malaria transmission models: ",
            tags$a(tags$b("malariasimulation"), href = "https://mrc-ide.github.io/malariasimulation/", target = "_blank"),
            " and ",
            tags$a(tags$b("OpenMalaria. "), href = "https://github.com/SwissTPH/openmalaria", target = "_blank"),
            "Each model explored future scenarios to assess the impact of a suite of malaria intervention packages ",
            "at the subnational level (admin 1 regions) across sub-Saharan Africa."
          ),

          tags$p(
            "Both models were calibrated and customised for individual admin 1 regions, incorporating context-specific factors ",
            "that influence historical infection patterns, intervention efficacy, and observed malaria prevalence."
          ),

          tags$div(
            style = "background-color:#f9f9f9; border-left:4px solid #0072B2; padding:10px; margin:20px 0; border-radius:6px;",
            tags$img(src = "modelling approach.png",
                     width = "100%",
                     style = "max-width:600px; border-radius:8px; margin:10px 0;"),
            tags$br(),
            tags$b("Figure 1. Schematic representation of the modelling approach at the admin 1 level: "),
            "Models are calibrated using global estimates from the Malaria Atlas Project to accurately represent each admin 1 region ",
            "and reproduce historical malaria burden trends. Calibrated models are then used to conduct scenario analyses ",
            "and assess cost-effectiveness at the admin 1 level."
          ),

          tags$br(),

          # --- SECTION II ---
          tags$h3("II. Interventions Assessed"),

          tags$p(
            "The following interventions were evaluated in all possible combinations, starting from the strategy currently implemented ",
            "in each administrative unit (the ",
            tags$b("Business as Usual (BAU)"),
            "), with its corresponding coverage levels. Scenarios were generated by adding new interventions or increasing ",
            "the coverage of existing ones:"
          ),

          tags$ul(
            tags$li("Insecticide-treated bed nets (ITNs)"),
            tags$li("Chemoprevention: Seasonal (SMC) or perennial (PMC)"),
            tags$li("Indoor residual spraying (IRS)"),
            tags$li("Vaccine"),
            tags$li("Case management(CM)")
          ),

          tags$br(),

          # --- SECTION III ---
          tags$h3("III. Health Impact and Cost-Effectiveness"),

          tags$p(
            "To estimate the impact of each intervention package, results were compared against the BAU scenario."
          ),

          tags$b("Health outcomes evaluated:"),
          tags$ul(
            tags$li("Incidence – number of new malaria cases over time"),
            tags$li("Cases averted – reduction in malaria cases relative to the BAU"),
            tags$li("Deaths averted – reduction in malaria-related mortality relative to the BAU"),
            tags$li("DALYs averted – disability-adjusted life years gained through reduced morbidity and mortality")
          ),

          tags$p(
            "Health outcomes were estimated for three age groups (0–5 years, 5–15 years, and 15 years and above) ",
            "and across both short- and long-term time horizons."
          ),

          tags$p(
            "Costs for each intervention were derived from WHO estimates for intervention and case-management costs. ",
            "The most cost-effective intervention package at each budget level was identified using a frontier approach, ",
            "providing guidance on how to cost-effectively scale interventions up or down from current coverage levels."
          ),

          tags$br(),

          # --- SECTION IV ---
          tags$h3("IV. Setting Archetypes and Model Harmonisation"),

          tags$p(
            "Results from both models were harmonised and grouped into representative archetypes. ",
            "Each archetype captures common patterns in how optimal intervention strategies change with budget levels ",
            "across similar epidemiological settings."
          ),

          tags$p(
            "Simulations were run across a range of realistic scenarios, and settings showing similar decision-making trends ",
            "were aggregated. This allows general insights to be drawn — for example, identifying which intervention combinations ",
            "are consistently prioritised in settings with comparable transmission intensity, seasonality, and historical coverage."
          ),

          tags$div(
            style = "background-color:#fff3cd; border-left:4px solid #ffcc00; padding:12px; margin:25px 0; border-radius:6px;",
            tags$b("⚠️ Limitations"),
            tags$ul(
              tags$li("Aggregation of results smooths over local heterogeneities."),
              tags$li("Outputs are indicative and meant to inform, not prescribe, interventions.")
            )
          )
        )

      ),

      # ----------------------------------------------------------------------------

      # Page 3: How to interpret output --------------------------------------------
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
          h3("Cost-effective intervention pathways"),
          br(),
          br(),
          sidebarLayout(
            sidebarPanel(
              # Dynamic filter inputs --------------------------------------------
              uiOutput("dynamic_filters"),
              br(),
              br(),

              # Optional CSV upload ----------------------------------------------
              fileInput(
                "csv_upload",
                "Upload CSV to replace default inputs",
                accept = c(".csv", "text/csv", "text/comma-separated-values")
              ),
              width = 3
            ),
            mainPanel(
              bslib::card(
                height = "620px",
                bslib::navset_card_pill(
                  bslib::nav_panel(
                    "Removing interventions",
                    br(),
                    br(),
                    uiOutput("sankey_down_container"),
                    h4("") # spacer to avoid scrollbars
                  ),
                  bslib::nav_panel(
                    "Adding interventions",
                    br(),
                    br(),
                    uiOutput("sankey_up_container"),
                    h4("") # spacer to avoid scrollbars
                  )
                )
              )
            )
          )
        )
      ),
      # ----------------------------------------------------------------------------

      # Page 4: FAQs ---------------------------------------------------------------
      tabPanel(
        "FAQs",
        fluidPage(
          bslib::card(
            # Each question and answer pair --------------------------
            tags$div(
              tags$span(
                shiny::icon("lightbulb"),
                tags$b(" What does the Sankey diagram show?")
              ),
              br(),
              p("Each node represents an intervention or combination of interventions.
           The links show how resources or impact shift between scenarios.
           Thicker links indicate larger changes."),
              br()
            ),

            tags$div(
              tags$span(
                shiny::icon("lightbulb"),
                tags$b(" Why can’t I see a Sankey when I open the app?")
              ),
              p("You need to select a baseline 'current' value in the filters on the
           left-hand side. Once you do, the Sankey will appear."),
              br()
            ),

            tags$div(
              tags$span(
                shiny::icon("lightbulb"),
                tags$b("  What does ‘No matching pathways’ mean?")
              ),
              p("This appears when your chosen filters return no data—try broadening
           your selections or resetting one of the filters to 'All'."),
              br()
            )
            # ---------------------------------------------------------
          )
        )
      ),

      # Page 5: Modelling team -----------------------------------------------------
      tabPanel(
        "Modelling team",
        fluidPage(
          h3("Meet the team"),
          br(),
          br(),
          bslib::layout_column_wrap(
            width = 300, heights_equal = "all",
            contact_card(
              name = "Emilie Pothin",
              role = "Project lead",
              org  = "SwissTPH",
              email = "emilie.pothin@swisstph.ch",
              photo = "Emilie_Pothin.png"
            ),
            contact_card(
              name = "Peter Winskill",
              role = "Project lead",
              org  = "Imperial College London",
              email = "p.winskill@imperial.ac.uk",
              photo = "Pete_Winskill.jpg"
            ),
            contact_card(
              name = "Monica Golumbeanu",
              role = "Senior Modeller",
              org  = "SwissTPH",
              photo = "Monica_Golumbeanu.png"
            ),
            contact_card(
              name = "Tom Brewer",
              role = "Senior Modeller",
              org  = "Imperial College London",
              photo = "Tom_Brewer.jpg"
            ),
            contact_card(
              name = "Leandro Gandos Brito",
              role = "Methodology and implementation",
              org  = "SwissTPH",
              photo = "Leandro_Gandos_Brito.jpg"
            ),
            contact_card(
              name = "Dariya Nikitin",
              role = "Methodology and implementation",
              org  = "Imperial College London",
              photo = "Dariya_Nikitin.jpg"
            ),
            contact_card(
              name = "Daniela Olivera Mesa",
              role = "App development",
              org  = "Imperial College London",
              photo = "Daniela_Olivera_Mesa.jpg"
            )
          )
        )
      )
      # ----------------------------------------------------------------------------
    )
  )


server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # 0. Columns we consider "core" to Sankey construction.
  #    Everything NOT in this list will become a filter dropdown automatically.
  #    Adjust this list to match what make_nodes / make_links require.
  # ---------------------------------------------------------------------------
  core_cols <- c("-4", "-3", "-2", "-1", "0", "1", "2", "3", "4")

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
    c("current", setdiff(names(d), c("current", core_cols)))
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

    up   <- nodes_up(d)
    down <- nodes_down(d)

    list(
      down = down,
      up   = up
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
  output$sankey_up <- renderPlot({
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
        nrow(f$up$up) > 0,
        "No matching pathways for this combination of filters."
      )
    )

    make_sankey(
      f$up$up,
      f$up$up_nodes,
      node_width = 0.3,
      flow_label_font_size = 4,
      node_label_font_size = 5
    )
  })

  output$sankey_up_container <- renderUI({
    f <- sankey_inputs()
    n_cols <- ncol(f$up$up)
    plot_width <- n_cols * 220

    plotOutput(
      "sankey_up",
      height = "500px",
      width = paste0(plot_width, "px")
    )
  })


  # ---------------------------------------------------------------------------
  # 9. Render "Decreasing spend" Sankey (mirror logic).
  # ---------------------------------------------------------------------------
  output$sankey_down <- renderPlot({
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
        nrow(f$down$down) > 0,
        "No matching pathways for this combination of filters."
      )
    )

    currentsee::make_sankey(
      f$down$down,
      f$down$down_nodes,
      node_width = 0.25,
      flow_label_font_size = 4,
      node_label_font_size = 5
    )
  })

  output$sankey_down_container <- renderUI({
    f <- sankey_inputs()
    n_cols <- ncol(f$down$down)
    plot_width <- n_cols * 200

    plotOutput(
      "sankey_down",
      height = "500px",
      width = paste0(plot_width, "px")
    )
  })

}

shinyApp(ui, server)

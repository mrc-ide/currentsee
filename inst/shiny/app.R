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
       <div>M3CPI<br><small>Modelling to inform malaria intervention prioritisation</small></div>
     </div>'
  ),
  theme = shinytheme("sandstone"),

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
      h2("About the Results"),
      p("The visualisations in this app use Sankey diagrams to illustrate optimal intervention strategies across archetypal settings"),
      div(
        style = "text-align: center;",
        tags$img(src = "sankey_example.png",
                 class = "centered-img",
                 width = "50%",
                 style = "max-width:600px; border-radius:8px; margin:10px 0;")),



      h4(tags$b("1. Intervention Packages")),
      div(
        style = "text-align: left;",
        tags$img(src = "box_sankey.png",
                 width = "25%",
                 height="25%")),
      h4(tags$b("2. Budget change:")),
      p("We move along the", tags$i("x axis"),"to analys how intervention might change as budget changes."),
      div(
        style = "text-align: left;",
        tags$img(src = "budget_change.png",
                 width = "25%",
                 height="25%")),
      h4(tags$b("3. Intervention flows")),
      p("To know what is the most likely scale up (or down) of interventions, we follow the widest boxes.
      All flows finally lead to our highest cost solution, with all interventions implemented"),

      div(
        style = "text-align: left;",
        tags$img(src = "pathway.png",
                 width = "15%",
                 height="15%")),

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
    # bslib::layout_column_wrap(
    #   width = 300, heights_equal = "all",
    #   contact_card(
    #     name = "Emilie Pothin",
    #     role = "Senior Modeller",
    #     org  = "SwissTPH",
    #     email = "emilie.pothin@swisstph.ch"
    #   ),
    #   contact_card(
    #     name = "Peter Winskill",
    #     role = "Senior Modeller",
    #     org  = "Imperial College London",
    #     email = "p.winskill@imperial.ac.uk"
    #   )
    # )
  )
),
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

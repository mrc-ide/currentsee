#' App introduction tab
#'
#' @return NULL
#' @export
tab_introduction <- function(){
  shiny::tabPanel(
    "Introduction",
    shiny::fluidPage(
      shiny::h2("Welcome to the M3CPI App"),

      shiny::p("This application provides guidance on malaria intervention strategies across representative epidemiological
                settings in sub-Saharan Africa. The recommended intervention packages correspond to the most cost-effective
                configurations for scaling interventions up or down in response to changing budgets. Optimal strategies explicitly
                account for uncertainty arising from the simulation process, model parameters, and cost assumptions."),

      shiny::h3("Key aspects to keep in mind:"),

      # --- List of key messages ---
      shiny::tags$ul(
        shiny::tags$li(
          shiny::tags$b("Simulations are built on established models:"),
          shiny::tags$br(),
          "Results are based on two well-established malaria transmission models calibrated to historical data and representative of sub-Saharan African settings. ",
          "Both models were implemented in parallel, and their outputs were compared and combined to assess the robustness of the results.",
          shiny::tags$br(),
          "For further details on the modelling framework and methods, please see the Methods tab."),

        shiny::tags$br(),
        shiny::tags$li(
          shiny::tags$b("Epidemiological results are integrated with cost-effectiveness analysis:"),
          shiny::tags$br(),
          "Epidemiological outputs were combined with intervention and case-management costs, along with their associated uncertainties. ",
          "Cost-effectiveness was evaluated under varying budget constraints to identify the most efficient allocation of resources."
        ),

        shiny::tags$br(),
        shiny::tags$li(
          shiny::tags$b("Results shown here are applicable to archetypal settings:"),
          shiny::tags$br(),
          "The analyses provide a generic overview of how malaria interventions can be scaled up or down in settings with similar transmission intensity, ",
          "seasonality, and historical intervention coverage."
        )
      ),


      # --- Warning box (outside of the list, no bullet) ---
      shiny::tags$div(
        style = "background-color:#fff3cd; border-left:4px solid #ffcc00; padding:12px; margin:20px 0; border-radius:6px;",
        shiny::tags$b("\u26a0 Important: "),
        "This tool provides general guidance. Outputs should not be interpreted as country-specific recommendations. ",
        "For detailed local decision-making, consult context-specific modelling studies."
      ),


      shiny::tags$ul(
        shiny::tags$li(
          shiny::tags$b("Results highlight uncertainty across simulations:"),
          shiny::tags$br(),
          "Multiple simulations were conducted for each scenario to capture uncertainty arising from model structure, parameter variation, ",
          "and stochastic processes. Results represent a range of possible strategies, highlighting both commonly optimal intervention packages ",
          "and alternative, less frequent outcomes."
        )
      )
    )
  )
}

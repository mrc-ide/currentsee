#' App introduction tab
#'
#' @return A tabPanel for the Introduction tab
#' @export
tab_introduction <- function() {

  warning_box_style <- paste(
    "background-color:#fff3cd;",
    "border-left:4px solid #ffcc00;",
    "padding:12px;",
    "margin:16px 0;",
    "border-radius:6px;"
  )

  shiny::tabPanel(
    title = "Introduction",
    shiny::fluidPage(

      shiny::h2("Welcome to the M3CPI app"),

      shiny::p(
        "This application provides guidance on malaria intervention strategies ",
        "across representative epidemiological settings in sub-Saharan Africa. ",
        "The recommended intervention packages correspond to the most ",
        "cost-effective configurations for scaling interventions up or down in ",
        "response to changing budgets. Optimal strategies explicitly account for ",
        "uncertainty arising from the simulation process, model parameters, and ",
        "cost assumptions."
      ),

      shiny::p(
        "M3CPI is a WHO-led project generating modelled evidence to inform ",
        "global recommendations on the optimal mix of malaria interventions ",
        "in malaria-endemic settings. The results contribute to updating WHO's ",
        "\"Guiding principles for prioritizing malaria interventions in ",
        "resource-constrained country contexts to achieve maximum impact\"."
      ),

      # --- Warning boxes ---
      shiny::tags$div(
        style = warning_box_style,
        shiny::tags$b("\u26a0 Important: "),
        "M3CPI provides general guidance only, not country-specific ",
        "recommendations.",
        shiny::tags$br(),
        "Context-specific modelling as part of the sub-national tailoring (SNT) ",
        "process fulfils this need."
      ),

      shiny::tags$div(
        style = warning_box_style,
        shiny::tags$b("\u26a0 Important: "),
        "M3CPI does not address prioritisation of resources across multiple ",
        "geographies.",
        shiny::tags$br(),
        "It identifies patterns within a given setting, not between settings."
      ),

      shiny::tags$div(
        style = warning_box_style,
        shiny::tags$b("\u26a0 Important: "),
        "M3CPI is not a deep model-harmonisation project.",
        shiny::tags$br(),
        "Differences between models remain and are informative."
      ),

      # --- Explanation of the framing ---
      shiny::h3("Why this framing matters"),

      shiny::tags$p(
        shiny::tags$b("Purpose:"),
        " To identify broad structural insights, common patterns, and ",
        "system-level dynamics that hold across settings."
      ),
      shiny::tags$p(
        shiny::tags$b("Concern:"),
        " Generic guidance may be applied literally to a specific context. ",
        "This is a reason for careful framing and communication, not for ",
        "avoiding general guidance."
      ),
      shiny::tags$p(
        shiny::tags$b("Utility:"),
        " High-level guidance provides a shared language for programmes, ",
        "donors, and modellers, highlights what tends to matter most, and ",
        "helps identify dominant patterns and clearly poor strategies. ",
        "It is a map of the decision landscape, not a shortcut to decisions."
      ),

      shiny::h3("Key aspects to keep in mind:"),

      # --- List of key messages ---
      shiny::tags$ul(

        shiny::tags$li(
          shiny::tags$b("Simulations are built on established models:"),
          shiny::tags$br(),
          "Results are based on two well-established malaria transmission ",
          "models calibrated to historical data and representative of ",
          "sub-Saharan African settings. Both models were implemented in ",
          "parallel, and their outputs were compared and combined to assess ",
          "the robustness of the results.",
          shiny::tags$br(),
          "For further details on the modelling framework and methods, please ",
          "see the Methods tab."
        ),

        shiny::tags$br(),

        shiny::tags$li(
          shiny::tags$b("Epidemiological results are integrated with cost-effectiveness analysis:"),
          shiny::tags$br(),
          "Epidemiological outputs were combined with intervention and ",
          "case-management costs, along with their associated uncertainties. ",
          "Cost-effectiveness was evaluated under varying budget constraints ",
          "to identify the most efficient allocation of resources."
        ),

        shiny::tags$br(),

        shiny::tags$li(
          shiny::tags$b("Results apply to archetypal settings:"),
          shiny::tags$br(),
          "The analyses provide a generic overview of how malaria ",
          "interventions can be scaled up or down in settings with similar ",
          "transmission intensity, seasonality, and historical intervention ",
          "coverage."
        ),

        shiny::tags$br(),

        shiny::tags$li(
          shiny::tags$b("Results highlight uncertainty across simulations:"),
          shiny::tags$br(),
          "Multiple simulations were conducted for each scenario to capture ",
          "uncertainty arising from model structure, parameter variation, and ",
          "stochastic processes. The outputs represent a range of possible ",
          "strategies, showing both commonly optimal intervention packages ",
          "and alternative, less frequent outcomes."
        )
      )
    )
  )
}

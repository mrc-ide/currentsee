tab_faqs <- function(){
  shiny::tabPanel(
    "FAQs",
    shiny::fluidPage(
      bslib::card(
        # Each question and answer pair --------------------------
        shiny::tags$div(
          shiny::tags$span(
            shiny::icon("lightbulb"),
            shiny::tags$b(" What does the Sankey diagram show?")
          ),
          shiny::br(),
          shiny::p("Each node represents an intervention or combination of interventions.
           The links show how resources or impact shift between scenarios.
           Thicker links indicate larger changes."),
          shiny::br()
        ),

        shiny::tags$div(
          shiny::tags$span(
            shiny::icon("lightbulb"),
            shiny::tags$b(" Why can’t I see a Sankey when I open the app?")
          ),
          shiny::p("You need to select a baseline 'current' value in the filters on the
           left-hand side. Once you do, the Sankey will appear."),
          shiny::br()
        ),

        shiny::tags$div(
          shiny::tags$span(
            shiny::icon("lightbulb"),
            shiny::tags$b("  What does ‘No matching pathways’ mean?")
          ),
          shiny::p("This appears when your chosen filters return no data—try broadening
           your selections or resetting one of the filters to 'All'."),
          shiny::br()
        )
        # ---------------------------------------------------------
      )
    )
  )
}

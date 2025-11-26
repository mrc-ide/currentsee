opts <- getOption("currentsee.app")
df <- opts$df

steps <- names(df)[names(df) %in% paste(-20:20)] |>
  as.numeric() |>
  sort()

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(bsicons)

# Helper Functions --------------------------------------------------------


# UI Definition -----------------------------------------------------------

ui <- fluidPage(
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

    tab_introduction(),
    tab_methods(),

    tabPanel(
      "How to interpret output",
      fluidPage(
        tags$iframe(
          src = "sankey_101.pdf",
          width = "100%",
          height = "600px",
          style = "border: none;"
        )
      )
    ),

    tabPanel(
      "Explore",
      fluidPage(
        h3("Cost-effective intervention pathways"),
        br(), br(),
        sidebarLayout(
          sidebarPanel(
            uiOutput("coverage_box"),
            uiOutput("filter_controls"),
            width = 3
          ),
          mainPanel(
            div(
              style = "margin-bottom: 20px;",
              radioButtons(
                "plot_type",
                "Select view:",
                choices = list(
                  "Full pathway" = "full",
                  "Removing interventions" = "down",
                  "Adding interventions" = "up"
                ),
                selected = "full",
                inline = TRUE
              )
            ),
            conditionalPanel(
              condition = "input.plot_type == 'full'",
              uiOutput("sankey_full_container")
            ),
            conditionalPanel(
              condition = "input.plot_type == 'down'",
              uiOutput("sankey_down_container")
            ),
            conditionalPanel(
              condition = "input.plot_type == 'up'",
              uiOutput("sankey_up_container")
            )
          )
        )
      )
    ),

    tab_faqs(),
    tab_team()
  )
)

# Server Logic ------------------------------------------------------------

server <- function(input, output, session) {

  # Reactive values
  filter_vars <- reactive(get_filter_vars(df))

  filtered_data <- reactive({
    apply_filters(df, filter_vars(), input)
  })

  # Dynamic filter controls
  output$filter_controls <- renderUI({
    fvars <- filter_vars()

    lapply(fvars, function(var) {
      values <- sort(unique(as.character(df[[var]])))
      selectInput(
        inputId = var,
        label = var,
        choices = c("All", values),
        selected = "All",
        selectize = TRUE
      )
    })
  })

  # Keep filter dropdowns mutually consistent
  observe({
    fvars <- filter_vars()

    if (length(fvars) == 0) return(NULL)

    for (var in fvars) {
      # Apply all filters except the current one
      partial_data <- df
      for (other_var in fvars) {
        if (other_var == var) next
        selection <- input[[other_var]]
        if (!is.null(selection) && selection != "All") {
          partial_data <- partial_data[partial_data[[other_var]] == selection, , drop = FALSE]
        }
      }

      # Update choices based on remaining valid values
      valid_values <- sort(unique(as.character(partial_data[[var]])))
      valid_choices <- c("All", valid_values)

      # Preserve current selection if still valid
      current_selection <- isolate(input[[var]])
      if (is.null(current_selection) || !(current_selection %in% valid_choices)) {
        current_selection <- "All"
      }

      updateSelectInput(
        session,
        inputId = var,
        choices = valid_choices,
        selected = current_selection
      )
    }
  })

  # Coverage indicator
  output$coverage_box <- renderUI({
    create_coverage_box(nrow(df), nrow(filtered_data()))
  })

  # Sankey plot renderers
  output$sankey_full <- renderPlot({
    create_sankey_plot(filtered_data(), paste(steps))
  })

  output$sankey_down <- renderPlot({
    create_sankey_plot(filtered_data(), paste(steps[steps <= 0]))
  })

  output$sankey_up <- renderPlot({
    create_sankey_plot(filtered_data(), paste(steps[steps >= 0]))
  })

  # Dynamic plot containers with appropriate widths
  output$sankey_full_container <- renderUI({
    plot_width <- calculate_plot_width(filtered_data(), -20:20, 200)
    plotOutput("sankey_full", height = "500px", width = paste0(plot_width, "px"))
  })

  output$sankey_down_container <- renderUI({
    plot_width <- calculate_plot_width(filtered_data(), 0:-20, 220)
    plotOutput("sankey_down", height = "500px", width = paste0(plot_width, "px"))
  })

  output$sankey_up_container <- renderUI({
    plot_width <- calculate_plot_width(filtered_data(), 0:20, 220)
    plotOutput("sankey_up", height = "500px", width = paste0(plot_width, "px"))
  })
}

shinyApp(ui, server)

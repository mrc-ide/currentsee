#' App team tab
#'
#' @return NULL
tab_team <- function(){
  shiny::tabPanel(
    "Modelling team",
    shiny::fluidPage(
      shiny::h3("Meet the team"),
      shiny::br(),
      shiny::br(),
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
}

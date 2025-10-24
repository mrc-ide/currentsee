#' Create a contact card for RShiny applications
#'
#' Generates a styled contact card component using bslib cards, displaying
#' personal information, photo, and contact links. The card includes responsive
#' design elements and integrates seamlessly with Bootstrap-styled Shiny apps.
#'
#' @param name Character string. The person's full name to display as the card title.
#' @param role Character string. The person's job title or role.
#' @param org Character string. The organization or company name.
#' @param email Character string, optional. Email address. If provided, creates
#'   a mailto link with an envelope icon. Default is `NULL`.
#' @param website Character string, optional. Website URL. If provided, creates
#'   an external link with a globe icon. Default is `NULL`.
#' @param photo Character string, optional. Path or URL to profile photo. If `NULL`,
#'   displays a placeholder user icon. Default is `NULL`.
#'
#' @return A `bslib::card()` HTML element containing the formatted contact card
#'   with responsive styling, suitable for rendering in Shiny UI.
#' @export
contact_card <- function(
    name, role, org,
    email = NULL,
    website = NULL,
    photo = NULL
) {
  img_el <- if (!is.null(photo)) {
    bslib::card_image(src = photo, alt = paste(name, "photo"),
                      class = "card-img-top object-cover")
  } else {
    div(class = "card-img-top d-flex align-items-center justify-content-center placeholder",
        icon("user", class = "fa-2x text-secondary"))
  }

  btn <- function(href, icon_name, title, class = "btn btn-sm btn-outline-secondary me-2") {
    if (is.null(href) || href == "") return(NULL)
    tags$a(href = href, target = "_blank", class = class, title = title, icon(icon_name))
  }

  bslib::card(
    class = "contact-card overflow-hidden rounded-3 shadow-sm",
    img_el,
    bslib::card_body(
      h4(class = "mb-1", name),
      div(class = "text-muted", role),
      div(class = "text-muted small mb-3", org),
      div(
        class = "d-flex flex-wrap",
        if (!is.null(email))  btn(paste0("mailto:", email), "envelope", "Email", "btn btn-sm btn-primary me-2"),
        btn(website,  "globe",   "Website")
      )
    )
  )
}

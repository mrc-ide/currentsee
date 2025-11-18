# Create a contact card for RShiny applications

Generates a styled contact card component using bslib cards, displaying
personal information, photo, and contact links. The card includes
responsive design elements and integrates seamlessly with
Bootstrap-styled Shiny apps.

## Usage

``` r
contact_card(name, role, org, email = NULL, website = NULL, photo = NULL)
```

## Arguments

- name:

  Character string. The person's full name to display as the card title.

- role:

  Character string. The person's job title or role.

- org:

  Character string. The organization or company name.

- email:

  Character string, optional. Email address. If provided, creates a
  mailto link with an envelope icon. Default is `NULL`.

- website:

  Character string, optional. Website URL. If provided, creates an
  external link with a globe icon. Default is `NULL`.

- photo:

  Character string, optional. Path or URL to profile photo. If `NULL`,
  displays a placeholder user icon. Default is `NULL`.

## Value

A [`bslib::card()`](https://rstudio.github.io/bslib/reference/card.html)
HTML element containing the formatted contact card with responsive
styling, suitable for rendering in Shiny UI.

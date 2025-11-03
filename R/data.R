#' Package colour palette
#'
#' A dataset mapping malaria intervention package combinations to assigned colour
#' codes for consistent plotting and visualisation across outputs.
#'
#' @format A data frame with 24 rows and 2 variables:
#' \describe{
#'   \item{package}{Character string describing the intervention package (e.g. `"cm"`, `"cm, itn"`).}
#'   \item{colour}{Hexadecimal colour code associated with the package.}
#' }
#'
#' @source M3CPI project dataset
"colours_df"

#' Package flow labels
#'
#' A dataset with flow start and end codes and associated user-facing flow label
#'
#' @format A data frame with 39 rows and 3 variables:
#' \describe{
#'   \item{flow_start}{Character string describing the intervention package (e.g. `"cm"`, `"cm, itn"`) of flow origin.}
#'   \item{flow_end}{Character string describing the intervention package (e.g. `"cm"`, `"cm, itn"`) of flow terminus}
#'   \item{flow_label}{Character string of label to display for flow.}
#' }
#'
#' @source M3CPI project dataset
"flow_labels"

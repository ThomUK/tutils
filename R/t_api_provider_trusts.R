#' Use the NHS ORD API to fetch a lookup table of active trusts
#'
#' Queries the NHS Organisation Data Service API (Organisation Reference Data - ORD)
#' https://digital.nhs.uk/developer/api-catalogue/organisation-data-service-ord
#'
#' @param role character. NHS TRUST
#' @param include_inactive A logical vector to include inactive trusts in the
#'  result.  Defaults to FALSE.
#'
#' @return A dataframe of active trusts.
#'
#' @export
t_api_provider_trusts <- function(role = "NHS TRUST", include_inactive = FALSE){

  roleID <- switch(
    role,
    "NHS TRUST" = "RO197"
  )

  active_string <- ifelse(include_inactive, "", "&Status=Active")

  # construct the api url string
  url <- paste0(
    "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?PrimaryRoleId=RO197&Limit=1000",
    active_string
  )

  response <- httr::GET(url)

  data <- httr::content(response) |>
    tidyr::as_tibble() |>
    tidyr::unnest_wider("Organisations")

}

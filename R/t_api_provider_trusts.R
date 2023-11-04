#' Use the NHS ORD API to fetch a lookup table of active trusts
#'
#' Queries the NHS Organisation Data Service API (Organisation Reference Data - ORD)
#' https://digital.nhs.uk/developer/api-catalogue/organisation-data-service-ord
#'
#' @param role character. One of c("NHS TRUST", "NHS TRUST SITE", "ICB", "SUB ICB LOCATION", "GP"). Defaults to "NHS TRUST".
#' @param include_inactive logical. Include inactive trusts in the
#'  results? Defaults to FALSE.
#'
#' @return A dataframe of organisations.
#'
#' @export
#'
t_api_provider_trusts <- function(role = "NHS TRUST", include_inactive = FALSE){

  roleID <- switch(
    role,
    "NHS TRUST" = "RO197",
    "NHS TRUST SITE" = "RO198",
    "ICB" = "RO318",
    "SUB ICB LOCATION" = "RO319",
    "GP" = "RO76"
  )

  active_string <- ifelse(include_inactive, "", "&Status=Active")

  # construct the api url string
  url <- glue::glue(
    "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations?Roles={roleID}&Limit=1000",
    active_string
  )

  response <- httr::GET(url)

  # check that all the available records have been returned, if not throw a warning
  returned_count <- response[["headers"]][["returned-records"]]
  total_count <- response[["headers"]][["x-total-count"]]

  if(total_count > returned_count) warning(total_count, " records are available, but only the first ", returned_count, " are being returned.")

  message("Fetched ", returned_count, " Records.")

  data <- httr::content(response) |>
    tidyr::as_tibble() |>
    tidyr::unnest_wider("Organisations")

}

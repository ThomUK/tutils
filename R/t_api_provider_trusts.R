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

  # use a preliminary call to check the total number of records to fetch
  head <- httr::HEAD(url)
  total_records <- head[["headers"]][["x-total-count"]]
  message("There are ", total_records, " records to fetch")

  # get the first page, shape the results into a dataframe, and check if there is another page
  response <- httr::GET(url)

  data <- httr::content(response) |>
    tidyr::as_tibble() |>
    tidyr::unnest_wider("Organisations")

  next_url <- response[["headers"]][["next-page"]]

  # if there is another page, enter a while loop to fetch the rest of the data
  while(!is.null(next_url)){
    message("Fetching: ", next_url)

    # get the new page's data
    response <- httr::GET(next_url)

    # shape the new data
    new_data <- httr::content(response) |>
      tidyr::as_tibble() |>
      tidyr::unnest_wider("Organisations")

    # append the new data
    data <- rbind(data, new_data)

    next_url <- response[["headers"]][["next-page"]]
  }

  # check that all the available records have been returned, if not throw a warning
  # returned_count <- head[["headers"]][["returned-records"]]
  total_count <- response[["headers"]][["x-total-count"]]

  if(nrow(data) != total_records) stop("The number of rows in the dataframe is different to the number of records available.")

  return(data)

}

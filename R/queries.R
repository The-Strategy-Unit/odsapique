#' Return an organisation name, based on its organisation code
#'
#' @param org_code string - the organisation code
#' @returns A named character vector of length 1. The organisation name is the
#'  content, with the org code as name of the vector
get_name_from_org_code <- function(org_code) {
  resp <- organisation_query() |>
    httr2::req_url_path_append(org_code) |>
    httr2::req_perform() |>
    httr2::resp_check_status()
  org_name <- purrr::chuck(httr2::resp_body_json(resp), "name")
  rlang::set_names(org_name, org_code)
}


#' Return information about organisations from a name search
#'
#' The name search can be partial or exact. There are three flavours of search
#'  available:
#'  * "starts": search for an organisation name that starts with `name`
#'  * "contains": search for an organisation name that contains `name` anywhere
#'  * "exact": search for an organisation name that exactly matches `name`
#' Use the `type` argument to specify which one you want. `"starts"` is the
#'  default option.
#'
#' @param org_name string Part or whole organisation name to search for
#' @param type string Type of name search to perform
#' @returns A tibble with 2 columns: `org_code` and `org_name`, and a row for
#'  each organisation matched by `name` (according to the chosen search `type`)
get_org_info <- function(org_name, type = c("starts", "contains", "exact")) {
  type <- rlang::arg_match(type) |>
    switch(starts = "name", contains = "name:contains", exact = "name:exact")
  org_name <- toupper(org_name)
  resp <- organisation_query() |>
    httr2::req_url_query(!!type := org_name, .space = "form") |>
    httr2::req_perform() |>
    httr2::resp_check_status()
  resp_body <- purrr::chuck(httr2::resp_body_json(resp), "entry")
  tibble::tibble(
    org_code = purrr::map_chr(resp_body, c("resource", "id")),
    org_name = purrr::map_chr(resp_body, c("resource", "name"))
  )
}


#' @keywords internal
organisation_query <- function(api_url = "https://sandbox.api.service.nhs.uk") {
  httr2::req_url_path_append(core_query(api_url), "Organization")
}

#' @keywords internal
core_query <- function(api_url) {
  httr2::request(api_url) |>
    httr2::req_url_path_append("organisation-data-terminology-api") |>
    httr2::req_url_path_append("fhir")
}

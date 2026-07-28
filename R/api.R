#' A helper function to extract the next link from a response and return an
#'  amended request, or NULL if all results have now been returned
#' @keywords internal
get_next_link_url <- function(resp, req) {
  link_element <- purrr::pluck(httr2::resp_body_json(resp), "link")
  next_link_element <- purrr::keep(link_element, \(x) x[["relation"]] == "next")
  next_url <- purrr::pluck(next_link_element, 1, "url")
  if (is.null(next_url)) {
    NULL
  } else {
    httr2::req_url(req, next_url)
  }
}


#' Basic request to OrganizationAffiliation API
#' @inheritParams get_name_from_org_code
#' @keywords internal
org_affiliation_request <- function(...) {
  httr2::req_url_path_append(core_request(...), "OrganizationAffiliation")
}

#' Simple httr2 request passing the og code as an ID to the Organization API
#' @param id string An NHS England organisation code
#' @inheritParams get_name_from_org_code
#' @keywords internal
org_req <- \(id, ...) httr2::req_url_path_append(organisation_request(...), id)

#' Basic request to the Organization API
#' @inheritParams get_name_from_org_code
#' @keywords internal
organisation_request <- function(...) {
  httr2::req_url_path_append(core_request(...), "Organization")
}

#' A core httr2 `request` to be used by wrapper functions
#' @param api_url The base URL of the NHS England Organisation Data Terminology
#'  FHIR API Service
#' @keywords internal
core_request <- function(api_url = "https://sandbox.api.service.nhs.uk") {
  httr2::request(api_url) |>
    httr2::req_url_path_append("organisation-data-terminology-api") |>
    httr2::req_url_path_append("fhir") |>
    # See https://digital.nhs.uk/developer/api-catalogue/organisation-data-terminology#overview--rate-limits
    httr2::req_throttle(capacity = 5000, fill_time_s = 300)
}

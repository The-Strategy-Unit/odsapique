#' Return an organisation name, based on its organisation code
#'
#' @param org_code string - the organisation code
#' @returns A named character vector of length 1. The organisation name is the
#'  content, with the org code as name of the vector
#' @export
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
#' @param search_type string Type of name search to perform
#' @param org_role character vector Must be one of "all", "trust", "trust_site",
#'  "icb", "pcn" or "any". If "any" is specified, no filtering by organisation
#'  role will be done. If multiple values are supplied, the search will look for
#'  any of these types (an `OR` search). If "all" is specified, all results
#'  for NHS Trusts, NHS Trust sites, ICBs (technically sub-ICBs), and Primary
#'  Care Networks will be returned.
#' @returns A tibble with 5 columns: `org_code`, `org_name`, `org_role`, `city`
#'  and `postcode`, and a row for each organisation matched by `name`
#'  (according to the chosen `search_type` and `org_role` arguments)
#' @export
get_org_info <- function(
  org_name,
  search_type = c("starts", "contains", "exact"),
  org_role = c("all", "trust", "trust_site", "icb", "pcn", "any")
) {
  search_type <- rlang::arg_match(search_type) |>
    switch(starts = "name", contains = "name:contains", exact = "name:exact")
  org_role <- rlang::arg_match(org_role, multiple = TRUE)
  org_name <- toupper(org_name)
  init_req <- organisation_query() |>
    httr2::req_url_query(!!search_type := org_name, .space = "form")
  if ("any" %in% org_role) {
    new_req <- init_req
  } else {
    role_codes <- convert_roles(org_role)
    new_req <- httr2::req_url_query(init_req, activeRoleCode = role_codes)
  }
  count <- min(1000L, get_result_count(new_req))
  final_req <- httr2::req_url_query(new_req, `_count` = count)
  resps <- httr2::req_perform_iterative(final_req, get_next_link_url)
  fails <- length(httr2::resps_failures(resps))
  if (fails > 0) {
    cli::cli_alert("{fails} request{?s} of {length(resps)} returned an error")
  }

  resource_data <- extract_resource_data(resps)
  vcc <- "valueCodeableConcept"
  # pluck location for organisation role label
  role_loc <- list("extension", 1, "extension", 2, vcc, "coding", 1, "display")
  resource_data |>
    purrr::map(\(x) {
      tibble::tibble(
        org_code = poss_map(x, "id"),
        org_name = poss_map(x, "name"),
        org_role = poss_map(x, list(!!!role_loc)),
        city = poss_map(x, list("address", 1, "city")),
        postcode = poss_map(x, list("address", 1, "postalCode"))
      )
    }) |>
    purrr::list_rbind()
}


#' Converts a vector of role labels such as "trust" to a single string of codes
#' @keywords internal
convert_roles <- function(roles) {
  sw_code <- \(x) switch(x, trust = 197, trust_site = 198, icb = 98, pcn = 272)
  if ("all" %in% roles) {
    role_codes <- c(197, 198, 98, 272)
  } else {
    role_codes <- purrr::map_int(roles, sw_code)
  }
  paste0(paste0("RO", role_codes), collapse = ",")
}


#' A "safe" version of purrr::map_chr that returns NA if a value is not found
#' @keywords internal
poss_map <- \(...) purrr::partial(purrr::map_chr, .default = NA_character_)(...)


#' Gets the count of results for a query
#' @keywords internal
get_result_count <- function(req) {
  req |>
    httr2::req_url_query(`_summary` = "count") |>
    httr2::req_perform() |>
    httr2::resp_check_status() |>
    httr2::resp_body_json() |>
    purrr::chuck("total")
}


#' Extracts data from a list of API responses
#' @keywords internal
extract_resource_data <- function(resps) {
  resps |>
    httr2::resps_successes() |>
    purrr::map(httr2::resp_body_json) |>
    purrr::map("entry") |>
    purrr::map_depth(2, "resource")
}


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

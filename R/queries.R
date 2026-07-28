#' Return an organisation name, based on its organisation code
#'
#' @param org_code string - the organisation code
#' @param ... Used for supplying an alternative API URL. Use only if needed.
#' @returns A named character vector of length 1. The organisation name is the
#'  content, with the org code as name of the vector
#' @export
get_name_from_org_code <- function(org_code, ...) {
  resp <- get_organisation_details(org_code, ...)
  org_name <- purrr::chuck(resp, "name")
  rlang::set_names(org_name, org_code)
}


#' Return information about organisations from a name search
#'
#' The name search can be partial or exact. There are three flavours of search
#'  available:
#'  * "starts": search for an organisation name that starts with `org_name`
#'  * "contains": search for an organisation name that contains `org_name`
#'     anywhere
#'  * "exact": search for an organisation name that exactly matches `org_name`
#' Use the `search_type` argument to specify which one you want. `"starts"` is
#'  the default option.
#'
#' @param org_name string Part or whole organisation name to search for
#' @param search_type string Type of name search to perform
#' @param org_role character vector Must be one of "all", "trust", "trust_site",
#'  "icb", "pcn" or "any". If "any" is specified, no filtering by organisation
#'  role will be done. If multiple values are supplied, the search will look for
#'  any of these types (an `OR` search). If "all" is specified, all results
#'  for NHS Trusts, NHS Trust sites, ICBs (technically sub-ICBs), and Primary
#'  Care Networks will be returned.
#' @inheritParams get_name_from_org_code
#' @returns A tibble with 5 columns: `org_code`, `org_name`, `org_role`, `city`
#'  and `postcode`, and a row for each organisation matched by `name`
#'  (according to the chosen `search_type` and `org_role` arguments)
#' @export
get_org_info <- function(
  org_name,
  search_type = c("starts", "contains", "exact"),
  org_role = c("all", "trust", "trust_site", "icb", "pcn", "any"),
  ...
) {
  search_type <- rlang::arg_match(search_type) |>
    switch(starts = "name", contains = "name:contains", exact = "name:exact")
  org_role <- rlang::arg_match(org_role, multiple = TRUE)
  org_name <- toupper(org_name)
  init_req <- organisation_request(...) |>
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
  role_loc <- org_role_location("display")
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

#' Query the Organization API using an organisation code
#'
#' See https://digital.nhs.uk/developer/api-catalogue/organisation-data-terminology#get-/Organization/-id-
#' @inheritParams get_name_from_org_code
#' @returns All data as a list
#' @export
get_organisation_details <- function(org_code, ...) {
  org_req(org_code, ...) |>
    httr2::req_perform() |>
    httr2::resp_check_status() |>
    httr2::resp_body_json()
}


#' Return affiliated sites of an NHS Trust, based on the Trust organisation code
#'
#' @inheritParams get_name_from_org_code
#' @keywords internal
#' @returns A character vector of site organisation codes
#' @export
get_sites_from_org_code <- function(org_code, ...) {
  if (!is_trust(org_code, ...)) {
    cli::cli_abort("The code {org_code} does not seem to be an NHS Trust")
  }
  aff_str <- "OrganizationAffiliation:participating-organization"
  init_req <- org_affiliation_request(...) |>
    httr2::req_url_query(active = "true") |>
    httr2::req_url_query(`participating-organization` = org_code) |>
    httr2::req_url_query(`_include` = aff_str)
  count <- min(1000L, get_result_count(init_req))
  new_req <- httr2::req_url_query(init_req, `_count` = count)
  resps <- httr2::req_perform_iterative(new_req, get_next_link_url)
  fails <- length(httr2::resps_failures(resps))
  if (fails > 0) {
    cli::cli_alert("{fails} request{?s} of {length(resps)} returned an error")
  }
  resource_data <- extract_resource_data(resps)
  resource_data |>
    purrr::map(\(x) {
      x <- purrr::keep(x, is_operated_by)
      poss_map(x, list("organization", "identifier", "value"))
    }) |>
    purrr::list_c() |>
    purrr::keep(\(x) is_trust_site(x, ...))
}


#' Check to see if a code represents an organisation that is an NHS Trust
#' @returns logical TRUE or FALSE
#' @keywords internal
is_trust <- function(org_code, ...) {
  role_loc <- org_role_location("code")
  value <- get_organisation_details(org_code, ...) |>
    purrr::pluck(!!!role_loc, .default = "no")
  value == "RO197"
}


#' Check to see if code represent NHS Trust sites
#' @param org_codes A character vector of organisation codes
#' @returns A logical vector of TRUE or FALSE values
#' @keywords internal
is_trust_site <- function(org_codes, ...) {
  dots <- rlang::list2(...)
  part_org_req <- \(...) purrr::partial(get_organisation_details, !!!dots)(...)
  resps <- httr2::req_perform_parallel(purrr::map(org_codes, part_org_req))
  role_loc <- org_role_location("code")
  role_codes <- resps |>
    httr2::resps_successes() |>
    purrr::map(httr2::resp_body_json) |>
    purrr::map_chr(\(x) purrr::pluck(x, !!!role_loc, .default = "no"))
  role_codes == "RO198"
}


#' Returns a list of elements to use as a location for the poss_map accessor fn.
#'
#' @param which string Whether to access the organisation role code or the
#'  organisation role name ("display")
#' @keywords internal
#' @returns A list
org_role_location <- function(which = c("code", "display")) {
  which <- rlang::arg_match(which)
  vcc <- "valueCodeableConcept"
  list("extension", 1, "extension", 2, vcc, "coding", 1, which)
}

#' A test to see if a relationship value is equal to "RE6" (IS_OPERATED_BY)
#' @param structure A section of a list
#' @returns logical TRUE or FALSE
#' @keywords internal
is_operated_by <- function(structure) {
  value <- purrr::pluck(structure, "code", 1, "coding", 1, "code")
  ifelse(is.null(value), FALSE, value == "RE6")
}

#' Converts a vector of role labels such as "trust" to a single string of codes
#' @param roles A character vector
#' @returns A character scalar (string)
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


#' Basic request to OrganizationAffiliation API
#' @inheritParams get_name_from_org_code
#' @keywords internal
org_affiliation_request <- function(...) {
  httr2::req_url_path_append(core_request(...), "OrganizationAffiliation")
}


#' @keywords internal
org_req <- \(id, ...) httr2::req_url_path_append(organisation_request(...), id)


#' Basic request to Organization API
#' @inheritParams get_name_from_org_code
#' @keywords internal
organisation_request <- function(...) {
  httr2::req_url_path_append(core_request(...), "Organization")
}

#' @keywords internal
core_request <- function(api_url = "https://sandbox.api.service.nhs.uk") {
  httr2::request(api_url) |>
    httr2::req_url_path_append("organisation-data-terminology-api") |>
    httr2::req_url_path_append("fhir") |>
    # See https://digital.nhs.uk/developer/api-catalogue/organisation-data-terminology#overview--rate-limits
    httr2::req_throttle(capacity = 5000, fill_time_s = 300)
}

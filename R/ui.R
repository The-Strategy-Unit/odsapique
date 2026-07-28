#' Return an organisation name, based on its organisation code
#'
#' @param org_code string An NHS England organisation code
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
  resps <- httr2::req_perform_iterative(final_req, get_next_link_url) |>
    alert_on_fails()

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
  resps <- httr2::req_perform_iterative(new_req, get_next_link_url) |>
    alert_on_fails()
  resource_data <- extract_resource_data(resps)
  resource_data |>
    purrr::map(\(x) {
      x <- purrr::keep(x, is_operated_by)
      poss_map(x, list("organization", "identifier", "value"))
    }) |>
    purrr::list_c() |>
    purrr::keep(\(x) is_trust_site(x, ...))
}

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
  part_org_req <- \(...) purrr::partial(org_req, !!!dots)(...)
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

#' Generates a cli alert if any requests have failed
#' @param resps A list of API responses
#' @keywords internal
#' @returns The list of resps that was passed in
alert_on_fails <- function(resps) {
  fails <- length(httr2::resps_failures(resps))
  if (fails > 0) {
    cli::cli_alert("{fails} request{?s} of {length(resps)} returned an error")
  }
  resps
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

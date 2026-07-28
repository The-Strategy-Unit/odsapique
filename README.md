# odsapique ![R](https://www.r-project.org/favicon-32x32.png)📦🏥❔

<!-- badges: start -->
[![Licence: MIT][mit_svg]](https://opensource.org/licenses/MIT) [![Project
Status: WIP -- Initial development is in progress, but there has not yet been a
stable release][repostatus_svg]][repostatus_info]

[mit_svg]: https://img.shields.io/badge/License-MIT-yellow.svg
[repostatus_svg]: https://www.repostatus.org/badges/latest/wip.svg
[repostatus_info]: https://www.repostatus.org/#wip
<!-- badges: end -->

An R package providing a few functions to help with basic querying of the NHS
[ODS <dfn id="fhir"><abbr title="Fast Healthcare Interoperability Resources">FHIR</abbr></dfn> API][api],
also known as the Organisation Data Terminology
<abbr title="Fast Healthcare Interoperability Resources">FHIR</abbr>
R4 API.

[api]: https://digital.nhs.uk/developer/api-catalogue/organisation-data-terminology

The API supports a selection of queries, including:

- :detective: Search for an organisation
- :microscope: Get organisation details
- :handshake: Search for a relationship
- :ledger: Get relationship details
- :newspaper: Get deletion notices
- :shrug: Perform a ValueSet expansion

The first two of these are supported by the `Organization` API, and these are
the only two features currently supported by this package.

The "relationship" features belong to the `OrganizationAffiliation` API and are
not yet supported by this package.

## Installation

```r
# install.packages("pak") # if not already installed
pak::pak("The-Strategy-Unit/odsapique")
```


## Examples of usage

Get the name of the organisation whose ODS code is "RH5"

```r
get_name_from_org_code("RH5")
```

Search for organisations whose name starts with "Somerset"

```r
get_org_info("Somerset")
```

Search for an organisation with the exact name "Somerset NHS Foundation Trust"

```r
get_org_info("Somerset NHS Foundation Trust", type = "exact")
```

Return all NHS Trust sites that are [operated by][api_opd] a given NHS Trust.
Supply the org code for the Trust.

> [!NOTE]
> This returns only sites (organisations that have an active role code of
> "RO198") that have an `RE6` ("IS OPERATED BY") relationship to the Trust.
> A Trust is defined as an organisation with an active role code of "RO197".

[api_opd]: https://www.odsdatasearchandexport.nhs.uk/referenceDataCatalogue/Relationships_571324965.html

```r
get_sites_from_org_code("RD8")
```


Please note that [throttling][api_rates] is in place, so heavy use may result in
delays in data being returned.

[api_rates]: https://digital.nhs.uk/developer/api-catalogue/organisation-data-terminology#overview--rate-limits

## Problems

Please use GitHub issues to log any problems, questions, or ideas for
extension or improvement.

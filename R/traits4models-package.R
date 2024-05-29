#' traits4models: From plant trait data to model parameters
#'
#'  Utility functions to facilitate harmonizing plant trait data bases and defining species parameter tables for process-based models
#'
#' @name traits4models-package
#' @aliases traits4models traits4models-package
#' @docType package
#' @author
#' Maintainer: Miquel De Cáceres
#' \email{miquelcaceres@@gmail.com}
#' [ORCID](https://orcid.org/0000-0001-7132-2080)
#'
#' Author: Nicolas Martin-StPaul
#' [ORCID](https://orcid.org/0000-0001-7574-0108)
#'
#' Author: Adriana Tovar
#'
#' Author: Víctor Granda
#' [ORCID](https://orcid.org/0000-0002-0469-1991)
#'
#' @seealso Useful links: \itemize{ \item{
#' \url{https://emf-creaf.github.io/traits4models/index.html}} }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import medfate
#' @importFrom assertthat assert_that
#' @importFrom cli cli_li cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom dplyr bind_rows relocate left_join rename
#' @importFrom memoise memoise
#' @importFrom readr read_delim
#' @importFrom taxize classification get_gbifid_
#' @importFrom tibble tibble as_tibble
#' @importFrom utils data
#' @importFrom WorldFlora WFO.match WFO.one
## usethis namespace: end
NULL

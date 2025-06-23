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
#' @import data.table
#' @import cli
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows relocate left_join rename
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom memoise memoise
#' @importFrom rlang .data
#' @importFrom stats family quantile
#' @importFrom readr read_delim
#' @importFrom tibble tibble as_tibble
#' @importFrom utils data read.csv2 write.csv2 packageVersion
#' @importFrom WorldFlora WFO.match WFO.one
## usethis namespace: end
NULL

# global variable exporting
utils::globalVariables(c(".data","HarmonizedTraitDefinition", "SpParamsDefinition"))

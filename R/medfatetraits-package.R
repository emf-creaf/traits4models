#' medfatetraits: From plant trait data to model parameters
#'
#'  Utility functions to facilitate harmonizing plant trait data bases and defining species parameter tables for medfate.
#'
#' @name medfatetraits-package
#' @aliases medfatetraits medfatetraits-package
#' @docType package
#' @author
#' Maintainer: Miquel De Cáceres
#' \email{miquelcaceres@@gmail.com}
#' [ORCID](https://orcid.org/0000-0001-7132-2080)
#'
#' Author: Víctor Granda
#' [ORCID](https://orcid.org/0000-0002-0469-1991)
#'
#' Author: Adriana Tovar
#'
#' Author: Antoine Cabon
#' [ORCID](https://orcid.org/0000-0001-6426-1726)
#'
#' Author: Nicolas Martin-StPaul
#' [ORCID](https://orcid.org/0000-0001-7574-0108)
#'
#' Author: Arsène Druel
#' [ORCID](https://orcid.org/0000-0002-3938-0085)
#'
#' @seealso Useful links: \itemize{ \item{
#' \url{https://emf-creaf.github.io/medfateutils/index.html}} }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import medfate
#' @importFrom assertthat assert_that
#' @importFrom cli cli_li cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom dplyr bind_rows relocate left_join rename
#' @improtFrom readr read_delim
#' @importFrom taxize classification get_gbifid_
#' @importFrom tibble tibble as_tibble
#' @importFrom utils data
#' @importFrom WorldFlora WFO.match WFO.one
## usethis namespace: end
NULL

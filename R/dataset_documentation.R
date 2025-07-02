
#' Plant trait definitions
#'
#' A data frame of accepted plant trait definitions.
#'
#' @name HarmonizedTraitDefinition
#' @aliases HarmonizedTraitDefinition
#'
#' @docType data
#'
#' @format
#' A data frame with parameters in rows and columns:
#' \itemize{
#'   \item{\code{Definition}: Trait definition}
#'   \item{\code{Notation}: Trait notation required for harmonization.}
#'   \item{\code{Type}: Either 'Numeric' (for quantitative traits), 'Integer' (for counts) or 'String' (for qualitative traits).}
#'   \item{\code{Units}: Required units for quantitative traits.}
#'   \item{\code{EquivalentUnits}: Alternative, but equivalent, unit notation for quantitative traits.}
#'   \item{\code{AcceptedValues}: Set of comma-separated accepted values.}
#'   \item{\code{MinimumValue}: Minimum value for quantitative traits.}
#'   \item{\code{MaximumValue}: Maximum value for quantitative traits.}
#' }
#'
#' @examples
#' data(HarmonizedTraitDefinition)
#' @keywords data
NULL

#' Data tables with species parameter definition and values for different countries
#'
#' A data sets of species parameter tables resulting from existing databases to be used in conjunction
#' with national forest inventory data.
#'
#' @name SpParamsES
#' @aliases SpParamsES SpParamsFR SpParamsUS SpParamsAU SpParamsZM
#'
#' @docType data
#'
#' @format
#' \itemize{
#'   \item{Data frames \code{SpParamsES} (for Spain), \code{SpParamsFR} (for France), \code{SpParamsAU} (for Australia), \code{SpParamsUS} (for US) and \code{SpParamsZM} (for Zambia) have species or genus as rows and column names equal to parameter names
#'   in \code{SpParamsDefinition} (the latter from package \code{medfate}).}
#' }
#' @details
#' \code{SpParamsES}, \code{SpParamsFR},  \code{SpParamsAU}, \code{SpParamsUS} and \code{SpParamsZM} are species parameter data frames designed to be used with National Forest Inventories
#' of Spain, France, Australia and USA, respectively.
#'
#' Details of the procedures used to obtain the species parameter tables can be found in an article at https://emf-creaf.github.io/traits4models/.
#' @examples
#' data(SpParamsES)
#' data(SpParamsFR)
#' data(SpParamsUS)
#' data(SpParamsAU)
#' data(SpParamsZM)
#' @keywords data
NULL

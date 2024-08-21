read_WFO_backbone<- function(WFO_backbone_file) {
  r <- options(readr.show_col_types = FALSE)
  classification  <- readr::read_delim(file = WFO_backbone_file,
                                       delim = "\t", escape_double = FALSE,
                                       trim_ws = TRUE)|> tibble::tibble()
  options(readr.show_col_types = r)
  return(classification)
}

#' Harmonizes taxonomy
#'
#' Harmonizes plant taxonomy according to World Flora Online
#'
#' @param db Data frame to harmonize, with species names in a column called 'originalName'. The data frame will normally
#' include other columns which are transferred unaltered to the output data frame.
#' @param WFO_backbone_file Path to file containing the backbone of World Flora Online.
#' @param progress A boolean flag to prompt progress.
#' @param verbose A boolean flag to print console output with matching information.
#'
#' @return A data frame with columns:
#'  \itemize{
#'    \item{\code{originalName}: Original taxon name given in the input data frame.}
#'    \item{\code{acceptedName}: Accepted taxon name according to World Flora Online.}
#'    \item{\code{acceptedNameAuthorship}: Accepted taxon name with authorship according to World Flora Online.}
#'    \item{\code{family}: Taxonomic family of the accepted taxon.}
#'    \item{\code{genus}: Taxonomic genus of the accepted taxon.}
#'    \item{\code{specificEpithet}: Specific epithet of the accepted taxon.}
#'    \item{\code{taxonRank}: Taxonomic rank of the accepted taxon (e.g. "species", "subspecies", "genus", ...).}
#'  }
#'  Additional columns may be present, coming from the input data frame. These are left unmodified.
#' @export
#'
harmonize_taxonomy_WFO <- function(db, WFO_backbone_file, progress = TRUE, verbose = FALSE) {

  if(progress) cli::cli_progress_step("Reading WFO backbone")
  classification <- read_WFO_backbone(WFO_backbone_file)

  if(progress) cli::cli_progress_step("Taxon matching")
  WFO.match<- WorldFlora::WFO.match(unique(db$originalName),
                                    WFO.data = classification, Fuzzy = 0.05,
                                    verbose = verbose)
  if(progress) cli::cli_progress_step("Find best matching name for each initial taxon")
  WFO.one <- WorldFlora::WFO.one(WFO.match,
                                 verbose = verbose)

  if(progress) cli::cli_progress_step("Finalizing")
  db_post <- db |>
    dplyr::left_join(WFO.one[,c("spec.name", "scientificName", "scientificNameAuthorship", "family", "genus", "specificEpithet", "taxonRank")], by = c("originalName" = "spec.name"))|>
    dplyr::relocate(scientificName, scientificNameAuthorship, family, genus, specificEpithet, taxonRank, .after = originalName) |>
    dplyr::rename(acceptedName = scientificName,
                  acceptedNameAuthorship = scientificNameAuthorship)

  if(progress) cli::cli_progress_done()
  return(db_post)
}

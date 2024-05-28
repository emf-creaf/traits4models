read_WFO_backbone<- function(WFO_backbone_file) {
  classification  <- readr::read_delim(file = WFO_backbone_file,
                                       delim = "\t", escape_double = FALSE,
                                       trim_ws = TRUE)|> tibble::tibble()
  return(classification)
}

#' Harmonizes taxonomy
#'
#' Harmonizes plant taxonomy according to World Flora Online
#'
#' @param db Data frame to harmonize, with species names in column 'originalName'
#' @param WFO_backbone_file Path to file containing the backbone of World Flora Online
#'
#' @return A data frame
#' @export
#'
#' @examples
harmonize_taxonomy_WFO <- function(db, WFO_backbone_file) {

  classification <- read_WFO_backbone(WFO_backbone_file)

  WFO.match<- WorldFlora::WFO.match(unique(db$originalName),
                                    WFO.data = classification, Fuzzy = 0.05)
  WFO.one <- WorldFlora::WFO.one(WFO.match)

  db_post <- db |>
    dplyr::left_join(WFO.one[,c("spec.name", "scientificName", "scientificNameAuthorship", "family", "genus", "specificEpithet", "taxonRank")], by = c("originalName" = "spec.name"))|>
    dplyr::relocate(scientificName, scientificNameAuthorship, family, genus, specificEpithet, taxonRank, .after = originalName) |>
    dplyr::rename(acceptedName = scientificName,
                  acceptedNameAuthorship = scientificNameAuthorship)

  return(db_post)
}

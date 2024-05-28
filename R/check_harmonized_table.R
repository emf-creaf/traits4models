#' Checks trait data harmonization
#'
#' Checks that the input data frame has the appropriate format and data for model parameter filling.
#'
#' @param x A data frame with harmonized trait data
#'
#' @returns An (invisible) boolean indicating whether the data frame is acceptable or not.
#' @export
#'
check_harmonized_table<- function(x) {
  if(!inherits(x, "data.frame")) cli::cli_abort("Input should be a data frame")
  data("HarmonizedTraitDefinition", package = "traits4models")
  cn <- names(x)
  fixed <- c("originalName", "acceptedName","acceptedNameAuthorship","family",
             "genus", "specificEpithet","taxonRank")
  acceptable <- TRUE
  if(!all(fixed %in% cn)) {
    w_mis <- fixed[!(fixed %in% cn)]
    acceptable <- FALSE
    cli::cli_alert_warning(paste0("Taxonomy columns missing: ", paste0(w_mis, collapse =" ")))
  }
  if(!("Units" %in% cn)) {
    cli::cli_alert_info("Column 'Units' should preferably be defined.")
  }
  if(!("Reference" %in% cn)) {
    cli::cli_alert_info("Column 'Reference' should preferably be defined.")
  }
  other <- cn[!(cn %in% c(fixed, "Units", "Reference"))]
  for(t in other) {
    if(!(t %in% HarmonizedTraitDefinition$Notation)) {
      acceptable <- FALSE
      cli::cli_alert_warning(paste0("Trait column '", t, "' does not match any harmonized trait."))
    } else {
      type <- HarmonizedTraitDefinition$Type[HarmonizedTraitDefinition$Notation == t]
      if(type=="String") {
        if(!inherits(x[[t]], "character")) {
          acceptable <- FALSE
          cli::cli_alert_warning(paste0("Trait column '", t, "' should contain character data."))
        }
      } else if(type =="Integer") {
        if(!inherits(x[[t]], "integer")) {
          acceptable <- FALSE
          cli::cli_alert_warning(paste0("Trait column '", t, "' should contain integer data."))
        }
      } else if(type =="Numeric") {
        if(!inherits(x[[t]], "numeric")) {
          acceptable <- FALSE
          cli::cli_alert_warning(paste0("Trait column '", t, "' should contain numeric data."))
        }
      }
    }
  }
  if(!acceptable) cli::cli_alert_warning("The data frame is not acceptable as harmonized source data.")
  else  cli::cli_alert_success("The data frame is acceptable as harmonized source data.")

  return(invisible(acceptable))
}

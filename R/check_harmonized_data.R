#' Check data harmonization
#'
#' Functions to check that the input data frame (trait table or allometry table) has the appropriate format and data for model parameter filling.
#'
#' @param x A data frame with harmonized trait data or harmonized allometry data
#'
#' @details
#' Function \code{check_harmonized_trait()} checks that the input data frame conforms to the following requirements:
#'   \itemize{
#'     \item{Has columns called \code{originalName}, \code{acceptedName}, \code{acceptedNameAuthorship}, \code{family}, \code{genus}, \code{specificEpithet}, and \code{taxonRank},
#'     as returned by function \code{\link{harmonize_taxonomy_WFO}}.}
#'     \item{The names of the remaining columns are "Units", "Reference", "Priority" or a valid trait name according to the notation required in \code{\link{HarmonizedTraitDefinition}}.}
#'     \item{For trait columns, their values conform to the required type in \code{\link{HarmonizedTraitDefinition}}.}
#'   }
#' Function \code{check_harmonized_allometry()} checks that the input data frame conforms to the following requirements:
#'   \itemize{
#'     \item{Has columns called \code{originalName}, \code{acceptedName}, \code{acceptedNameAuthorship}, \code{family}, \code{genus}, \code{specificEpithet}, and \code{taxonRank},
#'     as returned by function \code{\link{harmonize_taxonomy_WFO}}.}
#'     \item{Has columns called \code{"Response"} and \code{"Equation"}. Columns \code{"Priority"} and \code{"Reference"} are recommended.}
#'     \item{Column \code{"Response"} identifies the response variable.}
#'     \item{Column \code{"ResponseDescription"} contains a longer description of the response variable.}
#'     \item{Columns \code{"Predictor1"}, \code{"Predictor2"}, ... identify predictor variables.}
#'     \item{Columns \code{"PredictorDescription1"}, \code{"PredictorDescription2"}, ... contain longer descriptions of predictor variables.}
#'     \item{Allometry coefficient columns should be named \code{"a"}, \code{"c"}, \code{"c"}, ..., \code{"f"} and contain numeric values.}
#'   }
#' @returns An (invisible) logical indicating whether the data frame is acceptable or not.
#' @seealso \code{\link{harmonize_taxonomy_WFO}}, \code{\link{HarmonizedTraitDefinition}}
#'
#' @name check_harmonized_trait
#' @export
#'
check_harmonized_trait<- function(x) {
  if(!inherits(x, "data.frame")) cli::cli_abort("Input should be a data frame")
  cn <- names(x)
  fixed <- c("originalName", "acceptedName","acceptedNameAuthorship","family",
             "genus", "specificEpithet","taxonRank")
  acceptable <- TRUE
  if(!all(fixed %in% cn)) {
    w_mis <- fixed[!(fixed %in% cn)]
    acceptable <- FALSE
    cli::cli_alert_warning(paste0("Taxonomy columns missing: ", paste0(w_mis, collapse =" ")))
  }
  # if(!("Units" %in% cn)) {
  #   cli::cli_alert_info("Column 'Units' should preferably be defined.")
  # }
  if(!("Reference" %in% cn)) {
    cli::cli_alert_info("Column 'Reference' should preferably be defined.")
  }
  if(!("Priority" %in% cn)) {
    cli::cli_alert_info("Column 'Priority' should preferably be defined.")
  }
  other <- cn[!(cn %in% c(fixed, "Units", "Reference", "Priority"))]
  for(t in other) {
    if(!(t %in% traits4models::HarmonizedTraitDefinition$Notation)) {
      acceptable <- FALSE
      cli::cli_alert_warning(paste0("Trait column '", t, "' does not match any harmonized trait definition."))
    } else {
      type <- traits4models::HarmonizedTraitDefinition$Type[traits4models::HarmonizedTraitDefinition$Notation == t]
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
  if(!acceptable) cli::cli_alert_warning("The data frame is not acceptable as harmonized trait data source.")
  else  cli::cli_alert_success("The data frame is acceptable as harmonized trait data source.")

  return(invisible(acceptable))
}


#' @export
#' @rdname check_harmonized_trait
check_harmonized_allometry<- function(x) {
  if(!inherits(x, "data.frame")) cli::cli_abort("Input should be a data frame")
  cn <- names(x)
  fixed <- c("originalName", "acceptedName","acceptedNameAuthorship","family",
             "genus", "specificEpithet","taxonRank","Response", "Equation")
  acceptable <- TRUE
  if(!all(fixed %in% cn)) {
    w_mis <- fixed[!(fixed %in% cn)]
    acceptable <- FALSE
    cli::cli_alert_warning(paste0("Required columns missing: ", paste0(w_mis, collapse =" ")))
  }
  if(!("Reference" %in% cn)) {
    cli::cli_alert_info("Column 'Reference' should preferably be defined.")
  }
  if(!("Priority" %in% cn)) {
    cli::cli_alert_info("Column 'Priority' should preferably be defined.")
  }
  if(!("ResponseDescription" %in% cn)) {
    cli::cli_alert_info("Column 'ResponseDescription' should preferably be defined.")
  }
  other <- cn[!(cn %in% c(fixed, "Reference", "Priority", "ResponseDescription"))]
  for(col in other) {
    if(!(col %in% c("Predictor1", "Predictor2", "Predictor3", "Predictor4", "Predictor5",
                    "PredictorDescription1", "PredictorDescription2","PredictorDescription3", "PredictorDescription4", "PredictorDescription5",
                    "a", "b", "c", "d", "e", "f"))) {
      acceptable <- FALSE
      cli::cli_alert_warning(paste0("Column '", col, "' is not recognized."))
    }
    if(acceptable) {
      if(col %in% c("Predictor1", "Predictor2", "Predictor3", "Predictor4","Predictor5",
                    "PredictorDescription1", "PredictorDescription2","PredictorDescription3", "PredictorDescription4","PredictorDescription5")) {
        if(!inherits(x[[col]], "character")) {
          acceptable <- FALSE
          cli::cli_alert_warning(paste0("Column '", col, "' should contain strings."))
        }
      } else {
        if(!inherits(x[[col]], "numeric")) {
          acceptable <- FALSE
          cli::cli_alert_warning(paste0("Coefficient column '", col, "' should contain numeric data."))
        }
      }
    }
  }
  if(!acceptable) cli::cli_alert_warning("The data frame is not acceptable as harmonized allometry data source.")
  else  cli::cli_alert_success("The data frame is acceptable as harmonized allometry data source.")
  return(invisible(acceptable))
}

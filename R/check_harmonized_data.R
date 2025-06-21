#' Check data harmonization
#'
#' Functions to check that the data tables (trait tables or allometry tables) have the appropriate format and data for model parameter filling.
#'
#' @param x A data frame with harmonized trait data or harmonized allometry data
#' @param verbose A logical flag indicating extra console output (information alerts and check success)
#'
#' @details
#' Function \code{check_harmonized_trait()} checks that the input data frame conforms to the following requirements:
#'   \enumerate{
#'     \item{Has columns called \code{originalName}, \code{acceptedName}, \code{acceptedNameAuthorship}, \code{family}, \code{genus}, \code{specificEpithet}, and \code{taxonRank},
#'     as returned by function \code{\link{harmonize_taxonomy_WFO}}.}
#'     \item{Trait data can be in wide-format or long-format. When supplied in wide format, the names of trait columns are either a valid trait names according to the notation in \code{\link{HarmonizedTraitDefinition}} and the type of column needs to match the corresponding type. In this format
#'     trait units cannot be checked and the same reference applies to all traits. When supplied in long format, the following columns are required:
#'        \itemize{
#'          \item{\code{Trait} - A character describing the the name of the trait, with values according to the notation in \code{\link{HarmonizedTraitDefinition}}.}
#'          \item{\code{Value} - A character or numeric containing the trait value. For numeric traits, values should be coercible to numbers via \code{\link{as.numeric}}.}
#'          \item{\code{Units} - A character describing the units of the trait, with values according to units in \code{\link{HarmonizedTraitDefinition}}.}
#'        }
#'      }
#'     \item{The names of the remaining columns are:
#'        \itemize{
#'          \item{\code{Reference} - A string describing the harmonized trait data source, i.e. normally a reference.}
#'          \item{\code{DOI} - A string describing the DOI of the harmonized trait data source.}
#'          \item{\code{OriginalReference} - A string describing the original source of trait data (if \code{Reference} is a compilation).}
#'          \item{\code{OriginalDOI} - A string describing the DOI of the original source of trait data (if \code{Reference} is a compilation).}
#'          \item{\code{Priority} - A numeric value to prioritize some data values (sources) over others (1 means highest priority).}
#'          \item{\code{checkVersion} - A string of the package version used for harmonization checking.}
#'        }
#'        Columns \code{OriginalReference} and \code{OriginalDOI} are optional, and the remaining columns are recommended.
#'      }
#'     \item{Trait value columns contain non-missing data.}
#'     \item{Numeric trait columns do not contain values beyond expected ranges (when defined).}
#'   }
#' Function \code{check_harmonized_allometry()} checks that the input data frame conforms to the following requirements:
#'   \enumerate{
#'     \item{Has columns called \code{originalName}, \code{acceptedName}, \code{acceptedNameAuthorship}, \code{family}, \code{genus}, \code{specificEpithet}, and \code{taxonRank},
#'     as returned by function \code{\link{harmonize_taxonomy_WFO}}.}
#'     \item{Has columns called \code{Response} and \code{Equation}. Columns \code{Priority}, \code{Reference} and \code{DOI} are recommended.}
#'     \item{Column \code{Response} identifies the response variable.}
#'     \item{Column \code{ResponseDescription} contains a longer description of the response variable.}
#'     \item{Columns \code{Predictor1}, \code{Predictor2}, ... identify predictor variables.}
#'     \item{Columns \code{PredictorDescription1}, \code{PredictorDescription2}, ... contain longer descriptions of predictor variables.}
#'     \item{Allometry coefficient columns should be named \code{a}, \code{b}, \code{c}, ..., \code{f} and contain numeric values.}
#'   }
#' Functions \code{check_harmonized_trait_dir()} and \code{check_harmonized_allometry_dir()} allow checking the appropriateness of multiple harmonized files at once.
#'
#' @returns
#' \itemize{
#'  \item{Functions \code{check_harmonized_trait()} and \code{check_harmonized_allometry()}} return an (invisible) logical indicating whether the data frame is acceptable or not.
#'  \item{Functions \code{check_harmonized_trait_dir()} and \code{check_harmonized_allometry_dir()}} return an (invisible) logical vector indicating those files whose data is acceptable or not.
#' }
#'
#' @seealso \code{\link{harmonize_taxonomy_WFO}}, \code{\link{HarmonizedTraitDefinition}}
#'
#' @name check_harmonized_trait
#' @export
#'
check_harmonized_trait<- function(x, verbose = TRUE) {
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
  if(!("Reference" %in% cn)) {
    if(verbose) cli::cli_alert_info("Column 'Reference' should preferably be defined.")
  }
  if(!("DOI" %in% cn)) {
    if(verbose) cli::cli_alert_info("Column 'DOI' should preferably be defined.")
  }
  if(!("Priority" %in% cn)) {
    if(verbose) cli::cli_alert_info("Column 'Priority' should preferably be defined.")
  }
  if(!("checkVersion" %in% cn)) {
    if(verbose) cli::cli_alert_info("Column 'checkVersion' should preferably be defined.")
  }

  format <- "undefined"
  if(all(c("Trait", "Value", "Units") %in% cn)) {
    format <- "long"
  } else if(all(!(c("Trait", "Value", "Units") %in% cn))) {
    format <- "wide"
  } else {
    acceptable <- FALSE
    cli::cli_alert_warning("Trait data should be in either long or wide format (see documentation)")
  }
  if(format == "wide") {
    other <- cn[!(cn %in% c(fixed, "Units", "Reference", "DOI", "OriginalReference", "OriginalDOI","Priority", "checkVersion"))]
    for(t in other) {
      if(!(t %in% traits4models::HarmonizedTraitDefinition$Notation)) {
        acceptable <- FALSE
        cli::cli_alert_warning(paste0("Trait column '", t, "' does not match any harmonized trait definition."))
      } else {
        row <- which(traits4models::HarmonizedTraitDefinition$Notation==t)
        if(sum(is.na(x[[t]])) == length(x[[t]])) {
          acceptable <- FALSE
          cli::cli_alert_warning(paste0("Trait column '", t, "' does not contain any data."))
        } else {
          type <- traits4models::HarmonizedTraitDefinition$Type[row]
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
          if(acceptable && (type %in% c("Integer", "Numeric"))) {
            min_value <- min(x[[t]], na.rm=TRUE)
            max_value <- max(x[[t]], na.rm=TRUE)
            if(!is.na(traits4models::HarmonizedTraitDefinition$MinimumValue[row])) {
              if(min_value < traits4models::HarmonizedTraitDefinition$MinimumValue[row]) {
                acceptable <- FALSE
                cli::cli_alert_warning(paste0("Trait column '", t, "' has values below accepted range."))
              }
            }
            if(!is.na(traits4models::HarmonizedTraitDefinition$MaximumValue[row])) {
              if(max_value > traits4models::HarmonizedTraitDefinition$MaximumValue[row]) {
                acceptable <- FALSE
                cli::cli_alert_warning(paste0("Trait column '", t, "' has values above accepted range."))
              }
            }
          }
          if(acceptable) {
            accepted_values <- traits4models::HarmonizedTraitDefinition$AcceptedValues[row]
            if(!is.na(accepted_values)) {
              accepted_values <- strsplit(accepted_values, ",")[[1]]
              if(type=="Integer") accepted_values <- as.integer(accepted_values)
              else if(type=="Numeric") accepted_values <- as.numeric(accepted_values)
              vals <- x[[t]]
              if(!all(vals[!is.na(vals)] %in% accepted_values)) {
                cli::cli_alert_warning(paste0("Values for trait '", t, "' include non-accepted values."))
                acceptable <- FALSE
              }
            }
          }
        }
      }
    }
  } else if(format =="long") {
    if(sum(is.na(x[["Trait"]])) == length(x[["Trait"]])) {
      acceptable <- FALSE
      cli::cli_alert_warning(paste0("`Trait` column does not contain any data."))
    }
    tt <- unique(x[["Trait"]])
    for(t in tt) {
      if(!(t %in% traits4models::HarmonizedTraitDefinition$Notation)) {
        acceptable <- FALSE
        cli::cli_alert_warning(paste0("Trait value '", t, "' does not match any harmonized trait definition."))
      } else {
        row <- which(traits4models::HarmonizedTraitDefinition$Notation==t)
        expected_type <- traits4models::HarmonizedTraitDefinition$Type[row]
        expected_unit <- traits4models::HarmonizedTraitDefinition$Units[row]
        alternative_unit <- traits4models::HarmonizedTraitDefinition$EquivalentUnits[row]
        accepted_values <- traits4models::HarmonizedTraitDefinition$AcceptedValues[row]

        sel <- x[["Trait"]]==t
        vals <- x[["Value"]][sel]
        units <- x[["Units"]][sel]
        t_acceptable <- TRUE
        if(!is.na(expected_unit)) {
          units[is.na(units)] <- ""
          if(!all(units==expected_unit)) {
            if(!is.na(alternative_unit)) {
              if(!all(units==alternative_unit)) {
                cli::cli_alert_warning(paste0("Units for trait '", t, "' are different than expected ('", expected_unit, " or ", alternative_unit,"')."))
                t_acceptable <- FALSE
              }
            } else {
              cli::cli_alert_warning(paste0("Units for trait '", t, "' are different than expected ('", expected_unit,"')."))
              t_acceptable <- FALSE
            }
          }
        }
        if(t_acceptable) {
          if(any(is.na(vals))) {
            cli::cli_alert_warning(paste0("Values for trait '", t, "' should not be missing."))
            t_acceptable <- FALSE
          }
        }
        if(t_acceptable) {
          if(!is.na(accepted_values)) {
            accepted_values <- strsplit(accepted_values, ",")[[1]]
            if(expected_type=="Integer") accepted_values <- as.integer(accepted_values)
            else if(expected_type=="Numeric") accepted_values <- as.numeric(accepted_values)
            if(!all(vals %in% accepted_values)) {
              cli::cli_alert_warning(paste0("Values for trait '", t, "' include non-accepted values."))
              t_acceptable <- FALSE
            }
          }
        }
        if(t_acceptable && (expected_type %in% c("Integer", "Numeric"))) {
          vals_coerced <- as.numeric(vals)
          if(any(is.na(vals))) {
            cli::cli_alert_warning(paste0("Non-numeric values for numeric trait '", t, "' found."))
            t_acceptable <- FALSE
          }
        }
        if(t_acceptable && (expected_type %in% c("Integer", "Numeric"))) {
          min_value <- min(vals, na.rm=TRUE)
          max_value <- max(vals, na.rm=TRUE)
          if(!is.na(traits4models::HarmonizedTraitDefinition$MinimumValue[row])) {
            if(min_value < traits4models::HarmonizedTraitDefinition$MinimumValue[row]) {
              acceptable <- FALSE
              cli::cli_alert_warning(paste0("Trait '", t, "' has values below accepted range."))
            }
          }
          if(!is.na(traits4models::HarmonizedTraitDefinition$MaximumValue[row])) {
            if(max_value > traits4models::HarmonizedTraitDefinition$MaximumValue[row]) {
              acceptable <- FALSE
              cli::cli_alert_warning(paste0("Trait '", t, "' has values above accepted range."))
            }
          }
        }

        if(!t_acceptable) acceptable = FALSE
      }
    }
  }

  if((!acceptable) && verbose) cli::cli_alert_warning("The data frame is not acceptable as harmonized trait data source.")
  else if(verbose) cli::cli_alert_success(paste0("The data frame (", format, " format) is acceptable as harmonized trait data source."))

  return(invisible(acceptable))
}


#' @export
#' @param harmonized_trait_path The path to harmonized trait data files (.rds or .csv format).
#' @rdname check_harmonized_trait
check_harmonized_trait_dir<- function(harmonized_trait_path, verbose = TRUE) {
  fn_full <- list.files(harmonized_trait_path, full.names = TRUE)
  fn <- list.files(harmonized_trait_path)
  accepted <- rep(NA, length(fn))
  if(verbose) cli::cli_h1(paste0("Checking ", length(fn), " harmonized trait files"))
  for(i in 1:length(fn)) {
    if(verbose) cli::cli_bullets(fn[i])
    if(endsWith(fn[i], ".rds")) {
      tab <- readRDS(fn_full[i])
    } else if(endsWith(fn[i], ".csv")) {
      tab <- read.csv2(fn_full[i])
    }
    accepted[i] <- traits4models::check_harmonized_trait(tab, verbose = verbose)
    if(!accepted[i]) cli::cli_alert_warning(paste0("File ", fn[i], " is not acceptable."))
  }
  return(invisible(accepted))
}

#' @export
#' @rdname check_harmonized_trait
check_harmonized_allometry<- function(x, verbose = TRUE) {
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
    if(verbose) cli::cli_alert_info("Column 'Reference' should preferably be defined.")
  }
  if(!("DOI" %in% cn)) {
    if(verbose) cli::cli_alert_info("Column 'DOI' should preferably be defined.")
  }
  if(!("Priority" %in% cn)) {
    if(verbose) cli::cli_alert_info("Column 'Priority' should preferably be defined.")
  }
  if(!("ResponseDescription" %in% cn)) {
    if(verbose) cli::cli_alert_info("Column 'ResponseDescription' should preferably be defined.")
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
  if((!acceptable) && verbose) cli::cli_alert_warning("The data frame is not acceptable as harmonized allometry data source.")
  else if(verbose)  cli::cli_alert_success("The data frame is acceptable as harmonized allometry data source.")
  return(invisible(acceptable))
}

#' @export
#' @param harmonized_allometry_path The path to harmonized allometry data files (.rds or .csv format).
#' @rdname check_harmonized_trait
check_harmonized_allometry_dir<- function(harmonized_allometry_path, verbose = TRUE) {
  fn_full <- list.files(harmonized_allometry_path, full.names = TRUE)
  fn <- list.files(harmonized_allometry_path)
  accepted <- rep(NA, length(fn))
  if(verbose) cli::cli_h1(paste0("Checking ", length(fn), " harmonized allometry files"))
  for(i in 1:length(fn)) {
    if(verbose) cli::cli_bullets(fn[i])
    if(endsWith(fn[i], ".rds")) {
      tab <- readRDS(fn_full[i])
    } else if(endsWith(fn[i], ".csv")) {
      tab <- read.csv2(fn_full[i])
    }
    accepted[i] <- traits4models::check_harmonized_allometry(tab, verbose = verbose)
    if(!accepted[i]) cli::cli_alert_warning(paste0("File ", fn[i], " is not acceptable."))
  }
  return(invisible(accepted))
}

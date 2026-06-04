#' Checks species parameter tables
#'
#' Checks that the input data frame has the appropriate format and data for simulations with medfate.
#'
#' @param x A data frame with species parameter data
#' @param check_consitency A boolean flag to force checking ecological and physiological consistence of parameter values.
#' @param verbose A boolean flag to prompt detailed process information.
#'
#' @returns An (invisible) list with two data frames, one with missing values in strict parameters and the other with plant names whose parameters (observed or imputed) do not conform to physiological rules.
#'
#' @details The function performs the following checks:
#' \itemize{
#'   \item{The input is a data frame.}
#'   \item{All parameter names defined in \code{\link[medfate]{SpParamsDefinition}} should be listed as columns.}
#'   \item{Strict parameters should not contain missing values. Strict parameters are defined as such in \code{\link[medfate]{SpParamsDefinition}}.}
#' }
#'
#' When \code{check_consistency = TRUE}, the function performs the following consistency checks for all species:
#'
#' \enumerate{
#'   \item{Stem hydraulic vulnerability is not larger than leaf hydraulic vulnerability (i.e. VCstem_P50 not less negative than VCleaf_P50).}
#'   \item{Stem hydraulic vulnerability is not larger than root hydraulic vulnerability (i.e. VCstem_P50 not less negative than VCroot_P50).}
#' }
#'
#' @export
#' @examples
#' check_medfate_params(SpParamsES)
#' check_medfate_params(SpParamsFR)
#' check_medfate_params(SpParamsUS)
#' check_medfate_params(SpParamsAU)
check_medfate_params<- function(x, check_consistency = TRUE, verbose = TRUE) {
  if(!inherits(x, "data.frame")) cli::cli_abort("Input should be a data frame")
  cn <- names(x)
  fixed <- medfate::SpParamsDefinition$ParameterName
  if(!all(fixed %in% cn)) {
    w_mis <- fixed[!(fixed %in% cn)]
    cli::cli_abort(paste0("Parameter columns missing: ", paste0(w_mis, collapse =" ")))
  }
  wrong_types <- rep(FALSE, length(fixed))
  for(i in 1:length(fixed)) {
    p <- fixed[i]
    if(!all(is.na(x[[p]]))) {
      type  <- medfate::SpParamsDefinition$Type[medfate::SpParamsDefinition$ParameterName==p]
      if(type=="String") {
        if(!is.character(x[[p]])) {
          if(verbose) cli::cli_alert_warning(paste0("Parameter column '", p, "' should contain strings."))
          wrong_types[i] <- TRUE
        }
      } else if(type=="Integer") {
        if(!is.numeric(x[[p]])) {
          if(verbose) cli::cli_alert_warning(paste0("Parameter column '", p, "' should contain integer values."))
          wrong_types[i] <- TRUE
        }
      } else if(type=="Numeric") {
        if(!is.numeric(x[[p]])) {
          if(verbose) cli::cli_alert_warning(paste0("Parameter column '", p, "' should contain numeric values."))
          wrong_types[i] <- TRUE
        }
      }
    }
  }

  # Strict parameters should not contain missing values
  strict_params <- medfate::SpParamsDefinition$ParameterName[medfate::SpParamsDefinition$Strict]
  mis_strict <- data.frame(row.names = 1:nrow(x))
  for(p in strict_params) {
    mis_strict[[p]] <- is.na(x[[p]])
    if(any(mis_strict[[p]])) {
      if(verbose) cli::cli_alert_warning(paste0("Strict parameter column '", p, "' should not contain any missing value."))
    }
  }

  if(sum(wrong_types)==0 && sum(as.matrix(mis_strict))==0) {
    if(verbose) cli::cli_alert_success("The data frame is formally acceptable as species parameter table for medfate.")
  }

  failed_rules <- data.frame(row.names = x$Name) |>
    dplyr::mutate(Rule1 = FALSE,
                  Rule2 = FALSE)
  if(check_consistency) {
    for(i in 1:nrow(x)) {
      sp_name <- x$Name[i]
      StemP50 <- medfate::species_parameter(sp_name, SpParams = x, parName = "VCstem_P50", fillWithGenus = TRUE, fillMissing = TRUE)
      LeafP50 <- medfate::species_parameter(sp_name, SpParams = x, parName = "VCleaf_P50", fillWithGenus = TRUE, fillMissing = TRUE)
      RootP50 <- medfate::species_parameter(sp_name, SpParams = x, parName = "VCroot_P50", fillWithGenus = TRUE, fillMissing = TRUE)
      # Rule 1. Stem hydraulic vulnerability is not larger than leaf hydraulic vulnerability
      failed_rules$Rule1[i] <- LeafP50 < StemP50
      if(failed_rules$Rule1[i] && verbose) cli::cli_alert_warning(paste0("Rule1 failed for plant name '", sp_name, "'."))
      # Rule 2. Stem hydraulic vulnerability is not larger than root hydraulic vulnerability
      failed_rules$Rule2[i] <- RootP50 < StemP50
      if(failed_rules$Rule2[i] && verbose) cli::cli_alert_warning(paste0("Rule2 failed for plant name '", sp_name, "'."))
    }
    if(sum(as.matrix(failed_rules))==0) {
      if(verbose) cli::cli_alert_success("The data frame is physiologically acceptable as species parameter table for medfate.")
    }
  }
  return(invisible(list(mis_strict = mis_strict, failed_rules = failed_rules)))
}

#' Checks species parameter tables
#'
#' Checks that the input data frame has the appropriate format and data for simulations with medfate.
#'
#' @param x A data frame with species parameter data
#' @param verbose A boolean flag to prompt detailed process information.
#'
#' @returns An (invisible) data frame with missing values in strict parameters.
#'
#' @details The function performs the following checks:
#' \itemize{
#'   \item{The input is a data frame.}
#'   \item{All parameter names defined in \code{\link[medfate]{SpParamsDefinition}} should be listed as columns.}
#'   \item{Strict parameters should not contain missing values. Strict parameters are defined as such in \code{\link[medfate]{SpParamsDefinition}}.}
#' }
#' @export
#' @examples
#' check_medfate_params(SpParamsES)
#' check_medfate_params(SpParamsFR)
#' check_medfate_params(SpParamsUS)
#' check_medfate_params(SpParamsAU)
check_medfate_params<- function(x, verbose = TRUE) {
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
  if(sum(wrong_types)==0 && sum(as.matrix(mis_strict))==0) if(verbose) cli::cli_alert_success("The data frame is acceptable as species parameter table for medfate.")
  return(invisible(mis_strict))
}

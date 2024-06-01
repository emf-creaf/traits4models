#' Checks species parameter tables
#'
#' Checks that the input data frame has the appropriate format and data for simulations with medfate.
#'
#' @param x A data frame with species parameter data
#'
#' @returns An (invisible) boolean indicating whether the data frame is acceptable or not.
#' @export
#'
check_spparams<- function(x) {
  if(!inherits(x, "data.frame")) cli::cli_abort("Input should be a data frame")
  data("SpParamsDefinition", package = "medfate")
  cn <- names(x)
  fixed <- SpParamsDefinition$ParameterName
  acceptable <- TRUE
  if(!all(fixed %in% cn)) {
    w_mis <- fixed[!(fixed %in% cn)]
    acceptable <- FALSE
    cli::cli_alert_warning(paste0("Parameter columns missing: ", paste0(w_mis, collapse =" ")))
  }

  # Strict parameters should not contain missing values
  if("Strict" %in% names(SpParamsDefinition)) {
    strict_params <- SpParamsDefinition$ParameterName[SpParamsDefinition$Strict]
    for(p in strict_params) {
      if(any(is.na(x[[p]]))) {
        acceptable <- FALSE
        cli::cli_alert_warning(paste0("Strict parameter column '", p, "' should not contain any missing value."))
      }
    }
  }

  if(!acceptable) cli::cli_alert_warning("The data frame is not acceptable as species parameter table for medfate.")
  else  cli::cli_alert_success("The data frame is acceptable as species parameter table for medfate.")

  return(invisible(acceptable))
}

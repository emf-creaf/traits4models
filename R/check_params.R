#' Checks species parameter tables
#'
#' Checks that the input data frame has the appropriate format and data for simulations with medfate.
#'
#' @param x A data frame with species parameter data
#' @param check_consistency A boolean flag to force checking ecological and physiological consistence of parameter values.
#' @param verbose A boolean flag to prompt detailed process information.
#'
#' @returns An (invisible) list with two data frames, one with missing values in strict parameters and the other with plant names whose parameters (observed or imputed) do not conform to physiological rules.
#'
#' @details
#'
#' The function can perform two kinds of parameter check.
#'
#' A formal check is always performed with the following rules:
#' \itemize{
#'   \item{The input is a data frame.}
#'   \item{All parameter names defined in \code{\link[medfate]{SpParamsDefinition}} should be listed as columns.}
#'   \item{Strict parameters should not contain missing values. Strict parameters are defined as such in \code{\link[medfate]{SpParamsDefinition}}.}
#' }
#'
#' If \code{check_consistency = TRUE}, the function also performs the following consistency checks for the parameter values of each species:
#'
#' \enumerate{
#'   \item{Rule 1. Stem hydraulic vulnerability is not larger than leaf hydraulic vulnerability (VCstem_P50 < VCleaf_P50) (Bartlett et al. 2016).}
#'   \item{Rule 2. Stem hydraulic vulnerability is not larger than root hydraulic vulnerability (VCstem_P50 < VCroot_P50).}
#'   \item{Rule 3. Stem hydraulic vulnerability curve is consistent (VCstem_P88 < VCstem_P50 < VCstem_P12).}
#'   \item{Rule 4. Leaf hydraulic vulnerability curve is consistent (VCleaf_P88 < VCleaf_P50 < VCleaf_P12).}
#'   \item{Rule 5. Root hydraulic vulnerability curve is consistent (VCroot_P88 < VCroot_P50 < VCroot_P12).}
#'   \item{Rule 6. Maximum (stomatal) conductance is larger than minimum (cuticular) stomatal conductance (Gswmax > Gswmin).}
#'   \item{Rule 7. 12\% PLC does not occur at before stomatal closure, represented by water potential at turgor loss point (VCstem_P12 < Ptlp) (Bartlett et al. 2016).}
#'   \item{Rule 8. Leaf angle is not lower than 20 degrees.}
#' }
#'
#' Parameter consistency is conducted including imputation of missing values, according to medfate inbuilt parameter estimation.
#'
#' @references
#' Bartlett et al. (2016). The correlations and sequence of plant stomatal, hydraulic, and wilting responses to drought. PNAS 113: 13098-13103.
#'
#' @export
#' @examples
#' check_medfate_params(SpParamsES, check_consistency = FALSE)
check_medfate_params<- function(x, check_consistency = FALSE, verbose = TRUE) {
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
    dplyr::mutate(Rule1 = as.logical(NA),
                  Rule2 = as.logical(NA),
                  Rule3 = as.logical(NA),
                  Rule4 = as.logical(NA),
                  Rule5 = as.logical(NA),
                  Rule6 = as.logical(NA),
                  Rule7 = as.logical(NA),
                  Rule8 = as.logical(NA))
  if(check_consistency) {
    for(i in 1:nrow(x)) {
      sp_name <- x$Name[i]
      isNAStemP50 <- is.na(medfate::species_parameter(sp_name, SpParams = x, parName = "VCstem_P50", fillMissing = FALSE))
      isNAStemP12 <- is.na(medfate::species_parameter(sp_name, SpParams = x, parName = "VCstem_P12", fillMissing = FALSE))
      isNAStemP88 <- is.na(medfate::species_parameter(sp_name, SpParams = x, parName = "VCstem_P88", fillMissing = FALSE))
      isNALeafP12 <- is.na(medfate::species_parameter(sp_name, SpParams = x, parName = "VCleaf_P12", fillMissing = FALSE))
      isNALeafP50 <- is.na(medfate::species_parameter(sp_name, SpParams = x, parName = "VCleaf_P50", fillMissing = FALSE))
      isNALeafP88 <- is.na(medfate::species_parameter(sp_name, SpParams = x, parName = "VCleaf_P88", fillMissing = FALSE))
      isNARootP12 <- is.na(medfate::species_parameter(sp_name, SpParams = x, parName = "VCroot_P12", fillMissing = FALSE))
      isNARootP50 <- is.na(medfate::species_parameter(sp_name, SpParams = x, parName = "VCroot_P50", fillMissing = FALSE))
      isNARootP88 <- is.na(medfate::species_parameter(sp_name, SpParams = x, parName = "VCroot_P88", fillMissing = FALSE))
      isNAPtlp <- is.na(medfate::species_parameter(sp_name, SpParams = x, parName = "Ptlp", fillMissing = FALSE))
      isNAGswmin <- is.na(medfate::species_parameter(sp_name, SpParams = x, parName = "Gswmin", fillMissing = FALSE))
      isNAGswmax <- is.na(medfate::species_parameter(sp_name, SpParams = x, parName = "Gswmax", fillMissing = FALSE))
      isNALeafAngle <- is.na(medfate::species_parameter(sp_name, SpParams = x, parName = "LeafAngle", fillMissing = FALSE))
      StemP50 <- medfate::species_parameter(sp_name, SpParams = x, parName = "VCstem_P50", fillWithGenus = TRUE, fillMissing = TRUE)
      StemP12 <- medfate::species_parameter(sp_name, SpParams = x, parName = "VCstem_P12", fillWithGenus = TRUE, fillMissing = TRUE)
      StemP88 <- medfate::species_parameter(sp_name, SpParams = x, parName = "VCstem_P88", fillWithGenus = TRUE, fillMissing = TRUE)
      LeafP12 <- medfate::species_parameter(sp_name, SpParams = x, parName = "VCleaf_P12", fillWithGenus = TRUE, fillMissing = TRUE)
      LeafP50 <- medfate::species_parameter(sp_name, SpParams = x, parName = "VCleaf_P50", fillWithGenus = TRUE, fillMissing = TRUE)
      LeafP88 <- medfate::species_parameter(sp_name, SpParams = x, parName = "VCleaf_P88", fillWithGenus = TRUE, fillMissing = TRUE)
      RootP12 <- medfate::species_parameter(sp_name, SpParams = x, parName = "VCroot_P12", fillWithGenus = TRUE, fillMissing = TRUE)
      RootP50 <- medfate::species_parameter(sp_name, SpParams = x, parName = "VCroot_P50", fillWithGenus = TRUE, fillMissing = TRUE)
      RootP88 <- medfate::species_parameter(sp_name, SpParams = x, parName = "VCroot_P88", fillWithGenus = TRUE, fillMissing = TRUE)
      Gswmin <- medfate::species_parameter(sp_name, SpParams = x, parName = "Gswmin", fillWithGenus = TRUE, fillMissing = TRUE)
      Gswmax <- medfate::species_parameter(sp_name, SpParams = x, parName = "Gswmax", fillWithGenus = TRUE, fillMissing = TRUE)
      LeafPI0 <- medfate::species_parameter(sp_name, SpParams = x, parName = "LeafPI0", fillWithGenus = TRUE, fillMissing = TRUE)
      LeafEPS <- medfate::species_parameter(sp_name, SpParams = x, parName = "LeafEPS", fillWithGenus = TRUE, fillMissing = TRUE)
      Ptlp <- medfate::species_parameter(sp_name, SpParams = x, parName = "Ptlp", fillWithGenus = TRUE, fillMissing = TRUE)
      LeafAngle <- medfate::species_parameter(sp_name, SpParams = x, parName = "LeafAngle", fillWithGenus = TRUE, fillMissing = TRUE)

      # Rule 1. Stem hydraulic vulnerability is not larger than leaf hydraulic vulnerability
      failed_rules$Rule1[i] <- LeafP50 >= StemP50
      if(!failed_rules$Rule1[i] && verbose) cli::cli_alert_warning(paste0("Rule #1 failed for '", sp_name, "' [VCstem_P50",ifelse(isNAStemP50, "*","")," = ", format(StemP50, digits = 3), "; VCleaf_P50",ifelse(isNALeafP50, "*","")," = ", format(LeafP50, digits = 3) ,"]."))
      # Rule 2. Stem hydraulic vulnerability is not larger than root hydraulic vulnerability
      failed_rules$Rule2[i] <- RootP50 >= StemP50
      if(!failed_rules$Rule2[i] && verbose) cli::cli_alert_warning(paste0("Rule #2 failed for '", sp_name, "' [VCstem_P50",ifelse(isNAStemP50, "*","")," = ", format(StemP50, digits = 3), "; VCroot_P50",ifelse(isNARootP50, "*","")," = ", format(RootP50, digits = 3) ,"]."))
      # Rule 3. Stem hydraulic vulnerability curve is consistent
      failed_rules$Rule3[i] <- ((StemP88 < StemP50) && (StemP50 < StemP12))
      if(!failed_rules$Rule3[i] && verbose) cli::cli_alert_warning(paste0("Rule #3 failed for '", sp_name, "' [VCstem_P12",ifelse(isNAStemP12, "*","")," = ", format(StemP12, digits = 3), "; VCstem_P50",ifelse(isNAStemP50, "*","")," = ", format(StemP50, digits = 3) ,"; VCstem_P88",ifelse(isNAStemP88, "*","")," = ", format(StemP88, digits = 3) ,"]."))
      # Rule 4. Leaf hydraulic vulnerability curve is consistent
      failed_rules$Rule4[i] <- ((LeafP88 < LeafP50) && (LeafP50 < LeafP12))
      if(!failed_rules$Rule4[i] && verbose) cli::cli_alert_warning(paste0("Rule #4 failed for '", sp_name, "' [VCleaf_P12",ifelse(isNALeafP12, "*","")," = ", format(LeafP12, digits = 3), "; VCleaf_P50",ifelse(isNALeafP50, "*","")," = ", format(LeafP50, digits = 3) ,"; VCleaf_P88",ifelse(isNALeafP88, "*","")," = ", format(LeafP88, digits = 3) ,"]."))
      # Rule 5. Root hydraulic vulnerability curve is consistent
      failed_rules$Rule5[i] <- ((RootP88 < RootP50) && (RootP50 < RootP12))
      if(!failed_rules$Rule5[i] && verbose) cli::cli_alert_warning(paste0("Rule #5 failed for '", sp_name, "' [VCroot_P12",ifelse(isNARootP12, "*","")," = ", format(RootP12, digits = 3), "; VCroot_P50",ifelse(isNARootP50, "*","")," = ", format(RootP50, digits = 3) ,"; VCroot_P88",ifelse(isNARootP88, "*","")," = ", format(RootP88, digits = 3) ,"]."))
      # Rule 6. Maximum (stomatal) conductance is larger than minimum (cuticular) stomatal conductance
      failed_rules$Rule6[i] <- (Gswmax > Gswmin)
      if(!failed_rules$Rule6[i] && verbose) cli::cli_alert_warning(paste0("Rule #6 failed for '", sp_name, "' [Gswmax",ifelse(isNAGswmax, "*","")," = ", format(Gswmax, digits = 3), "; Gswmin",ifelse(isNAGswmin, "*","")," = ", format(Gswmin, digits = 3) ,"]."))
      # Rule 7: 12% PLC does not occur at before stomatal closure (represented by water potential at turgor loss point)
      failed_rules$Rule7[i] <- Ptlp >= StemP12
      if(!failed_rules$Rule7[i] && verbose) cli::cli_alert_warning(paste0("Rule #7 failed for '", sp_name, "' [VCstem_P12",ifelse(isNAStemP12, "*","")," = ", format(StemP12, digits = 3), "; Ptlp",ifelse(isNAPtlp, "*","")," = ", format(Ptlp, digits = 3) ,"]."))
      # Rule 8: Leaf angle is not lower than 20 degrees
      failed_rules$Rule8[i] <- LeafAngle >= 20
      if(!failed_rules$Rule8[i] && verbose) cli::cli_alert_warning(paste0("Rule #8 failed for '", sp_name, "' [LeafAngle",ifelse(isNALeafAngle, "*","")," = ", format(LeafAngle, digits = 3),"]."))
    }
    if(sum(as.matrix(failed_rules))==0) {
      if(verbose) cli::cli_alert_success("The data frame is physiologically acceptable as species parameter table for medfate.")
    }
  }
  return(invisible(list(mis_strict = mis_strict, failed_rules = failed_rules)))
}

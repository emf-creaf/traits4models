#' Complete strict parameters for medfate
#'
#' Taxonomy-based imputation of strict parameters in medfate.
#'
#' @param SpParams A species parameter data frame to be completed
#' @param params A character vector with strict parameters to be completed. If missing, all strict parameters will be checked for completion.
#' @param progress A boolean flag to prompt progress.
#' @param verbose A boolean flag to prompt detailed process information.
#'
#' @details
#' The function performs imputation of strict parameters on the basis of non-missing values in taxonomically-close taxa.
#' Strict parameters are defined as such in \code{\link[medfate]{SpParamsDefinition}}.
#' First, if the taxon with missing values is a subspecies, the function looks for values at the species level. Missing values at the species level are filled with data at the
#' genus, family, order or group levels, depending on the availability of information. Quantitative values are averaged, whereas for qualitative values the first non-missing value is used.
#' Beware that imputation can result in errors if the initial amount of information is scarce. Therefore, users should revised the output of this function and correct
#' potential errors manually, whenever detected.
#'
#' @return A species parameter data frame with imputed strict parameter values
#' @export
#'
complete_medfate_strict<- function(SpParams, params = NULL, progress = TRUE, verbose = FALSE) {
  get("SpParamsDefinition", envir = environment())
  if("Strict" %in% names(SpParamsDefinition)) {
    strict_params <- SpParamsDefinition$ParameterName[SpParamsDefinition$Strict]
    strict_params <-strict_params[!(strict_params %in% c("Name", "SpIndex", "AcceptedName", "Genus", "Family", "Order", "Group"))]
  } else {
    strict_params  <- c("GrowthForm", "LifeForm", "LeafShape", "LeafSize", "PhenologyType", "DispersalType", "Hmax", "Hmed", "Z95")
  }
  if(is.null(params)) params <- strict_params
  else params <- match.arg(params, strict_params, several.ok = TRUE)
  for(param in params) {
    type <- SpParamsDefinition$Type[SpParamsDefinition$ParameterName==param]
    if(progress) cli::cli_progress_step(paste0("Filling missing values for: ", param, " [", type,"]"))
    # Look for species if subspecies
    na_rows <- which(is.na(SpParams[[param]]))
    if(progress && verbose) cli::cli_li(paste0("Initial number of missing: ", length(na_rows)))
    for(r in na_rows) {
      if(!is.na(SpParams$Species[r])) {
        if(SpParams$AcceptedName[r] != SpParams$Species[r]) {
          rowSp = which(SpParams$AcceptedName==SpParams$Species[r])
          if(length(rowSp)>0) {
            if(type=="String") {
              SpParams[[param]][r] <- SpParams[[param]][rowSp[1]]
            } else {
              SpParams[[param]][r] <- mean(SpParams[[param]][rowSp], na.rm=TRUE)
            }
          }
        }
      }
    }
    # Look for genus if species
    na_rows <- which(is.na(SpParams[[param]]))
    if(progress && verbose) cli::cli_li(paste0("Number of missing after assigning species to sub-species: ", length(na_rows)))
    for(r in na_rows) {
      acc_name <- SpParams$AcceptedName[r]
      if(!is.na(acc_name)) {
        if(acc_name != SpParams$Genus[r]) {
          sel <- SpParams$AcceptedName==SpParams$Genus[r]
          sel[is.na(sel)] <- FALSE
          rowGen <- which(sel)
          if(length(rowGen)>0) {
            if(type=="String") {
              SpParams[[param]][r] <- SpParams[[param]][rowGen[1]]
            } else {
              SpParams[[param]][r] <- mean(SpParams[[param]][rowGen], na.rm=TRUE)
            }
          }
        }
      }
    }
    na_rows <- which(is.na(SpParams[[param]]))
    if(progress && verbose) cli::cli_li(paste0("Number of missing after assigning genus to species: ", length(na_rows)))
    for(r in na_rows) {
      fam <- SpParams$Family[r]
      if(!is.na(fam)) {
        fam_values <- SpParams[[param]][SpParams$Family==fam]
        fam_values <- fam_values[!is.na(fam_values)]
        if(length(fam_values)>0) {
          if(type=="String") {
            val <- names(sort(table(fam_values), decreasing = T))[1]
          } else {
            val <- mean(fam_values, na.rm=TRUE)
          }
          SpParams[[param]][r] <- val
        }
      }
    }
    na_rows <- which(is.na(SpParams[[param]]))
    if(progress && verbose) cli::cli_li(paste0("Number of missing after examining most common family values: ", length(na_rows)))
    for(r in na_rows) {
      ord <- SpParams$Order[r]
      if(!is.na(ord)) {
        ord_values <- SpParams[[param]][SpParams$Order==ord]
        ord_values <- ord_values[!is.na(ord_values)]
        if(length(ord_values)>0) {
          if(type=="String") {
            val <- names(sort(table(ord_values), decreasing = T))[1]
          } else {
            val <- mean(ord_values, na.rm=TRUE)
          }
          SpParams[[param]][r] <- val
        }
      }
    }
    na_rows <- which(is.na(SpParams[[param]]))
    if(progress && verbose) cli::cli_li(paste0("Number of missing after examining most common order values: ", length(na_rows)))
    for(r in na_rows) {
      gru <- SpParams$Group[r]
      if(!is.na(gru)) {
        gru_values <- SpParams[[param]][SpParams$Group==gru]
        gru_values <- gru_values[!is.na(gru_values)]
        if(length(gru_values)>0) {
          if(type=="String") {
            val <- names(sort(table(gru_values), decreasing = T))[1]
          } else {
            val <- mean(gru_values, na.rm=TRUE)
          }
          SpParams[[param]][r] <- val
        }
      }
    }
    na_rows <- which(is.na(SpParams[[param]]))
    if(progress && verbose) cli::cli_li(paste0("Number of missing after examining most common group values: ", length(na_rows)))
  }
  if(progress) cli::cli_progress_done()
  return(SpParams)
}

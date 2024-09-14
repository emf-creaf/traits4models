#' Initializes species parameter table
#'
#' Creates an empty plant parameter table for medfate, populating taxonomic information if desired.
#'
#' @param x A data frame with columns as returned by function \code{\link{harmonize_taxonomy_WFO}}.
#' @param complete_rows Boolean flag to indicate that extra rows should be added for cited species/genera (if \code{fill_taxonomy = TRUE}).
#' @param sort Boolean flag to force sorting in ascending order by \code{Name}.
#' @param verbose A boolean flag to indicate extra console output.
#'
#' @details Taxonomy is taken from the input data frame, and internal data obtained from package taxize is used for the taxa above family, only.
#'
#' @return A data frame with empty species parameter values suitable for medfate simulations.
#' The data frame will normally contain more rows if \code{complete_rows} is set to TRUE.
#'
#' @export
#'
#' @encoding UTF-8
#' @author  Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#'
#' @seealso \code{\link[medfate]{SpParamsMED}}
#'
init_medfate_params<-function(x,
                              complete_rows = TRUE,
                              sort = TRUE,
                              verbose = FALSE) {

  if(inherits(x, "data.frame")) {
    if(!("originalName" %in% names(x)))  cli::cli_abort("'x' should contain a column called 'originalName'.")
    if(!("acceptedName" %in% names(x)))  cli::cli_abort("'x' should contain a column called 'acceptedName'.")
    if(!("genus" %in% names(x)))  cli::cli_abort("'x' should contain a column called 'genus'.")
    if(!("specificEpithet" %in% names(x)))  cli::cli_abort("'x' should contain a column called 'specificEpithet'.")
    if(!("family" %in% names(x)))  cli::cli_abort("'x' should contain a column called 'family'.")
    names <- x$originalName
    accepted_names <- x$acceptedName
    species <- paste(x$genus, x$specificEpithet)
    species[is.na(x$specificEpithet)] <- x$genus[is.na(x$specificEpithet)]
    genus <- x$genus
    family <- x$family
  } else {
    cli::cli_abort("Wrong input for 'x'. Should be a data frame.")
  }
  if(verbose) cli::cli_progress_step("Initializing parameter table")
  SpParams <- data.frame(Name = as.character(names))
  for(cn in medfate::SpParamsDefinition$ParameterName) {
    if(!(cn %in% names(SpParams))) {
      SpParams[[cn]] = NA
    }
  }
  SpParams$AcceptedName <- accepted_names
  SpParams$Species <- species
  SpParams$Genus <- genus
  SpParams$Family <- family
  if(verbose) cli::cli_progress_step("Filling taxa above family")
  df_fam  <- SpParams[,"Family", drop=FALSE] |>
    dplyr::left_join(fam_data, by ="Family")
  SpParams$Order <- df_fam$Order
  SpParams$Group <- df_fam$Group

  if(complete_rows) {
    genera <- unique(SpParams$Genus)
    genera <- genera[!is.na(genera)]
    genera <- genera[!(genera %in% SpParams$AcceptedName)]
    if(length(genera)>0) {
      if(verbose) cli::cli_progress_step(paste0("Completing rows with ", length(genera), " genera"))
      gen_vec <- vector("list", length(genera))
      SpParams_filt <- SpParams[!is.na(SpParams$Genus),, drop = FALSE]
      for(i in 1:length(genera)) {
        g <- genera[i]
        row_g <- SpParams_filt[SpParams_filt$Genus == g,][1,, drop = FALSE]
        row_g$Name <- g
        row_g$AcceptedName <- g
        row_g$Species <- NA
        gen_vec[[i]] <- row_g
      }
      SpParams <- dplyr::bind_rows(SpParams, gen_vec)
    }
    species <- unique(SpParams$Species)
    species <- species[!is.na(species)]
    species <- species[!(species %in% SpParams$AcceptedName)]
    species <- species[!(species %in% genera)]
    species <- species[!endsWith(species, "x")]
    species <- species[!endsWith(species, "\u00d7")]
    if(length(species)>0) {
      if(verbose) cli::cli_progress_step(paste0("Completing rows with ", length(species), " species"))
      sp_vec <- vector("list", length(species))
      SpParams_filt <- SpParams[!is.na(SpParams$Species),, drop = FALSE]
      for(i in 1:length(species)) {
        s <- species[i]
        row_s <- SpParams_filt[SpParams_filt$Species == s,][1,, drop = FALSE]
        row_s$Name <- s
        row_s$AcceptedName <- s
        sp_vec[[i]] <- row_s
      }
      SpParams <- dplyr::bind_rows(SpParams, sp_vec)
    }
  }
  if(verbose) cli::cli_progress_step("Finalizing")
  if(sort) SpParams<- SpParams[order(SpParams$Name),, drop = FALSE]
  row.names(SpParams) <- NULL
  SpParams$SpIndex <- 0:(nrow(SpParams)-1)
  if(verbose) cli::cli_process_done()
  return(SpParams)
}

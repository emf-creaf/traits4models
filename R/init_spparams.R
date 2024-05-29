#' Initializes species parameter table
#'
#' Creates an empty plant parameter table for medfate, populating taxonomic information if desired.
#'
#' @param sp_names Vector of plant names, either taxon names or arbitrary species group names.
#' @param accepted_names Vector of accepted taxon names of the same length of \code{sp_names}.
#' @param fill_taxonomy Boolean flag to indicate that taxonomic information should be filled (retrieved from GBIF using package taxize).
#' @param complete_rows Boolean flag to indicate that extra rows should be added for cited species/genera (if \code{fill_taxonomy = TRUE}).
#' @param sort Boolean flag to force sorting in ascending order by \code{Name}.
#' @param verbose A boolean flag to indicate extra console output.
#'
#' @details Taxonomic information is retrieved using functions in package taxize and GBIF as data source.
#'
#' @return A data frame with empty species parameter values suitable for medfate simulations.
#' The data frame will normally contain more rows than \code{sp_names} because of arguments
#' \code{fill_taxonomy} and \code{complete_rows} are set to TRUE by default.
#'
#' @export
#'
#' @encoding UTF-8
#' @author  Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#'
#' @seealso \code{\link[medfate]{SpParamsMED}}
#'
#' @examples
#' \donttest{
#' # Simple example with two species
#' sp_names <- c("Salvia rosmarinus", "Pinus contorta")
#' init_spparams(sp_names)
#'
#' # Simple example with three species using synonyms and subspecies
#' sp_names <- c("Rosmarinus officinalis", "Pinus contorta", "Quercus ilex subsp. ilex")
#' accepted_names <- c("Salvia rosmarinus", "Pinus contorta", "Quercus ilex subsp. ilex")
#' init_spparams(sp_names, accepted_names)
#'
#'
#' }
init_spparams<-function(sp_names,
                       accepted_names = NULL,
                       fill_taxonomy = TRUE,
                       complete_rows = TRUE,
                       sort = TRUE,
                       verbose = FALSE) {

  if(length(sp_names)==0) stop("Please, supply at least one species name")
  if(length(sp_names) !=length(unique(sp_names))) stop("Plant names should be unique!")
  if(!is.null(accepted_names)) {
    if(length(accepted_names) != length(sp_names)) stop("The vector of accepted names has to be of the same length as the vector of species names")
  }
  data("SpParamsDefinition", package = "medfate")
  if(verbose) cli::cli_progress_step("Initializing parameter table")
  SpParams <- data.frame(Name = as.character(sp_names))
  for(cn in SpParamsDefinition$ParameterName) {
    if(!(cn %in% names(SpParams))) {
      SpParams[[cn]] = NA
    }
  }
  if(!is.null(accepted_names)) {
    SpParams$AcceptedName <- accepted_names
  } else {
    SpParams$AcceptedName <- SpParams$Name
  }
  if(fill_taxonomy) {
    if(verbose) cli::cli_progress_step("Retrieving taxonomy data")
    for(i in 1:nrow(SpParams)) {
      s = strsplit(SpParams$AcceptedName[i]," ")[[1]]
      SpParams$Genus[i] = s[1] # Genus always first word
      if(length(s)>1) {
        if(length(s)>2 && (s[2] %in% c("x", "×"))) {
          SpParams$Species[i] = paste0(s[1], " ", s[2], " ", s[3])
        } else {
          SpParams$Species[i] = paste0(s[1], " ", s[2])
        }
      }
      id_df<-taxize::get_gbifid_(s[1], messages = FALSE)[[1]]
      gbif_id<-numeric(0)
      if(nrow(id_df)>0) {
        sel1 = id_df$kingdom=="Plantae" & id_df$matchtype=="EXACT"
        if(sum(sel1)>0) {
          gbif_id<-id_df$usagekey[sel1]
        }
      }
      if(length(gbif_id)>0) {
        cdf<-taxize::classification(gbif_id[1], db="gbif")[[1]]
        if(inherits(cdf,"data.frame")) {
          if("family" %in% cdf$rank) SpParams$Family[i] = cdf$name[cdf$rank=="family"]
          if("order" %in% cdf$rank) {
            SpParams$Order[i] = cdf$name[cdf$rank=="order"]
            if(SpParams$Order[i] %in% c("Ginkgoales", "Pinales", "Welwitschiales", "Ephedrales")) {
              SpParams$Group[i] = "Gymnosperm"
            } else {
              SpParams$Group[i] = "Angiosperm"
            }
          }
        } else {
          warning(paste0("Taxonomy could not be retrieved for taxon '", SpParams$AcceptedName[i],"'"))
        }
      } else {
        warning(paste0("Taxon '", SpParams$AcceptedName[i],"' could not be identified"))
      }
    }
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
      species <- species[!endsWith(species, "×")]
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
  }
  if(verbose) cli::cli_progress_step("Finalizing")
  if(sort) SpParams<- SpParams[order(SpParams$Name),, drop = FALSE]
  row.names(SpParams) <- NULL
  SpParams$SpIndex <- 0:(nrow(SpParams)-1)
  if(verbose) cli::cli_process_done()
  return(SpParams)
}

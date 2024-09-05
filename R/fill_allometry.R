
#'
#' @param coef_mapping A named string vector specifying which data column should used to populate each medfate param. Elements are data base columns and names are medfate params.
#' @param sp_params_allom A data table of species allometric coefficients (typically from package medfuels)
#' @param group_params_allom A data table of group allometric coefficients (typically from package medfuels)
#' @param species_groups A data table specifying raunkiaer forms for many species (typically from package medfuels)
#'
#' @keywords internal
populate_shrub_allometries_from_medfuels<-function(SpParams,
                                               coef_mapping,
                                               sp_params_allom, group_params_allom,
                                               species_groups) {
  parnames = names(coef_mapping)
  sp_allom = row.names(sp_params_allom)
  gr_allom = row.names(group_params_allom)
  gen_sp = paste(species_groups$Genus, species_groups$Species)
  nshrub <- 0
  nmis <- 0
  nsp <- 0
  ngr <- 0
  ngen <- 0
  for(i in 1:nrow(SpParams)) {
    growth_form <- SpParams$GrowthForm[i]
    nm <- SpParams$AcceptedName[i]
    if(nm %in% sp_allom) { # If species-specific allometry available
      sp_row<-which(sp_allom==nm)
      if(length(sp_row)==1) {
        nsp <- nsp + 1
        for(j in 1:length(parnames)) {
          SpParams[i, parnames[j]] = sp_params_allom[sp_row, coef_mapping[[j]]]
        }
      }
    } else if (nm %in% gen_sp) { # If species is found in medfuels species table
      form_raunkiaer <-  species_groups$`shrub type`[which(gen_sp==nm)[1]]
      gr_row<-which(gr_allom==form_raunkiaer)
      if(length(gr_row)==1) {
        ngr <- ngr + 1
        for(j in 1:length(parnames)) {
          SpParams[i, parnames[j]]= group_params_allom[gr_row, coef_mapping[[j]]]
        }
      }
    } else if (nm %in% species_groups$Genus) { # If name is a genus and occurs in medfuels table
      forms_raunkiaer <-  species_groups$`shrub type`[which(species_groups$Genus==nm)]
      # Find most-frequent  raunkiaer form
      tfr<-table(forms_raunkiaer)
      tfr<-tfr[order(tfr, decreasing=TRUE)]
      gr_row<-which(gr_allom==names(tfr)[1])
      if(length(gr_row)==1) {
        ngen <- ngen + 1
        for(j in 1:length(parnames)) {
          SpParams[i, parnames[j]]= group_params_allom[gr_row, coef_mapping[[j]]]
        }
      }
    } else {
      nmis <- nmis + 1
    }
  }

  for(j in 1:length(parnames)) {
    message(paste0("Assignments by species: ", nsp, " by raunkiaer group (species): ", ngr ," by raunkiaer (genus): ", ngen))
    nmis <- sum(is.na(SpParams[,parnames[j]]))
    if(nmis>0) message(paste0("'",parnames[j], "' has ", nmis,
                              " missing trait values (",
                              round(100*nmis/nrow(SpParams),1),
                              " % of ", nrow(SpParams), " species) after populating with input data.\n"))
  }
  return(SpParams)

}



#' Populate tree species allometries
#'
#' Internal functions to populates allometric coefficients for tree species and genus of an input parameter table on the basis of their
#' accepted name.
#'
#' @param SpParams A data frame of medfate species parameters to be populated
#' @param allom_table A data frame of allometric parameters in columns and taxonomic entities (species or genus) as row names.
#' @param allom_type A string with the type of allometry to be filled, either "foliarbiomass", "barkthickness", "crownwidth" or "crownratio".
#' @param progress A boolean flag to prompt progress.
#' @param verbose A boolean flag to indicate extra console output.
#'
#' @return A modified data frame of medfate species parameters
#' @export
#'
#' @name fill_medfate_allometries
#' @encoding UTF-8
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#' @seealso \code{\link{init_spparams}}
#'
fill_medfate_allometries<-function(SpParams,
                                   harmonized_allometry_path,
                                   priorization = TRUE,
                                   erase_previous = TRUE,
                                   replace_previous = TRUE,
                                   progress = TRUE, verbose = FALSE) {

  priority_column <- NULL
  if(priorization) priority_column <- "Priority"
  for(response in c("FoliarBiomass", "BarkThickness", "CrownRatio", "CrownWidth")) {

  }

}

fill_medfate_allometries<-function(SpParams,
                                   allom_table,
                                   allom_type = "foliarbiomass",
                                   progress = TRUE, verbose = FALSE) {
  if(!inherits(SpParams, "data.frame")) cli::cli_abort("SpParams should be a species parameter data frame")
  if(!inherits(allom_table, "data.frame")) cli::cli_abort("allom_table should be a data frame")
  allom_type <- match.arg(allom_type, c("foliarbiomass", "barkthickness","crownratio","crownwidth"))
  if(allom_type=="foliarbiomass") {
    allom_vars <- c("a_fbt","b_fbt","c_fbt")
  } else if(allom_type=="barkthickness") {
    allom_vars <- c("a_bt","b_bt")
  } else if(allom_type=="crownratio") {
    allom_vars <- c("a_cr","b_1cr","b_2cr","b_3cr","c_1cr","c_2cr")
  } else if(allom_type=="crownwidth") {
    allom_vars <- c("a_cw","b_cw")
  }
  if(sum(names(allom_table) %in% allom_vars)!=length(allom_vars)) cli::cli_abort("Allometry table should contain all requested vars!")

  allom_names <- row.names(allom_table)
  for(i in 1:nrow(SpParams)) {
    nm = SpParams$AcceptedName[i]
    ## Find species
    allom_row <- NA
    found <- FALSE
    if(nm %in% allom_names) { # Species level
      allom_row <- which(allom_names==nm)
      found <- TRUE
    }
    if(found) {
      SpParams[i, allom_vars] <- allom_table[allom_row, allom_vars]
    }
  }
  return(SpParams)
}


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


.fill_allometry_table<-function(SpParams,
                                allom_table,
                                allom_vars,
                                target_params) {
  for(i in 1:nrow(SpParams)) {
    nm = SpParams$AcceptedName[i]
    ## Find species
    allom_row <- NA
    found <- FALSE
    if(nm %in% allom_table$acceptedName) { # Species level
      allom_row <- which(allom_table$acceptedName==nm)
      found <- TRUE
    }
    if(found) {
      SpParams[i, target_params] <- allom_table[allom_row[1], allom_vars]
    }
  }
  return(SpParams)
}

#' Fill plant allometries
#'
#' Function to populate allometric coefficients for taxa of an input parameter table on the basis of their
#' accepted name.
#'
#' @param SpParams A data frame of medfate species parameters to be populated
#' @param harmonized_trait_path The path to harmonized trait data files (.rds or .csv format).
#' @param priorization A boolean flag to perform priorization of some data sources over others.
#' @param replace_previous A boolean flag to indicate that non-missing previous values should be replaced with new data
#' @param erase_previous A boolean flag to indicate that all previous values should be set to NA before populating with new data
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
  if(progress) cli::cli_progress_step("Processing response: FoliarBiomass")
  response_data <- get_allometry_data(harmonized_allometry_path, response = "FoliarBiomass", progress = FALSE)
  allom_table <-response_data |>
    dplyr::filter(Equation == "FoliarBiomass = a·DBH^b·exp(c·BAL)·DBH^(d·BAL)")
  if(nrow(allom_table) > 0) SpParams <- .fill_allometry_table(SpParams, allom_table, c("a", "b", "c"), c("a_fbt", "b_fbt", "c_fbt"))
  allom_table <-response_data |>
    dplyr::filter(Equation == "FoliarBiomass = a·DBH^b·exp(c·BAL)")
  if(nrow(allom_table) > 0) SpParams <- .fill_allometry_table(SpParams, allom_table, c("a", "b", "c"), c("a_fbt", "b_fbt", "c_fbt"))
  allom_table <-response_data |>
    dplyr::filter(Equation == "FoliarBiomass = a·DBH^b")
  if(nrow(allom_table) > 0) SpParams <- .fill_allometry_table(SpParams, allom_table, c("a", "b"), c("a_fbt", "b_fbt"))

  if(progress) cli::cli_progress_step("Processing response: CrownRatio")
  response_data <- get_allometry_data(harmonized_allometry_path, response = "CrownRatio", progress = FALSE)
  allom_table <-response_data |>
    dplyr::filter(Equation == "CrownRatio = 1/(1 + exp(a + b·HD + c·(H/100) + d·DBH^2 + e·BAL + f·ln(CCF)))")
  if(nrow(allom_table) > 0) SpParams <- .fill_allometry_table(SpParams, allom_table,
                                                              c("a", "b", "c", "d", "e", "f"),
                                                              c("a_cr", "b_1cr", "b_2cr", "b_3cr", "c_1cr", "c_2cr"))

  if(progress) cli::cli_progress_step("Processing response: CrownWidth")
  response_data <- get_allometry_data(harmonized_allometry_path, response = "CrownWidth", progress = FALSE)
  allom_table <-response_data |>
    dplyr::filter(Equation == "CrownWidth = a·DBH^b")
  if(nrow(allom_table) > 0) SpParams <- .fill_allometry_table(SpParams, allom_table,
                                                              c("a", "b"),
                                                              c("a_cw", "b_cw"))

  if(progress) cli::cli_progress_step("Processing response: BarkThickness")
  response_data <- get_allometry_data(harmonized_allometry_path, response = "BarkThickness", progress = FALSE)
  allom_table <-response_data |>
    dplyr::filter(Equation == "BarkThickness = a·DBH^b")
  if(nrow(allom_table) > 0) SpParams <- .fill_allometry_table(SpParams, allom_table,
                                                              c("a", "b"),
                                                              c("a_bt", "b_bt"))


  if(progress) cli::cli_progress_step("Processing response: CrownArea")
  response_data <- get_allometry_data(harmonized_allometry_path, response = "CrownArea", progress = FALSE)
  allom_table <-response_data |>
    dplyr::filter(Equation == "CrownArea = a·Ht^b")
  if(nrow(allom_table) > 0) SpParams <- .fill_allometry_table(SpParams, allom_table,
                                                              c("a", "b"),
                                                              c("a_ash", "b_ash"))


  if(progress) cli::cli_progress_step("Processing response: FineFuelBiomass")
  response_data <- get_allometry_data(harmonized_allometry_path, response = "FineFuelBiomass", progress = FALSE)
  allom_table <-response_data |>
    dplyr::filter(Equation == "FineFuelBiomass = a·PHV^b")
  if(nrow(allom_table) > 0) SpParams <- .fill_allometry_table(SpParams, allom_table,
                                                              c("a", "b"),
                                                              c("a_bsh", "b_bsh"))

  if(progress) cli::cli_progress_step("Processing response: TotalBiomass")
  response_data <- get_allometry_data(harmonized_allometry_path, response = "TotalBiomass", progress = FALSE)
  allom_table <-response_data |>
    dplyr::filter(Equation == "TotalBiomass = a·PHV^b")
  if(nrow(allom_table) > 0) SpParams <- .fill_allometry_table(SpParams, allom_table,
                                                              c("a", "b"),
                                                              c("a_btsh", "b_btsh"))

  if(progress) cli::cli_process_done()
  return(SpParams)
}



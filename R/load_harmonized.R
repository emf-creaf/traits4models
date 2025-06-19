#' Load harmonized trait/allometry data
#'
#' Functions to load harmonized trait data and harmonized allometry data
#'
#' @param harmonized_trait_path The path to harmonized trait data files (.rds or .csv format).
#' @param harmonized_allometry_path The path to harmonized allometry data files (.rds or .csv format).
#' @param check A boolean flag to check harmonization and exclude non-acceptable data.
#' @param progress A boolean flag to prompt progress.
#'
#' @return
#' Function \code{load_harmonized_trait_tables()} returns the list with all harmonized trait data tables.
#'
#' Function \code{load_harmonized_allometry_tables()} returns the list with all harmonized allometry data tables.
#'
#' Function \code{get_trait_data()} returns a data frame with the pooled information of a single trait.
#'
#' Function \code{get_taxon_data()} returns a data frame with the pooled information for a given taxon.
#'
#' @details
#' Both functions will add \code{Priority = 1} to those trait data sources where \code{Priority} column is not defined.
#'
#' @export
#'
#' @name get_trait_data
#' @examples
#' \dontrun{
#'  # List harmonized trait files
#'  DB_path <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/traits_and_models/"
#'  harmonized_trait_path <- paste0(DB_path,"data/harmonized_trait_sources")
#'  list.files(path = harmonized_trait_path)
#'
#'  # Load all harmonized trait data
#'  l <- load_harmonized_trait_tables(harmonized_trait_path)
#'  head(l[[1]])
#'
#'  # Get data for one specific trait
#'  get_trait_data(harmonized_trait_path, "SLA")
#'
#'  # Get trait data for one specific taxon
#'  get_taxon_data(harmonized_trait_path, "Pinus halepensis")
#'
#'  # List harmonized allometry files
#'  harmonized_allometry_path = "~/OneDrive/EMF_datasets/AllometryDatabases/Products/harmonized"
#'  list.files(path = harmonized_allometry_path)
#'
#'  # Load all harmonized allometry data
#'  l <- load_harmonized_allometry_tables(harmonized_allometry_path)
#'  head(l[[1]])
#'
#'  # Get allometry data for one specific response
#'  get_allometry_data(harmonized_allometry_path, "FoliarBiomass")
#'}
load_harmonized_trait_tables <- function(harmonized_trait_path, check = TRUE, progress = TRUE) {
  trait_files_short <- list.files(path = harmonized_trait_path, full.names = FALSE)
  trait_files <- list.files(path = harmonized_trait_path, full.names = TRUE)
  filter <- endsWith(trait_files_short, ".csv") | endsWith(trait_files_short, ".rds")
  if(check) {
    filter <- filter & check_harmonized_trait_dir(harmonized_trait_path, verbose = FALSE)
  }
  trait_files_short <- trait_files_short[filter]
  trait_files <- trait_files[filter]
  trait_tables <- vector("list", length(trait_files))
  if(progress) cli::cli_progress_bar("Tables", total = length(trait_files))
  for(i in 1:length(trait_files)) {
    if(progress) cli::cli_progress_update()
    if(endsWith(trait_files[i], ".rds")) {
      tab <- readRDS(trait_files[i]) |>
        as.data.frame()|>
        dplyr::filter(!is.na(.data$acceptedName))
    } else if(endsWith(trait_files[i], ".csv")) {
      tab <- read.csv2(trait_files[i]) |>
        as.data.frame()|>
        dplyr::filter(!is.na(.data$acceptedName))
    }
    if(!("DOI" %in% names(tab))) tab$DOI <- as.character(NA)
    if(!("OriginalReference" %in% names(tab))) tab$OriginalReference <- as.character(NA)
    if(!("OriginalDOI" %in% names(tab))) tab$OriginalDOI <- as.character(NA)
    if(!("Priority" %in% names(tab))) tab$Priority <- 1
    trait_tables[[i]] <- tab
  }
  names(trait_tables) <- trait_files_short
  return(trait_tables)
}

#' @rdname get_trait_data
#' @export
load_harmonized_allometry_tables <- function(harmonized_allometry_path, check = TRUE, progress = TRUE) {
  allometry_files_short <- list.files(path = harmonized_allometry_path, full.names = FALSE)
  allometry_files <- list.files(path = harmonized_allometry_path, full.names = TRUE)
  filter <- endsWith(allometry_files_short, ".csv") | endsWith(allometry_files_short, ".rds")
  if(check) {
    filter <- filter & check_harmonized_allometry_dir(harmonized_allometry_path, verbose = FALSE)
  }
  allometry_files_short <- allometry_files_short[filter]
  allometry_files <- allometry_files[filter]
  allometry_tables <- vector("list", length(allometry_files))
  if(progress) cli::cli_progress_bar("Tables", total = length(allometry_files))
  for(i in 1:length(allometry_files)) {
    if(progress) cli::cli_progress_update()
    if(endsWith(allometry_files[i], ".rds")) {
      tab <- readRDS(allometry_files[i]) |>
        as.data.frame()|>
        dplyr::filter(!is.na(.data$acceptedName))
    } else if(endsWith(allometry_files[i], ".csv")) {
      tab <- read.csv2(allometry_files[i]) |>
        as.data.frame()|>
        dplyr::filter(!is.na(.data$acceptedName))
    }
    if(!("Priority" %in% names(tab))) tab$Priority <- 1
    allometry_tables[[i]] <- tab
  }
  names(allometry_tables) <- allometry_files_short
  return(allometry_tables)
}

#' @param trait_name A string of an accepted trait name
#' @param output_format
#'
#' @export
#'
#' @rdname get_trait_data
get_trait_data <- function(harmonized_trait_path,
                           trait_name, output_format = "long",
                           is_numeric = TRUE, check = TRUE, progress = TRUE) {
  output_format <- match.arg(output_format, c("wide", "long"))
  if(progress) cli::cli_progress_step("Loading harmonized source trait tables")
  all_tables <- load_harmonized_trait_tables(harmonized_trait_path, check = check, progress = progress)
  if(progress) cli::cli_progress_step(paste0("Filtering for trait: ", trait_name))
  trait_tables <- vector("list", length(all_tables))
  fixed <- c("originalName", "acceptedName","acceptedNameAuthorship","family",
             "genus", "specificEpithet","taxonRank", "Units", "Reference", "DOI", "OriginalReference", "OriginalDOI", "Priority")
  n_tab <- 0
  for(i in 1:length(all_tables)) {
    tab <- all_tables[[i]]
    cn <- names(tab)
    format <- "undefined"
    if(all(c("Trait", "Value", "Units") %in% cn)) {
      format <- "long"
    } else if(all(!(c("Trait", "Value", "Units") %in% cn))) {
      format <- "wide"
    } else {
      acceptable <- FALSE
      cli::cli_alert_warning("Trait data should be in either long or wide format (see documentation)")
    }
    if(format=="wide") {
      if(trait_name %in% names(tab)) {
        tab <- tab[!is.na(tab[[trait_name]]), ,drop =FALSE]
        n_tab <- n_tab + 1
        trait_tables[[i]] <- tab[,names(tab)[names(tab) %in% c(fixed, trait_name)]]
      }
    }

  }
  if(n_tab>0) {
    trait_table <- dplyr::bind_rows(trait_tables) |>
      dplyr::arrange(.data$acceptedName)
    trait_table <- trait_table[!is.na(trait_table[[trait_name]]), , drop = FALSE]
  } else {
    stop(paste0("Trait data not found for: ", trait_name))
  }
  if(progress) cli::cli_progress_done()
  return(trait_table)
}

#' @export
#' @param response String indicating a response variable for allometric equations.
#' @rdname get_trait_data
get_allometry_data <-function(harmonized_allometry_path,
                              response, check = TRUE, progress = TRUE) {
  if(progress) cli::cli_progress_step("Loading harmonized source allometry tables")
  allom_tables <- load_harmonized_allometry_tables(harmonized_allometry_path, check = check, progress = progress)
  if(progress) cli::cli_progress_step(paste0("Filtering for allometry: ", response))
  response_tables <- vector("list", length(allom_tables))
  for(i in 1:length(allom_tables)) {
    tab <- allom_tables[[i]]
    tab <- tab[tab$Response == response, , drop =FALSE]
    if(nrow(tab)>0) response_tables[[i]] <- tab
  }
  if(progress) cli::cli_progress_done()
  allometry_response_table <- dplyr::bind_rows(response_tables)
  if(nrow(allometry_response_table)==0) {
    cli::cli_inform(paste0("Allometry data not found for: ", response))
  }
  return(allometry_response_table)
}

#' @export
#' @param accepted_name String of an accepted taxon name.
#' @rdname get_trait_data
get_taxon_data<- function(harmonized_trait_path,
                          accepted_name, output_format = "long", progress = TRUE) {
  output_format <- match.arg(output_format, c("wide", "long"))
  if(progress) cli::cli_progress_step("Loading harmonized source trait tables")
  all_tables <- load_harmonized_trait_tables(harmonized_trait_path, progress = progress)
  if(progress) cli::cli_progress_step(paste0("Filtering for taxon: ", accepted_name))
  taxon_table <- data.frame(Trait = character(0), Value = character(0), Units = character(0), Reference = character(0))
  fixed <- c("originalName", "acceptedName","acceptedNameAuthorship","family",
             "genus", "specificEpithet","taxonRank", "Units", "Reference", "DOI", "OriginalReference", "OriginalDOI", "Priority")
  for(i in 1:length(all_tables)) {
    tab <- all_tables[[i]] |>
      dplyr::filter(.data$acceptedName == accepted_name)
    if(nrow(tab)>0) {
      traits <- names(tab)
      traits <- traits[!(traits %in% fixed)]
      for(trait_name in traits) {
        if(trait_name %in% HarmonizedTraitDefinition$Notation) {
          df_i <- data.frame(Trait = rep(trait_name, nrow(tab)),
                             Value = as.character(tab[[trait_name]]))
          if("Units" %in% names(tab)) df_i$Units = tab$Units
          else {
            unit <- HarmonizedTraitDefinition$Units[HarmonizedTraitDefinition$Notation==trait_name]
            if(length(unit)==1) df_i$Units <- rep(unit, nrow(tab))
          }
          if("Reference" %in% names(tab)) df_i$Reference <- tab$Reference
          if("DOI" %in% names(tab)) df_i$DOI <- tab$DOI
          if("OriginalReference" %in% names(tab)) df_i$OriginalReference <- tab$OriginalReference
          if("OriginalDOI" %in% names(tab)) df_i$OriginalDOI <- tab$OriginalDOI
          if("Priority" %in% names(tab)) df_i$Priority <- tab$Priority
          df_i <- df_i[!is.na(df_i$Value), , drop = FALSE]
          taxon_table <- dplyr::bind_rows(taxon_table, df_i)
        }
      }
    }
  }
  if(progress) cli::cli_progress_step(paste0("Reordering"))
  odf <- data.frame(Trait = HarmonizedTraitDefinition$Notation,
                    Order = 1:nrow(HarmonizedTraitDefinition))
  taxon_table <- taxon_table |>
    dplyr::left_join(odf, by="Trait") |>
    dplyr::arrange(.data$Order) |>
    dplyr::select(-.data$Order) |>
    unique()

  if(progress) cli::cli_progress_done()
  return(taxon_table)
}

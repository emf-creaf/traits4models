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
#' Function \code{get_trait_data()} returns a data frame (in long trait format) with the pooled information for a given trait.
#'
#' Function \code{get_taxon_data()} returns a data frame (in long trait format) with the pooled information for a given taxon.
#'
#' @details
#' Function \code{load_harmonized_trait_tables()} allows loading all kinds of trait data tables, including those that do not pass harmonization check, if \code{check = FALSE}.
#' In contrast, functions \code{get_trait_data()} and \code{get_taxon_data()} only return data from data sets passing harmonization checks (see function \code{\link{check_harmonized_trait}}).
#'
#' @seealso \code{\link{check_harmonized_trait}}
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
  trait_files_short <- trait_files_short[filter]
  trait_files <- trait_files[filter]
  trait_tables <- vector("list", length(trait_files))
  accepted <- rep(TRUE, length(trait_files))
  if(progress) cli::cli_progress_bar("Loading tables", total = length(trait_files))
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
    if(check) {
      accepted[i] <- check_harmonized_trait(tab, verbose = FALSE)
      if(!accepted[i]) cli::cli_alert_warning(paste0("File ", trait_files_short[i], " is not acceptable."))
    }
    if(accepted[i]) trait_tables[[i]] <- tab
  }
  names(trait_tables) <- trait_files_short
  trait_tables <- trait_tables[accepted]
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

#' @param trait_name A string of an accepted trait name, according to \code{\link{HarmonizedTraitDefinition}}.
#' @param output_format Either "long" or "wide", to indicate output format for trait columns.
#'
#' @export
#'
#' @rdname get_trait_data
get_trait_data <- function(harmonized_trait_path,
                           trait_name,
                           output_format = "long",
                           progress = TRUE) {
  output_format <- match.arg(output_format, c("long", "wide"))
  trait_name <- match.arg(trait_name, traits4models::HarmonizedTraitDefinition$Notation)
  all_tables <- load_harmonized_trait_tables(harmonized_trait_path, check = TRUE, progress = progress)
  if(progress) cli::cli_progress_bar(paste0("Filtering for trait '", trait_name,"'"), total = length(all_tables))
  trait_tables <- vector("list", length(all_tables))
  fixed <- c("originalName", "acceptedName","acceptedNameAuthorship","family",
             "genus", "specificEpithet","taxonRank", "Reference", "DOI",
             "OriginalReference", "OriginalDOI", "Priority", "checkVersion")
  n_tab <- 0
  row <- which(traits4models::HarmonizedTraitDefinition$Notation==trait_name)
  expected_type <- traits4models::HarmonizedTraitDefinition$Type[row]
  expected_unit <- traits4models::HarmonizedTraitDefinition$Units[row]
  for(i in 1:length(all_tables)) {
    if(progress) cli::cli_progress_update()
    tab <- all_tables[[i]]
    cn <- names(tab)
    format <- "undefined"
    if(all(c("Trait", "Value", "Units") %in% cn)) {
      format <- "long"
    } else if(all(!(c("Trait", "Value", "Units") %in% cn))) {
      format <- "wide"
    } else {
      cli::cli_abort("Trait data should be in either long or wide format (see documentation)")
    }
    if(format=="wide") {
      if(trait_name %in% names(tab)) {
        tab <- tab[!is.na(tab[[trait_name]]), ,drop =FALSE]
        tab <- tab[,names(tab)[names(tab) %in% c(fixed, trait_name)]]
        tab <- tab |>
          tidyr::pivot_longer(trait_name, names_to = "Trait", values_to="Value") |>
          dplyr::mutate(Units = expected_unit) |>
          dplyr::relocate("Trait", "Value", "Units", .before = "Reference")
        if("checkVersion" %in% names(tab)) {
          tab[["checkVersion"]] <-  as.character(tab[["checkVersion"]])
        }
        trait_tables[[i]] <- tab
        n_tab <- n_tab + 1
      }
    } else if(format == "long") {
      tab <- tab[tab$Trait==trait_name, ,drop =FALSE]
      if(nrow(tab)>0) {
        if("checkVersion" %in% names(tab)) {
          tab[["checkVersion"]] <-  as.character(tab[["checkVersion"]])
        }
        trait_tables[[i]] <- tab
        n_tab <- n_tab + 1
      }
    }
  }
  if(n_tab>0) {
    trait_table <- dplyr::bind_rows(trait_tables) |>
      dplyr::arrange(.data$acceptedName)
  } else {
    stop(paste0("Trait data not found for: ", trait_name))
  }
  if(output_format =="wide") {
    trait_table <- trait_table |>
      dplyr::select(-c("Units", "Trait"))
    names(trait_table)[names(trait_table)=="Value"] <- trait_name
  }
  if(progress) cli::cli_progress_done()
  return(trait_table)
}

#' @export
#' @param response String indicating a response variable for allometric equations.
#' @rdname get_trait_data
get_allometry_data <-function(harmonized_allometry_path,
                              response, progress = TRUE) {
  if(progress) cli::cli_progress_step("Loading harmonized source allometry tables")
  allom_tables <- load_harmonized_allometry_tables(harmonized_allometry_path, check = TRUE, progress = progress)
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
                          accepted_name, progress = TRUE) {
  all_tables <- load_harmonized_trait_tables(harmonized_trait_path, check = TRUE, progress = progress)
  if(progress) cli::cli_progress_bar(paste0("Filtering for taxon '", accepted_name,"'"), total = length(all_tables))
  taxon_table <- data.frame()
  fixed <- c("originalName", "acceptedName","acceptedNameAuthorship","family",
             "genus", "specificEpithet","taxonRank", "Reference", "DOI",
             "OriginalReference", "OriginalDOI", "Priority", "checkVersion")
  for(i in 1:length(all_tables)) {
    if(progress) cli::cli_progress_update()
    tab <- all_tables[[i]] |>
      dplyr::filter(.data$acceptedName == accepted_name)
    if("checkVersion" %in% names(tab)) {
      tab[["checkVersion"]] <-  as.character(tab[["checkVersion"]])
    }
    if(nrow(tab)>0) {
      cn <- names(tab)
      format <- "undefined"
      if(all(c("Trait", "Value", "Units") %in% cn)) {
        format <- "long"
      } else if(all(!(c("Trait", "Value", "Units") %in% cn))) {
        format <- "wide"
      } else {
        cli::cli_abort("Trait data should be in either long or wide format (see documentation)")
      }
      if(format=="wide") {
        tab <- tab |>
          tidyr::pivot_longer(cn[!(cn %in% fixed)], names_to = "Trait", values_to="Value",
                              values_transform = as.character) |>
          dplyr::mutate(Units = as.character(NA)) |>
          dplyr::relocate("Trait", "Value", "Units", .after="taxonRank")
      } else {
        tab$Value <- as.character(tab$Value)
      }
      taxon_table <- dplyr::bind_rows(taxon_table, tab)
    }
  }
  odf <- data.frame(Trait = traits4models::HarmonizedTraitDefinition$Notation,
                    Order = 1:nrow(traits4models::HarmonizedTraitDefinition))
  taxon_table <- taxon_table |>
    dplyr::left_join(odf, by="Trait") |>
    dplyr::arrange(.data$Order) |>
    dplyr::select(-.data$Order) |>
    unique()

  if(progress) cli::cli_progress_done()
  return(taxon_table)
}

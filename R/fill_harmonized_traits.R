#' Load harmonized trait data
#'
#' Functions to load harmonized trait data
#'
#' @param harmonized_trait_path The path to harmonized trait data files (.rds)
#' @param progress A boolean flag to prompt progress.
#'
#' @return
#' Function \code{harmonized_table_list()} returns the list with all harmonized trait data tables.
#'
#' Function \code{get_trait_data()} returns a data table with the pooled information of a single trait.
#'
#' @export
#'
#' @name get_trait_data
#' @examples
#' \dontrun{
#'  harmonized_trait_path = "~/OneDrive/EMF_datasets/PlantTraitDatabases/Products/harmonized"
#'
#'  # List of files
#'  trait_files <- list.files(path = harmonized_trait_path)
#'  head(trait_files)
#'
#'  # Load trait data
#'  l <- load_harmonized_tables(harmonized_trait_path)
#'  head(l[[1]])
#'
#'  get_trait_data(harmonized_trait_path, "SLA")
#'}
load_harmonized_tables <- function(harmonized_trait_path, progress = TRUE) {
  trait_files_short <- list.files(path = harmonized_trait_path, full.names = FALSE)
  trait_files <- list.files(path = harmonized_trait_path, full.names = TRUE)
  trait_tables <- vector("list", length(trait_files))

  if(progress) cli::cli_progress_bar("Tables", total = length(trait_files))
  for(i in 1:length(trait_files)) {
    if(progress) cli::cli_progress_update()
    tab <- readRDS(trait_files[[i]]) |>
      as.data.frame()|>
      dplyr::filter(!is.na(acceptedName))
    trait_tables[[i]] <- tab
  }
  names(trait_tables) <- trait_files_short
  return(trait_tables)
}

#' @param trait_name A string of an accepted trait name
#' @param is_numeric A boolean indicating whether the trait is numeric
#'
#' @export
#'
#' @rdname get_trait_data
get_trait_data <- function(harmonized_trait_path,
                           trait_name,
                           is_numeric = TRUE, progress = TRUE) {
  if(progress) cli::cli_progress_step("Loading harmonized source trait tables")
  all_tables <- load_harmonized_tables(harmonized_trait_path, progress = progress)
  if(progress) cli::cli_progress_step(paste0("Filtering for trait: ", trait_name))
  trait_tables <- vector("list", length(all_tables))
  fixed <- c("originalName", "acceptedName","acceptedNameAuthorship","family",
             "genus", "specificEpithet","taxonRank", "Units", "Reference")
  n_tab <- 0
  for(i in 1:length(all_tables)) {
    tab <- all_tables[[i]]
    if(trait_name %in% names(tab)) {
      tab <- tab[!is.na(tab[[trait_name]]), ,drop =FALSE]
      if(is_numeric) tab[[trait_name]] <- as.numeric(tab[[trait_name]])
      else tab[[trait_name]] <- as.character(tab[[trait_name]])
      n_tab <- n_tab + 1
      trait_tables[[i]] <- tab[,names(tab)[names(tab) %in% c(fixed, trait_name)]]
    }
  }
  if(n_tab>0) {
    trait_table <- dplyr::bind_rows(trait_tables) |>
      dplyr::arrange(acceptedName)
    trait_table <- trait_table[!is.na(trait_table[[trait_name]]), , drop = FALSE]
  } else {
    stop(paste0("Trait data not found for: ", trait_name))
  }
  if(progress) cli::cli_progress_done()
  return(trait_table)
}

#' Trait filling from harmonized data
#'
#' Fills species parameter table with trait data
#'
#' @param SpParams A species parameter data frame to be filled.
#' @param harmonized_trait_path A directory path were RDS files with harmonized trait databases are.
#' @param progress A boolean flag to prompt progress.
#' @param verbose A boolean flag to prompt detailed process information.
#'
#' @return A modified species parameter data frame
#' @export
#'
#' @name fill_harmonized_traits
#' @examples
fill_harmonized_traits<-function(SpParams,
                                 harmonized_trait_path,
                                 progress = TRUE, verbose = FALSE) {

  for(trait_name in c("GrowthForm","LifeForm","LeafShape","PhenologyType", "DispersalType")) {
    if(progress) cli::cli_progress_step(paste0("Processing parameter: ", trait_name))
    trait_table <- get_trait_data(harmonized_trait_path, trait_name, is_numeric = FALSE, progress = FALSE)
    if(nrow(trait_table)>0) {
      trait_mapping <- trait_name
      names(trait_mapping) <- trait_name
      SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                  taxon_column = "acceptedName", genus_column = "genus",
                                  erase_previous = TRUE, character_traits = TRUE,
                                  replace_previous = TRUE, verbose = verbose)
    }
  }
  for(trait_name in c("t0gdd", "Tbgdd", "Sgdd",
                      "Phsen", "Tbsen", "xsen", "ysen", "Ssen")) {
    if(progress) cli::cli_progress_step(paste0("Processing parameter: ", trait_name))
    trait_table <- get_trait_data(harmonized_trait_path, trait_name, is_numeric = TRUE, progress = FALSE)
    if(nrow(trait_table)>0) {
      trait_mapping <- trait_name
      names(trait_mapping) <- trait_name
      SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                               summary_function = "median",
                                               taxon_column = "acceptedName", genus_column = "genus",
                                               erase_previous = TRUE, character_traits = FALSE,
                                               replace_previous = TRUE, verbose = verbose)
    }
  }

  if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "LeafSize"))
  trait_table <- get_trait_data(harmonized_trait_path, "LeafArea", is_numeric = TRUE, progress = FALSE) |>
    dplyr::mutate(
      LeafSize = dplyr::case_when(
        LeafArea<225 ~"Small",
        LeafArea<2025 & LeafArea>=225 ~ "Medium",
        LeafArea>=2025 & LeafArea<100000 ~ "Large"
      ))
  trait_mapping <- "LeafSize"
  names(trait_mapping) <- "LeafSize"
  SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = TRUE,
                                           replace_previous = TRUE, verbose = verbose)

  if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "Hmax/Hmed"))
  trait_table <- get_trait_data(harmonized_trait_path, "Hact", is_numeric = TRUE, progress = FALSE)
  trait_mapping <- "Hact"
  names(trait_mapping) <- "Hmax"
  SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                           summary_function = "max",
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = FALSE,
                                           replace_previous = TRUE, verbose = verbose)
  trait_mapping <- "Hact"
  names(trait_mapping) <- "Hmed"
  SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                           summary_function = "median",
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = FALSE,
                                           replace_previous = TRUE, verbose = verbose)


  if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "Gs_P50"))
  trait_table <- get_trait_data(harmonized_trait_path, "Gs_P50", is_numeric = TRUE, progress = FALSE)
  trait_mapping <- "Gs_P50"
  names(trait_mapping) <- "Gs_P50"
  SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                           summary_function = "median",
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = FALSE,
                                           replace_previous = TRUE, verbose = verbose)

  if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "LFMC"))
  trait_table <- get_trait_data(harmonized_trait_path, "LFMC", is_numeric = TRUE, progress = FALSE)
  trait_mapping <- "LFMC"
  names(trait_mapping) <- "maxFMC"
  SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                           summary_function = "quantile", summary_params = list("prob" = 0.95),
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = FALSE,
                                           replace_previous = TRUE, verbose = verbose)
  trait_mapping <- "LFMC"
  names(trait_mapping) <- "minFMC"
  SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                           summary_function = "quantile", summary_params = list("prob" = 0.05),
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = FALSE,
                                           replace_previous = TRUE, verbose = verbose)

  if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "Kmax_stemxylem"))
  trait_table <- get_trait_data(harmonized_trait_path, "Ks", is_numeric = TRUE, progress = FALSE)
  trait_mapping <- "Ks"
  names(trait_mapping) <- "Kmax_stemxylem"
  SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                           summary_function = "median",
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = FALSE,
                                           replace_previous = TRUE, verbose = verbose)

  if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "Vmax298"))
  trait_table <- get_trait_data(harmonized_trait_path, "Vcmax", is_numeric = TRUE, progress = FALSE)
  trait_mapping <- "Vcmax"
  names(trait_mapping) <- "Vmax298"
  SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                           summary_function = "median",
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = FALSE,
                                           replace_previous = TRUE, verbose = verbose)


  if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "Jmax298"))
  trait_table <- get_trait_data(harmonized_trait_path, "Jmax", is_numeric = TRUE, progress = FALSE)
  trait_mapping <- "Jmax"
  names(trait_mapping) <- "Jmax298"
  SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                           summary_function = "median",
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = FALSE,
                                           replace_previous = TRUE, verbose = verbose)

  if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "Z95"))
  trait_table <- get_trait_data(harmonized_trait_path, "Z95", is_numeric = TRUE, progress = FALSE)
  if(nrow(trait_table)>0) {
    trait_mapping <- "Z95"
    names(trait_mapping) <- "Z95"
    SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                             summary_function = "quantile", summary_params = list("prob" = 0.95),
                                             taxon_column = "acceptedName", genus_column = "genus",
                                             erase_previous = TRUE, character_traits = FALSE,
                                             replace_previous = TRUE, verbose = verbose)
  }
  if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "LeafAngle"))
  trait_table <- get_trait_data(harmonized_trait_path, "LeafAngle", is_numeric = TRUE, progress = FALSE)
  if(nrow(trait_table)>0) {
    trait_mapping <- "LeafAngle"
    names(trait_mapping) <- "LeafAngle"
    SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                             summary_function = "median",
                                             taxon_column = "acceptedName", genus_column = "genus",
                                             erase_previous = TRUE, character_traits = FALSE,
                                             replace_previous = TRUE, verbose = verbose)
    trait_mapping <- "LeafAngle"
    names(trait_mapping) <- "LeafAngleSD"
    SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                             summary_function = "sd",
                                             taxon_column = "acceptedName", genus_column = "genus",
                                             erase_previous = TRUE, character_traits = FALSE,
                                             replace_previous = TRUE, verbose = verbose)
  }

  for(trait_name in c("LeafDensity", "WoodDensity", "SRL" , "r635",
                      "LigninPercent","pDead", "SAV", "HeatContent",
                      "LeafPI0", "LeafEPS", "LeafAF", "SLA", "Al2As", "conduit2sapwood",
                      "LeafWidth", "LeafDuration", "Gswmax", "Gswmin",
                      "VCleaf_P50", "VCleaf_P12", "VCleaf_P88", "VCleaf_slope",
                      "VCstem_P50", "VCstem_P12", "VCstem_P88", "VCstem_slope",
                      "VCroot_P50", "VCroot_P12", "VCroot_P88", "VCroot_slope",
                      "Nleaf","Nsapwood", "Nfineroot","SeedMass", "SeedLongevity",
                      "WoodC", "CCleaf", "CCsapwood", "CCfineroot")) {
    if(progress)  cli::cli_progress_step(paste0("Processing parameter: ", trait_name))
    trait_table <- get_trait_data(harmonized_trait_path, trait_name, is_numeric = TRUE, progress = FALSE)
    if(nrow(trait_table)>0) {
      trait_mapping <- trait_name
      names(trait_mapping) <- trait_name
      SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                               summary_function = "median",
                                               taxon_column = "acceptedName", genus_column = "genus",
                                               erase_previous = TRUE, character_traits = FALSE,
                                               replace_previous = TRUE, verbose = verbose)
    }
  }
  if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "RSSG"))
  trait_table <- get_trait_data(harmonized_trait_path, "ShadeTolerance", is_numeric = TRUE, progress = FALSE)
  trait_mapping <- "ShadeTolerance"
  names(trait_mapping) <- "RSSG"
  SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                           summary_function = "median",
                                           scalar_functions = c("RSSG" = function(x){min(0.95,0.25+0.70*((x-1)/2))}),
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = FALSE,
                                           replace_previous = TRUE, verbose = verbose)
  cli::cli_process_done()

  return(SpParams)
}

load_trait_tables <- function(harmonized_trait_path) {
  trait_files <- list.files(path = harmonized_trait_path, full.names = TRUE)
  trait_tables <- vector("list", length(trait_files))
  for(i in 1:length(trait_files)) {
    tab <- readRDS(trait_files[[i]]) |>
      as.data.frame()|>
      dplyr::filter(!is.na(acceptedName))
    trait_tables[[i]] <- tab
  }
  return(trait_tables)
}

filter_trait_table <- function(all_tables,
                               trait_name,
                               is_numeric = TRUE) {
  trait_tables <- vector("list", length(all_tables))
  n_tab <- 0
  for(i in 1:length(all_tables)) {
    tab <- all_tables[[i]]
    if(trait_name %in% names(tab)) {
      tab <- tab[!is.na(tab[[trait_name]]), ,drop =FALSE]
      if(is_numeric) tab[[trait_name]] <- as.numeric(tab[[trait_name]])
      else tab[[trait_name]] <- as.character(tab[[trait_name]])
      n_tab <- n_tab + 1
      trait_tables[[i]] <- tab[,c("acceptedName", "genus", trait_name)]
    }
  }
  if(n_tab>0) {
    trait_table <- dplyr::bind_rows(trait_tables) |>
      dplyr::arrange(acceptedName)
    trait_table <- trait_table[!is.na(trait_table[[trait_name]]), , drop = FALSE]
  } else {
    stop(paste0("Trait data not found for: ", trait_name))
  }
  return(trait_table)
}

#' Trait filling
#'
#' Fills species parameter table with trait data
#'
#' @param SpParams A species parameter data frame to be filled
#' @param harmonized_trait_path A directory path were RDS files with harmonized trait databases are
#' @param verbose A boolean flag for process information
#'
#' @return A modified species parameter data frame
#' @export
#'
#' @examples
fill_traits<-function(SpParams,
                      harmonized_trait_path,
                      verbose = FALSE) {

  cli::cli_progress_step("Loading harmonized source trait tables")
  trait_tables <- load_trait_tables(harmonized_trait_path)

  for(trait_name in c("GrowthForm","LifeForm","LeafShape","PhenologyType", "DispersalType")) {
    cli::cli_progress_step(paste0("Processing parameter: ", trait_name))
    trait_table <- filter_trait_table(trait_tables, trait_name, is_numeric = FALSE)
    if(nrow(trait_table)>0) {
      trait_mapping <- trait_name
      names(trait_mapping) <- trait_name
      SpParams <- medfateutils::populateTraits(SpParams, trait_table, trait_mapping,
                                               taxon_column = "acceptedName", genus_column = "genus",
                                               erase_previous = TRUE, character_traits = TRUE,
                                               replace_previous = TRUE, verbose = verbose)
    }
  }
  for(trait_name in c("t0gdd", "Tbgdd", "Sgdd",
                      "Phsen", "Tbsen", "xsen", "ysen", "Ssen")) {
    cli::cli_progress_step(paste0("Processing parameter: ", trait_name))
    trait_table <- filter_trait_table(trait_tables, trait_name, is_numeric = TRUE)
    if(nrow(trait_table)>0) {
      trait_mapping <- trait_name
      names(trait_mapping) <- trait_name
      SpParams <- medfateutils::populateTraits(SpParams, trait_table, trait_mapping,
                                               summary_function = "median",
                                               taxon_column = "acceptedName", genus_column = "genus",
                                               erase_previous = TRUE, character_traits = FALSE,
                                               replace_previous = TRUE, verbose = verbose)
    }
  }

  cli::cli_progress_step(paste0("Processing parameter: ", "LeafSize"))
  trait_table <- filter_trait_table(trait_tables, "LeafArea", is_numeric = TRUE) |>
    dplyr::mutate(
      LeafSize = dplyr::case_when(
        LeafArea<225 ~"Small",
        LeafArea<2025 & LeafArea>=225 ~ "Medium",
        LeafArea>=2025 & LeafArea<100000 ~ "Large"
      ))
  trait_mapping <- "LeafSize"
  names(trait_mapping) <- "LeafSize"
  SpParams <- medfateutils::populateTraits(SpParams, trait_table, trait_mapping,
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = TRUE,
                                           replace_previous = TRUE, verbose = verbose)

  cli::cli_progress_step(paste0("Processing parameter: ", "Hmax/Hmed"))
  trait_table <- filter_trait_table(trait_tables, "Hact", is_numeric = TRUE)
  trait_mapping <- "Hact"
  names(trait_mapping) <- "Hmax"
  SpParams <- medfateutils::populateTraits(SpParams, trait_table, trait_mapping,
                                           summary_function = "max",
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = FALSE,
                                           replace_previous = TRUE, verbose = verbose)
  trait_mapping <- "Hact"
  names(trait_mapping) <- "Hmed"
  SpParams <- medfateutils::populateTraits(SpParams, trait_table, trait_mapping,
                                           summary_function = "median",
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = FALSE,
                                           replace_previous = TRUE, verbose = verbose)


  cli::cli_progress_step(paste0("Processing parameter: ", "Gs_P50"))
  trait_table <- filter_trait_table(trait_tables, "Gs_P50", is_numeric = TRUE)
  trait_mapping <- "Gs_P50"
  names(trait_mapping) <- "Gs_P50"
  SpParams <- medfateutils::populateTraits(SpParams, trait_table, trait_mapping,
                                           summary_function = "median",
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = FALSE,
                                           replace_previous = TRUE, verbose = verbose)

  cli::cli_progress_step(paste0("Processing parameter: ", "LFMC"))
  trait_table <- filter_trait_table(trait_tables, "LFMC", is_numeric = TRUE)
  trait_mapping <- "LFMC"
  names(trait_mapping) <- "maxFMC"
  SpParams <- medfateutils::populateTraits(SpParams, trait_table, trait_mapping,
                                           summary_function = "quantile", summary_params = list("prob" = 0.95),
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = FALSE,
                                           replace_previous = TRUE, verbose = verbose)
  trait_mapping <- "LFMC"
  names(trait_mapping) <- "minFMC"
  SpParams <- medfateutils::populateTraits(SpParams, trait_table, trait_mapping,
                                           summary_function = "quantile", summary_params = list("prob" = 0.05),
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = FALSE,
                                           replace_previous = TRUE, verbose = verbose)

  cli::cli_progress_step(paste0("Processing parameter: ", "Kmax_stemxylem"))
  trait_table <- filter_trait_table(trait_tables, "Ks", is_numeric = TRUE)
  trait_mapping <- "Ks"
  names(trait_mapping) <- "Kmax_stemxylem"
  SpParams <- medfateutils::populateTraits(SpParams, trait_table, trait_mapping,
                                           summary_function = "median",
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = FALSE,
                                           replace_previous = TRUE, verbose = verbose)

  cli::cli_progress_step(paste0("Processing parameter: ", "Vmax298"))
  trait_table <- filter_trait_table(trait_tables, "Vcmax", is_numeric = TRUE)
  trait_mapping <- "Vcmax"
  names(trait_mapping) <- "Vmax298"
  SpParams <- medfateutils::populateTraits(SpParams, trait_table, trait_mapping,
                                           summary_function = "median",
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = FALSE,
                                           replace_previous = TRUE, verbose = verbose)


  cli::cli_progress_step(paste0("Processing parameter: ", "Jmax298"))
  trait_table <- filter_trait_table(trait_tables, "Jmax", is_numeric = TRUE)
  trait_mapping <- "Jmax"
  names(trait_mapping) <- "Jmax298"
  SpParams <- medfateutils::populateTraits(SpParams, trait_table, trait_mapping,
                                           summary_function = "median",
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = FALSE,
                                           replace_previous = TRUE, verbose = verbose)

  cli::cli_progress_step(paste0("Processing parameter: ", "Z95"))
  trait_table <- filter_trait_table(trait_tables, "Z95", is_numeric = TRUE)
  if(nrow(trait_table)>0) {
    trait_mapping <- "Z95"
    names(trait_mapping) <- "Z95"
    SpParams <- medfateutils::populateTraits(SpParams, trait_table, trait_mapping,
                                             summary_function = "quantile", summary_params = list("prob" = 0.95),
                                             taxon_column = "acceptedName", genus_column = "genus",
                                             erase_previous = TRUE, character_traits = FALSE,
                                             replace_previous = TRUE, verbose = verbose)
  }
  cli::cli_progress_step(paste0("Processing parameter: ", "LeafAngle"))
  trait_table <- filter_trait_table(trait_tables, "LeafAngle", is_numeric = TRUE)
  if(nrow(trait_table)>0) {
    trait_mapping <- "LeafAngle"
    names(trait_mapping) <- "LeafAngle"
    SpParams <- medfateutils::populateTraits(SpParams, trait_table, trait_mapping,
                                             summary_function = "median",
                                             taxon_column = "acceptedName", genus_column = "genus",
                                             erase_previous = TRUE, character_traits = FALSE,
                                             replace_previous = TRUE, verbose = verbose)
    trait_mapping <- "LeafAngle"
    names(trait_mapping) <- "LeafAngleSD"
    SpParams <- medfateutils::populateTraits(SpParams, trait_table, trait_mapping,
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
    cli::cli_progress_step(paste0("Processing parameter: ", trait_name))
    trait_table <- filter_trait_table(trait_tables, trait_name, is_numeric = TRUE)
    if(nrow(trait_table)>0) {
      trait_mapping <- trait_name
      names(trait_mapping) <- trait_name
      SpParams <- medfateutils::populateTraits(SpParams, trait_table, trait_mapping,
                                               summary_function = "median",
                                               taxon_column = "acceptedName", genus_column = "genus",
                                               erase_previous = TRUE, character_traits = FALSE,
                                               replace_previous = TRUE, verbose = verbose)
    }
  }
  cli::cli_progress_step(paste0("Processing parameter: ", "RSSG"))
  trait_table <- filter_trait_table(trait_tables, "ShadeTolerance", is_numeric = TRUE)
  trait_mapping <- "ShadeTolerance"
  names(trait_mapping) <- "RSSG"
  SpParams <- medfateutils::populateTraits(SpParams, trait_table, trait_mapping,
                                           summary_function = "median",
                                           scalar_functions = c("RSSG" = function(x){min(0.95,0.25+0.70*((x-1)/2))}),
                                           taxon_column = "acceptedName", genus_column = "genus",
                                           erase_previous = TRUE, character_traits = FALSE,
                                           replace_previous = TRUE, verbose = verbose)
  cli::cli_process_done()

  return(SpParams)
}

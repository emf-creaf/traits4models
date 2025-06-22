#' Trait filling from harmonized data
#'
#' Fills species parameter table for medfate with trait data from harmonized data sources
#'
#' @param SpParams A species parameter data frame to be filled for package medfate.
#' @param harmonized_trait_path A directory path were RDS files with harmonized trait databases are.
#' @param parameters A string vector of parameters to be populated. If \code{NULL} then all possible medfate parameters are parsed.
#' @param priorization A boolean flag to perform priorization of some data sources over others.
#' @param replace_previous A boolean flag to indicate that non-missing previous values should be replaced with new data
#' @param erase_previous A boolean flag to indicate that all previous values should be set to NA before populating with new data
#' @param progress A boolean flag to prompt progress.
#' @param verbose A boolean flag to prompt detailed process information.
#'
#' @return A modified data frame of medfate species parameters
#'
#' @details
#' The function processes multiple parameters of medfate SpParams table. It identifies the row to modify by
#' matching column \code{'AcceptedName'} of SpParams with the column \code{acceptedName} of harmonized trait parameter sources.
#' If the target taxon is a species, values are taken from those rows in trait_table where species names match.
#' If the target taxon is a genus, then values are taken from those rows where genus is the same.
#' If \code{priorization = TRUE} and column \code{priority_column} is available in data sources,
#' the function will prioritize sources with higher priority first, filling parameters with them before inspecting data sources
#' of lower priority.
#'
#' @export
#'
#' @seealso \code{\link{harmonize_taxonomy_WFO}}
#' @name fill_medfate_traits
fill_medfate_traits<-function(SpParams,
                              harmonized_trait_path,
                              parameters = NULL,
                              priorization = TRUE,
                              erase_previous = FALSE,
                              replace_previous = FALSE,
                              progress = TRUE, verbose = FALSE) {

  parameters_available <- medfate::SpParamsDefinition$ParameterName
  parameters_available <- parameters_available[!parameters_available %in% c("Name","SpIndex",
                                                                            "AcceptedName","Species",
                                                                            "Genus","Family","Order",
                                                                            "Group")]
  if(is.null(parameters)) {
    parameters <- parameters_available
  } else {
    parameters <- match.arg(parameters, parameters_available, several.ok = TRUE)
  }

  priority_column <- NULL
  if(priorization) priority_column <- "Priority"

  for(trait_name in c("GrowthForm","LifeForm","LeafShape","PhenologyType", "DispersalType")) {
    if(trait_name %in% parameters) {
      if(progress) cli::cli_progress_step(paste0("Processing parameter: ", trait_name))
      if(trait_name=="DispersalType")  {
        trait_table <- get_trait_data(harmonized_trait_path, "DispersalMode", output_format = "wide", progress = FALSE)
      } else {
        trait_table <- get_trait_data(harmonized_trait_path, trait_name, output_format = "wide", progress = FALSE)
      }
      if(nrow(trait_table)>0) {
        trait_mapping <- trait_name
        if(trait_name=="DispersalType") trait_mapping <- "DispersalMode"
        names(trait_mapping) <- trait_name
        SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                    taxon_column = "acceptedName", genus_column = "genus",
                                    priority_column = priority_column,
                                    erase_previous = erase_previous, character_traits = TRUE,
                                    replace_previous = replace_previous, verbose = verbose)
      }
    }
  }
  for(trait_name in c("t0gdd", "Tbgdd", "Sgdd",
                      "Phsen", "Tbsen", "xsen", "ysen", "Ssen")) {
    if(trait_name %in% parameters) {
      if(progress) cli::cli_progress_step(paste0("Processing parameter: ", trait_name))
      trait_table <- get_trait_data(harmonized_trait_path, trait_name, output_format = "wide", progress = FALSE)
      if(nrow(trait_table)>0) {
        trait_mapping <- trait_name
        names(trait_mapping) <- trait_name
        SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                    summary_function = "median",
                                    taxon_column = "acceptedName", genus_column = "genus",
                                    priority_column = priority_column,
                                    erase_previous = erase_previous, character_traits = FALSE,
                                    replace_previous = replace_previous, verbose = verbose)
      }
    }
  }

  if("LeafSize" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "LeafSize"))
    trait_table <- get_trait_data(harmonized_trait_path, "LeafArea", output_format = "wide", progress = FALSE) |>
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
                                priority_column = priority_column,
                                erase_previous = erase_previous, character_traits = TRUE,
                                replace_previous = replace_previous, verbose = verbose)
  }

  if("Dmax" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "Dmax"))
    trait_table <- get_trait_data(harmonized_trait_path, "Dmax", output_format = "wide", progress = FALSE)
    trait_mapping <- "Dmax"
    names(trait_mapping) <- "Dmax"
    SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                summary_function = "median",
                                taxon_column = "acceptedName", genus_column = "genus",
                                priority_column = priority_column,
                                erase_previous = erase_previous, character_traits = FALSE,
                                replace_previous = replace_previous, verbose = verbose)
  }

  if("Hmax" %in% parameters) {
    trait_table <- get_trait_data(harmonized_trait_path, "Hact", output_format = "wide", progress = FALSE)
    if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "Hmax"))
    trait_mapping <- "Hact"
    names(trait_mapping) <- "Hmax"
    SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                summary_function = "max",
                                taxon_column = "acceptedName", genus_column = "genus",
                                priority_column = priority_column,
                                erase_previous = erase_previous, character_traits = FALSE,
                                replace_previous = replace_previous, verbose = verbose)
  }
  if("Hmed" %in% parameters) {
    trait_table <- get_trait_data(harmonized_trait_path, "Hact", output_format = "wide", progress = FALSE)
    if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "Hmed"))
    trait_mapping <- "Hact"
    names(trait_mapping) <- "Hmed"
    SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                summary_function = "median",
                                taxon_column = "acceptedName", genus_column = "genus",
                                priority_column = priority_column,
                                erase_previous = erase_previous, character_traits = FALSE,
                                replace_previous = replace_previous, verbose = verbose)
  }
  if("cr" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "cr"))
    trait_table <- get_trait_data(harmonized_trait_path, "CrownRatio", output_format = "wide", progress = FALSE)
    trait_mapping <- "CrownRatio"
    names(trait_mapping) <- "cr"
    SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                summary_function = "median",
                                taxon_column = "acceptedName", genus_column = "genus",
                                priority_column = priority_column,
                                erase_previous = erase_previous, character_traits = FALSE,
                                replace_previous = replace_previous, verbose = verbose)
  }

  if("Gs_P50" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "Gs_P50"))
    trait_table <- get_trait_data(harmonized_trait_path, "Gs_P50", output_format = "wide", progress = FALSE)
    trait_mapping <- "Gs_P50"
    names(trait_mapping) <- "Gs_P50"
    SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                summary_function = "median",
                                taxon_column = "acceptedName", genus_column = "genus",
                                priority_column = priority_column,
                                erase_previous = erase_previous, character_traits = FALSE,
                                replace_previous = replace_previous, verbose = verbose)
  }

  if("maxFMC" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "maxFMC"))
    trait_table <- get_trait_data(harmonized_trait_path, "LFMC", output_format = "wide", progress = FALSE)
    trait_mapping <- "LFMC"
    names(trait_mapping) <- "maxFMC"
    SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                summary_function = "quantile", summary_params = list("prob" = 0.95),
                                taxon_column = "acceptedName", genus_column = "genus",
                                priority_column = priority_column,
                                erase_previous = erase_previous, character_traits = FALSE,
                                replace_previous = replace_previous, verbose = verbose)
  }
  if("minFMC" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "minFMC"))
    trait_table <- get_trait_data(harmonized_trait_path, "LFMC", output_format = "wide", progress = FALSE)
    trait_mapping <- "LFMC"
    names(trait_mapping) <- "minFMC"
    SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                summary_function = "quantile", summary_params = list("prob" = 0.05),
                                taxon_column = "acceptedName", genus_column = "genus",
                                priority_column = priority_column,
                                erase_previous = erase_previous, character_traits = FALSE,
                                replace_previous = replace_previous, verbose = verbose)
  }

  if("Kmax_stemxylem" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "Kmax_stemxylem"))
    trait_table <- get_trait_data(harmonized_trait_path, "Ks", output_format = "wide", progress = FALSE)
    trait_mapping <- "Ks"
    names(trait_mapping) <- "Kmax_stemxylem"
    SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                summary_function = "median",
                                taxon_column = "acceptedName", genus_column = "genus",
                                priority_column = priority_column,
                                erase_previous = erase_previous, character_traits = FALSE,
                                replace_previous = replace_previous, verbose = verbose)
  }

  if("Vmax298" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "Vmax298"))
    trait_table <- get_trait_data(harmonized_trait_path, "Vmax", output_format = "wide", progress = FALSE)
    trait_mapping <- "Vmax"
    names(trait_mapping) <- "Vmax298"
    SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                summary_function = "median",
                                taxon_column = "acceptedName", genus_column = "genus",
                                priority_column = priority_column,
                                erase_previous = erase_previous, character_traits = FALSE,
                                replace_previous = replace_previous, verbose = verbose)
  }

  if("Jmax298" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "Jmax298"))
    trait_table <- get_trait_data(harmonized_trait_path, "Jmax", output_format = "wide", progress = FALSE)
    trait_mapping <- "Jmax"
    names(trait_mapping) <- "Jmax298"
    SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                summary_function = "median",
                                taxon_column = "acceptedName", genus_column = "genus",
                                priority_column = priority_column,
                                erase_previous = erase_previous, character_traits = FALSE,
                                replace_previous = replace_previous, verbose = verbose)
  }

  if("Z95" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "Z95"))
    trait_table <- get_trait_data(harmonized_trait_path, "Z95", output_format = "wide", progress = FALSE)
    if(nrow(trait_table)>0) {
      trait_mapping <- "Z95"
      names(trait_mapping) <- "Z95"
      SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                  summary_function = "quantile", summary_params = list("prob" = 0.95),
                                  taxon_column = "acceptedName", genus_column = "genus",
                                  priority_column = priority_column,
                                  erase_previous = erase_previous, character_traits = FALSE,
                                  replace_previous = replace_previous, verbose = verbose)
    }
  }

  if("LeafAngle" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "LeafAngle"))
    trait_table <- get_trait_data(harmonized_trait_path, "LeafAngle", output_format = "wide", progress = FALSE)
    if(nrow(trait_table)>0) {
      trait_mapping <- "LeafAngle"
      names(trait_mapping) <- "LeafAngle"
      SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                  summary_function = "median",
                                  taxon_column = "acceptedName", genus_column = "genus",
                                  priority_column = priority_column,
                                  erase_previous = erase_previous, character_traits = FALSE,
                                  replace_previous = replace_previous, verbose = verbose)
    }
  }

  if("LeafAngleSD" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "LeafAngleSD"))
    trait_table <- get_trait_data(harmonized_trait_path, "LeafAngle", output_format = "wide", progress = FALSE)
    if(nrow(trait_table)>0) {
      trait_mapping <- "LeafAngle"
      names(trait_mapping) <- "LeafAngleSD"
      SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                  summary_function = "sd",
                                  taxon_column = "acceptedName", genus_column = "genus",
                                  priority_column = priority_column,
                                  erase_previous = erase_previous, character_traits = FALSE,
                                  replace_previous = replace_previous, verbose = verbose)
    }
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
    if(trait_name %in% parameters) {
      if(progress)  cli::cli_progress_step(paste0("Processing parameter: ", trait_name))
      trait_table <- get_trait_data(harmonized_trait_path, trait_name, output_format = "wide", progress = FALSE)
      if(nrow(trait_table)>0) {
        trait_mapping <- trait_name
        names(trait_mapping) <- trait_name
        SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                    summary_function = "median",
                                    taxon_column = "acceptedName", genus_column = "genus",
                                    priority_column = priority_column,
                                    erase_previous = erase_previous, character_traits = FALSE,
                                    replace_previous = replace_previous, verbose = verbose)
      }
    }
  }

  if("RSSG" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "RSSG"))
    trait_table <- get_trait_data(harmonized_trait_path, "ShadeTolerance", output_format = "wide", progress = FALSE)
    trait_mapping <- "ShadeTolerance"
    names(trait_mapping) <- "RSSG"
    SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                summary_function = "median",
                                scalar_functions = c("RSSG" = function(x){min(0.95,0.25+0.70*((x-1)/2))}),
                                taxon_column = "acceptedName", genus_column = "genus",
                                priority_column = priority_column,
                                erase_previous = erase_previous, character_traits = FALSE,
                                replace_previous = replace_previous, verbose = verbose)
  }

  if("SeedProductionDiameter" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing parameter: ", "SeedProductionDiameter"))
    trait_table <- get_trait_data(harmonized_trait_path, "Dmat", output_format = "wide", progress = FALSE)
    if(nrow(trait_table)>0) {
      trait_mapping <- "Dmat"
      names(trait_mapping) <- "SeedProductionDiameter"
      SpParams <- populate_traits(SpParams, trait_table, trait_mapping,
                                  summary_function = "median",
                                  taxon_column = "acceptedName", genus_column = "genus",
                                  priority_column = priority_column,
                                  erase_previous = erase_previous, character_traits = FALSE,
                                  replace_previous = replace_previous, verbose = verbose)
    }
  }
  cli::cli_process_done()

  return(SpParams)
}

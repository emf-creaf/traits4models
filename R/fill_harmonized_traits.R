.populate_traits <- function(SpParams,
                             trait_table,
                             trait_mapping,
                             replace_previous,
                             erase_previous) {

  trait_params <- names(trait_mapping)
  if(erase_previous) SpParams[,trait_params] = NA

  for(j in 1:length(trait_mapping)) {
    trait <- trait_mapping[[j]]
    param <- trait_params[j]
    for(i in 1:nrow(SpParams)) {
      initial_missing <- is.na(SpParams[i,param])
      if(initial_missing || replace_previous) {
        row <- which(trait_table$acceptedName == SpParams$AcceptedName[i])
        if(length(row)>0) {
          SpParams[i,param] <- trait_table[row[1],trait]
        }
      }
    }
  }
  return(SpParams)
}
.get_trait_table <- function(SpParams,
                             harmonized_trait_path,
                             traits,
                             priorization,
                             summary_function = "weightedmean",
                             summary_params = list(na.rm=TRUE),
                             scalar_functions = NULL,
                             aggregation_level_weights = c("individual" = 1, "population" = 10, "taxon" = 100)) {

  # print(traits)
  AcceptedNameGenus <- SpParams$Genus[is.na(SpParams$Species)]
  AcceptedNameSpecies <- SpParams$AcceptedName[!is.na(SpParams$Species)]

  trait_table_species <- taxon_trait_summary(harmonized_trait_path,
                                             taxonomic_level = "species",
                                             priorization = priorization,
                                             aggregation_level_weights = aggregation_level_weights,
                                             summary_function = summary_function,
                                             summary_params = summary_params,
                                             scalar_functions = scalar_functions,
                                             traits = traits,
                                             taxon_selection = AcceptedNameSpecies,
                                             progress = FALSE,
                                             verbose = FALSE)
  trait_table_genus <- taxon_trait_summary(harmonized_trait_path,
                                           taxonomic_level = "genus",
                                           priorization = priorization,
                                           aggregation_level_weights = aggregation_level_weights,
                                           taxon_selection = AcceptedNameGenus,
                                           summary_function = summary_function,
                                           summary_params = summary_params,
                                           scalar_functions = scalar_functions,
                                           traits = traits,
                                           progress = FALSE,
                                           verbose = FALSE) |>
    dplyr::rename("acceptedName" = "genus")
  # print(head(trait_table_species))
  # print(head(trait_table_genus))
  trait_table <- dplyr::bind_rows(trait_table_species, trait_table_genus)
  return(trait_table)
}


.fill_trait_block <- function(SpParams,
                              harmonized_trait_path,
                              trait_mapping,
                              priorization,
                              summary_function = "weightedmean",
                              summary_params = list(na.rm=TRUE),
                              scalar_functions = NULL,
                              erase_previous = FALSE,
                              replace_previous = FALSE,
                              aggregation_level_weights = c("individual" = 1, "population" = 10, "taxon" = 100)) {

  trait_table <- .get_trait_table(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  traits = as.character(trait_mapping),
                                  priorization = priorization,
                                  summary_function = summary_function,
                                  summary_params = summary_params,
                                  scalar_functions = scalar_functions,
                                  aggregation_level_weights = aggregation_level_weights)
  return(.populate_traits(SpParams,
                          trait_table,
                          trait_mapping,
                          replace_previous,
                          erase_previous))
}

#' Trait filling from harmonized data
#'
#' Fills species parameter table for medfate with trait data from harmonized data sources
#'
#' @param SpParams A species parameter data frame to be filled for package medfate.
#' @param harmonized_trait_path A directory path were RDS files with harmonized trait databases are.
#' @param parameters A string vector of parameters to be populated. If \code{NULL} then all possible medfate parameters are parsed.
#' @param priorization A boolean flag to perform priorization of some data sources over others.
#' @param aggregation_level_weights A vector of weights to be applied to different aggregation levels when calculating numeric averages.
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
                              aggregation_level_weights = c("individual" = 1, "population" = 10, "taxon" = 100),
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


  # GrowthForm, LifeForm, LeafShape, PhenologyType, DispersalType
  parameters_sel <- c("GrowthForm", "LifeForm", "LeafShape", "PhenologyType", "DispersalType")
  traits_sel <- c("GrowthForm", "LifeForm", "LeafShape", "PhenologyType", "DispersalMode")
  trait_mapping <- traits_sel
  names(trait_mapping) <- parameters_sel
  trait_mapping <- trait_mapping[parameters_sel %in% parameters]
  if(length(trait_mapping)>0) {
    if(progress) cli::cli_progress_step(paste0("Processing ", paste(names(trait_mapping), collapse=", ")))
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmode",
                                  summary_params = list(na.rm = TRUE),
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }


  parameters_sel <- c("t0gdd", "Tbgdd", "Sgdd",
                      "Phsen", "Tbsen", "Ssen")
  traits_sel <- parameters_sel
  trait_mapping <- traits_sel
  names(trait_mapping) <- parameters_sel
  trait_mapping <- trait_mapping[parameters_sel %in% parameters]
  if(length(trait_mapping)>0) {
    if(progress) cli::cli_progress_step(paste0("Processing ", paste(names(trait_mapping), collapse=", ")))
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmedian",
                                  summary_params = list(na.rm = TRUE),
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }
  parameters_sel <- c("xsen", "ysen")
  traits_sel <- parameters_sel
  trait_mapping <- traits_sel
  names(trait_mapping) <- parameters_sel
  trait_mapping <- trait_mapping[parameters_sel %in% parameters]
  if(length(trait_mapping)>0) {
    if(progress) cli::cli_progress_step(paste0("Processing ", paste(names(trait_mapping), collapse=", ")))
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmode",
                                  summary_params = list(na.rm = TRUE),
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }

  if("LeafSize" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing ", "LeafSize"))
    trait_table <- .get_trait_table(SpParams,
                                    harmonized_trait_path = harmonized_trait_path,
                                    traits = "LeafArea",
                                    priorization = priorization,
                                    summary_function = "weightedmedian",
                                    summary_params = list(na.rm = TRUE),
                                    aggregation_level_weights = aggregation_level_weights)|>
      dplyr::mutate(
        LeafSize = dplyr::case_when(
          LeafArea<225 ~"Small",
          LeafArea<2025 & LeafArea>=225 ~ "Medium",
          LeafArea>=2025 & LeafArea<100000 ~ "Large"
        ))
    trait_mapping <- "LeafSize"
    names(trait_mapping) <- "LeafSize"
    SpParams <- .populate_traits(SpParams,
                                 trait_table,
                                 trait_mapping,
                                 replace_previous,
                                 erase_previous)
  }
  if("Hmax" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing ", "Hmax"))
    trait_mapping <- "Hact"
    names(trait_mapping) <- "Hmax"
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedquantile",
                                  summary_params = list(probs = 0.99),
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }
  if("Hmed" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing ", "Hmed"))
    trait_mapping <- "Hact"
    names(trait_mapping) <- "Hmed"
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmedian",
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }

  if("cr" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing ", "cr"))
    trait_mapping <- "CrownRatio"
    names(trait_mapping) <- "cr"
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmedian",
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }

  if("Z95" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing ", "Z95"))
    trait_mapping <- "Z95"
    names(trait_mapping) <- "Z95"
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedquantile",
                                  summary_params = list("prob" = 0.95),
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }

  parameters_sel <- c("Dmax", "WoodDensity", "SRL" , "r635",
                      "LigninPercent","pDead", "SAV", "HeatContent")
  traits_sel <- parameters_sel
  trait_mapping <- traits_sel
  names(trait_mapping) <- parameters_sel
  trait_mapping <- trait_mapping[parameters_sel %in% parameters]
  if(length(trait_mapping)>0) {
    if(progress) cli::cli_progress_step(paste0("Processing ", paste(names(trait_mapping), collapse=", ")))
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmedian",
                                  summary_params = list(na.rm = TRUE),
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }
  parameters_sel <- c("LeafDensity", "SLA", "LeafWidth", "LeafDuration")
  traits_sel <- parameters_sel
  trait_mapping <- traits_sel
  names(trait_mapping) <- parameters_sel
  trait_mapping <- trait_mapping[parameters_sel %in% parameters]
  if(length(trait_mapping)>0) {
    if(progress) cli::cli_progress_step(paste0("Processing ", paste(names(trait_mapping), collapse=", ")))
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmedian",
                                  summary_params = list(na.rm = TRUE),
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }
  if("LeafAngle" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing ", "LeafAngle"))
    trait_mapping <- "LeafAngle"
    names(trait_mapping) <- "LeafAngle"
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmean",
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
    SpParams <- SpParams |> # Check consistency (avoid average angles that are too low)
      dplyr::mutate(LeafAngle = ifelse(.data[["LeafAngle"]] < 20, 20, .data[["LeafAngle"]]))
  }

  if("LeafAngleSD" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing ", "LeafAngleSD"))
    trait_mapping <- "LeafAngle"
    names(trait_mapping) <- "LeafAngleSD"
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedsd",
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
    SpParams <- SpParams |> # Check consistency (avoid sd angles that are too low)
      dplyr::mutate(LeafAngleSD = ifelse(.data[["LeafAngleSD"]] < 10, 10, .data[["LeafAngleSD"]]))
  }

  parameters_sel <- c("LeafAF", "LeafPI0", "LeafEPS", "Ptlp")
  traits_sel <- parameters_sel
  trait_mapping <- traits_sel
  names(trait_mapping) <- parameters_sel
  trait_mapping <- trait_mapping[parameters_sel %in% parameters]
  if(length(trait_mapping)>0) {
    if(progress) cli::cli_progress_step(paste0("Processing ", paste(names(trait_mapping), collapse=", ")))
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmedian",
                                  summary_params = list(na.rm = TRUE),
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }

  parameters_sel <- c("Gswmax", "Gswmin")
  traits_sel <- parameters_sel
  trait_mapping <- traits_sel
  names(trait_mapping) <- parameters_sel
  trait_mapping <- trait_mapping[parameters_sel %in% parameters]
  if(length(trait_mapping)>0) {
    if(progress) cli::cli_progress_step(paste0("Processing ", paste(names(trait_mapping), collapse=", ")))
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmedian",
                                  summary_params = list(na.rm = TRUE),
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }

  if("Kmax_stemxylem" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing ", "Kmax_stemxylem"))
    trait_mapping <- "Ks"
    names(trait_mapping) <- "Kmax_stemxylem"
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmedian",
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }
  parameters_sel <- c("Al2As", "conduit2sapwood")
  traits_sel <- parameters_sel
  trait_mapping <- traits_sel
  names(trait_mapping) <- parameters_sel
  trait_mapping <- trait_mapping[parameters_sel %in% parameters]
  if(length(trait_mapping)>0) {
    if(progress) cli::cli_progress_step(paste0("Processing ", paste(names(trait_mapping), collapse=", ")))
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmedian",
                                  summary_params = list(na.rm = TRUE),
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }
  if("VCleaf_kmax" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing ", "VCleaf_kmax"))
    trait_mapping <- "kleaf"
    names(trait_mapping) <- "VCleaf_kmax"
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmedian",
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }
  parameters_sel <- c("VCleaf_P50", "VCleaf_P12", "VCleaf_P88", "VCleaf_slope",
                      "VCstem_P50", "VCstem_P12", "VCstem_P88", "VCstem_slope",
                      "VCroot_P50", "VCroot_P12", "VCroot_P88", "VCroot_slope")
  traits_sel <- parameters_sel
  trait_mapping <- traits_sel
  names(trait_mapping) <- parameters_sel
  trait_mapping <- trait_mapping[parameters_sel %in% parameters]
  if(length(trait_mapping)>0) {
    if(progress) cli::cli_progress_step(paste0("Processing ", paste(names(trait_mapping), collapse=", ")))
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmedian",
                                  summary_params = list(na.rm = TRUE),
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
    # Check physiological inconsistency
    SpParams <- SpParams |>
      dplyr::mutate(VCstem_P12 = ifelse(.data[["VCstem_P12"]] < .data[["VCstem_P50"]], NA, .data[["VCstem_P12"]]),
                    VCstem_P88 = ifelse(.data[["VCstem_P88"]] > .data[["VCstem_P50"]], NA, .data[["VCstem_P88"]]),
                    VCstem_slope = ifelse(!is.na(.data[["VCstem_P88"]]) & !is.na(.data[["VCstem_P12"]]), (88 - 12)/(abs(.data[["VCstem_P88"]]) - abs(.data[["VCstem_P12"]])), .data[["VCstem_slope"]]))|>
      dplyr::mutate(VCleaf_P12 = ifelse(.data[["VCleaf_P12"]] < .data[["VCleaf_P50"]], NA, .data[["VCleaf_P12"]]),
                    VCleaf_P88 = ifelse(.data[["VCleaf_P88"]] > .data[["VCleaf_P50"]], NA, .data[["VCleaf_P88"]]),
                    VCleaf_slope = ifelse(!is.na(.data[["VCleaf_P88"]]) & !is.na(.data[["VCleaf_P12"]]), (88 - 12)/(abs(.data[["VCleaf_P88"]]) - abs(.data[["VCleaf_P12"]])), .data[["VCleaf_slope"]]))|>
      dplyr::mutate(VCroot_P12 = ifelse(.data[["VCroot_P12"]] < .data[["VCroot_P50"]], NA, .data[["VCroot_P12"]]),
                    VCroot_P88 = ifelse(.data[["VCroot_P88"]] > .data[["VCroot_P50"]], NA, .data[["VCroot_P88"]]),
                    VCroot_slope = ifelse(!is.na(.data[["VCroot_P88"]]) & !is.na(.data[["VCroot_P12"]]), (88 - 12)/(abs(.data[["VCroot_P88"]]) - abs(.data[["VCroot_P12"]])), .data[["VCroot_slope"]]))
  }

  if("Vmax298" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing ", "Vmax298"))
    trait_mapping <- "Vmax"
    names(trait_mapping) <- "Vmax298"
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmedian",
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }
  if("Jmax298" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing ", "Jmax298"))
    trait_mapping <- "Jmax"
    names(trait_mapping) <- "Jmax298"
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmedian",
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }

  parameters_sel <- c("Nleaf","Nsapwood", "Nfineroot")
  traits_sel <- parameters_sel
  trait_mapping <- traits_sel
  names(trait_mapping) <- parameters_sel
  trait_mapping <- trait_mapping[parameters_sel %in% parameters]
  if(length(trait_mapping)>0) {
    if(progress) cli::cli_progress_step(paste0("Processing ", paste(names(trait_mapping), collapse=", ")))
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmedian",
                                  summary_params = list(na.rm = TRUE),
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }
  parameters_sel <- c("SeedMass", "SeedLongevity")
  traits_sel <- parameters_sel
  trait_mapping <- traits_sel
  names(trait_mapping) <- parameters_sel
  trait_mapping <- trait_mapping[parameters_sel %in% parameters]
  if(length(trait_mapping)>0) {
    if(progress) cli::cli_progress_step(paste0("Processing ", paste(names(trait_mapping), collapse=", ")))
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmedian",
                                  summary_params = list(na.rm = TRUE),
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }
  parameters_sel <- c("WoodC", "CCleaf", "CCsapwood", "CCfineroot")
  traits_sel <- parameters_sel
  trait_mapping <- traits_sel
  names(trait_mapping) <- parameters_sel
  trait_mapping <- trait_mapping[parameters_sel %in% parameters]
  if(length(trait_mapping)>0) {
    if(progress) cli::cli_progress_step(paste0("Processing ", paste(names(trait_mapping), collapse=", ")))
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmedian",
                                  summary_params = list(na.rm = TRUE),
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }

  if("maxFMC" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing ", "maxFMC"))
    trait_mapping <- "LFMC"
    names(trait_mapping) <- "maxFMC"
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedquantile",
                                  summary_params = list("prob" = 0.95),
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }
  if("minFMC" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing ", "minFMC"))
    trait_mapping <- "LFMC"
    names(trait_mapping) <- "minFMC"
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedquantile",
                                  summary_params = list("prob" = 0.05),
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }


  if("RSSG" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing ", "RSSG"))
    trait_mapping <- "ShadeTolerance"
    names(trait_mapping) <- "RSSG"
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  scalar_functions = c("RSSG" = function(x){min(0.95,0.25+0.70*((x-1)/2))}),
                                  summary_function = "weightedmedian",
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }


  if("SeedProductionDiameter" %in% parameters) {
    if(progress) cli::cli_progress_step(paste0("Processing ", "SeedProductionDiameter"))
    trait_mapping <- "Dmat"
    names(trait_mapping) <- "SeedProductionDiameter"
    SpParams <- .fill_trait_block(SpParams,
                                  harmonized_trait_path = harmonized_trait_path,
                                  trait_mapping = trait_mapping,
                                  priorization = priorization,
                                  summary_function = "weightedmedian",
                                  aggregation_level_weights = aggregation_level_weights,
                                  erase_previous = erase_previous,
                                  replace_previous = replace_previous)
  }
  cli::cli_process_done()

  return(SpParams)
}

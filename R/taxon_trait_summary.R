.mode <- function(x, na.rm = TRUE, ...) {
  if(na.rm) x <- x[!is.na(x)]
  freq <- table(x)
  return(names(freq)[which.max(freq)])
}

.value_weight_data_frame <- function(values, levels, df_levels, na.rm = TRUE) {
  if(na.rm) {
    is_na <- is.na(values) | is.na(levels)
  } else {
    is_na <- rep(FALSE, length(values))
  }
  data.frame(x = values[!is_na],
                   level = levels[!is_na]) |>
    dplyr::left_join(df_levels, by = "level")
}

.level_weighted_mode <- function(values, levels, df_levels, na.rm = TRUE, ...) {
  df <- .value_weight_data_frame(values, levels, df_levels, na.rm)
  wbv <- tapply(df$weight, df$x, sum)
  names(wbv)[which.max(wbv)]
}

.level_weighted_mean <- function(values, levels, df_levels, na.rm = TRUE, ...) {
  df <- .value_weight_data_frame(values, levels, df_levels, na.rm)
  return(sum(df$x*df$weight)/sum(df$weight))
}

.level_weighted_median <- function(values, levels, df_levels, na.rm = TRUE, ...) {
  df <- .value_weight_data_frame(values, levels, df_levels, na.rm)
  ord <- order(df$x)
  x <- df$x[ord]
  w <- df$weight[ord]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= 0.5)[1]]
}

# From Hmisc
.level_weighted_quantile <- function(values, levels, df_levels, probs = 0.95, na.rm = TRUE, ...) {
  df <- .value_weight_data_frame(values, levels, df_levels, na.rm)
  return(Hmisc::wtd.quantile(df$x, df$weights, probs = probs))
}

.level_weighted_var <- function(values, levels, df_levels, na.rm = TRUE, ...) {
  df <- .value_weight_data_frame(values, levels, df_levels, na.rm)
  return(Hmisc::wtd.var(df$x, df$weights))
}

.level_weighted_sd <- function(values, levels, df_levels, na.rm = TRUE, ...) {
  return(sqrt(.level_weighted_var(values, levels, df_levels, na.rm, ...)))
}

.summary_evaluation <- function(summary_function, summary_params,
                                expected_type, values, levels, df_levels) {
  if(summary_function == "n") return(length(values[!is.na(values)]))
  if(expected_type=="Numeric") {
    if(summary_function == "weightedmean") {
      l = c(list("values"=values, "levels" = levels, "df_levels" = df_levels), summary_params)
      return(do.call(".level_weighted_mean", l))
    } else if(summary_function == "weightedmedian") {
      l = c(list("values"=values, "levels" = levels, "df_levels" = df_levels), summary_params)
      return(do.call(".level_weighted_median", l))
    } else if(summary_function == "weightedvar") {
      l = c(list("values"=values, "levels" = levels, "df_levels" = df_levels), summary_params)
      return(do.call(".level_weighted_var", l))
    } else if(summary_function == "weightedsd") {
      l = c(list("values"=values, "levels" = levels, "df_levels" = df_levels), summary_params)
      return(do.call(".level_weighted_sd", l))
    } else if(summary_function == "weightedquantile") {
      l = c(list("values"=values, "levels" = levels, "df_levels" = df_levels), summary_params)
      return(do.call(".level_weighted_quantile", l))
    } else if(summary_function %in% c("mean", "median", "quantile", "var", "sd")) {
      if(length(values)>0) {
        l = c(list("x"=values), summary_params)
        return(do.call(summary_function, l))
      } else {
        return(as.numeric(NA))
      }
    } else {
      cli::cli_abort(paste0("Wrong summary function for numerical data: ", summary_function))
    }
  } else if(expected_type=="Integer") {
    if(length(values)>0) {
      if(summary_function == "weightedmode") {
        l = c(list("values"=values, "levels" = levels, "df_levels" = df_levels), summary_params)
        return(as.integer(as.numeric(do.call(".level_weighted_mode", l))))
      } else if(summary_function == "mode") {
        l = c(list("x"=values), summary_params)
        return(as.integer(as.numeric(do.call(".mode", l))))
      } else {
        cli::cli_abort(paste0("Wrong summary function for integer/categorical data: ", summary_function))
      }
    } else {
      return(as.integer(NA))
    }
  } else if(expected_type=="String") {
    if(length(values)>0) {
      if(summary_function == "weightedmode") {
        l = c(list("values"=values, "levels" = levels, "df_levels" = df_levels), summary_params)
        return(as.character(do.call(".level_weighted_mode", l)))
      } else if(summary_function == "mode") {
        l = c(list("x"=values), summary_params)
        return(as.character(do.call(".mode", l)))
      } else {
        cli::cli_abort(paste0("Wrong summary function for integer/categorical data: ", summary_function))
      }
    } else {
      return(as.character(NA))
    }
  }
}

#' Taxon trait summaries
#'
#' Allows evaluating summary functions (e.g. means, sd, median, mode, quantiles, etc.) on trait data by taxon
#'
#' @param harmonized_trait_path The path to harmonized trait data files (.rds or .csv format).
#' @param traits A character vector with the set of traits to summarize. If \code{NULL}, then all available traits are summarized.
#' @param taxonomic_level Taxonomic level for summary: either 'species' (acceptedName), 'genus' or 'family'
#' @param taxon_selection String vector with taxon names to perform selection. By default, all taxa are returned.
#' @param summary_function A function (statistic) to summarize numeric values for the same taxonomic entity (see details).
#' @param summary_params A list of summary function params (by default \code{na.rm=TRUE}).
#' @param scalar_functions A named list of scalar functions for traits needing transformation of units or scaling before summary.
#' @param priorization A boolean flag to perform priorization of some data sources over others.
#' @param aggregation_level_weights A vector of weights to be applied to different aggregation levels when calculating numeric averages.
#' @param progress A boolean flag to prompt progress.
#' @param verbose A boolean flag to prompt detailed process information.
#'
#' @details
#' If \code{priorization = TRUE} and column \code{priority_column} is available in data sources,
#' the function will prioritize sources with higher priority first, filling parameters with them before inspecting data sources
#' of lower priority.
#'
#' For categorical traits the following options are possible using \code{summary_function} and \code{summary_params}:
#' \itemize{
#'   \item{n - Number of non-missing observations.}
#'   \item{weightedmode - Weighted mode using aggregation level weights.}
#'   \item{mode - The usual statistic functions. }
#' }
#'
#' For numerical traits the following options are possible using \code{summary_function} and \code{summary_params}:
#' \itemize{
#'   \item{n - Number of non-missing observations.}
#'   \item{weightedmean, weightedmedian, weightedquantile, weightedvar, weightedsd - Weighted statistics using aggregation level weights.}
#'   \item{mean, median, quantile, var, sd - The usual statistic functions. }
#' }
#'
#' @returns
#' A data frame with taxon summary function values for the set of indicated traits.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  # List harmonized trait files
#'  DB_path <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/traits_and_models/"
#'  harmonized_trait_path <- paste0(DB_path,"data/harmonized_trait_sources")
#'
#'  # Get family-level weighted means for two specific traits
#'  taxon_trait_summary(harmonized_trait_path, c("SLA", "Gswmax"),
#'                      taxonomic_level = "family", progress = FALSE)
#'
#'}
taxon_trait_summary <- function(harmonized_trait_path,
                                traits = NULL,
                                taxonomic_level = "species",
                                taxon_selection = NULL,
                                summary_function = "weightedmean",
                                summary_params = list(na.rm=TRUE),
                                scalar_functions = NULL,
                                priorization = TRUE,
                                aggregation_level_weights = c("individual" = 1, "population" = 10, "taxon" = 100),
                                progress = TRUE,
                                verbose = TRUE) {
  taxonomic_level <- match.arg(taxonomic_level, c("species", "genus", "family"))
  summary_function <- match.arg(summary_function, c("n", "mean", "median", "var", "sd", "quantile", "mode",
                                                    "weightedmean", "weightedmedian", "weightedvar", "weightedsd", "weightedquantile", "weightedmode"))
  if(is.null(traits)) {
    traits <- traits4models::HarmonizedTraitDefinition$Notation
  } else {
    traits <- match.arg(traits, traits4models::HarmonizedTraitDefinition$Notation, several.ok = TRUE)
  }
  df_levels <- data.frame(level = names(aggregation_level_weights),
                          weight = as.numeric(aggregation_level_weights))


  if(taxonomic_level =="species") {
    grouping_column = "acceptedName"
  } else if(taxonomic_level =="genus") {
    grouping_column = "genus"
  } else if(taxonomic_level =="family") {
    grouping_column = "family"
  }
  taxon_summaries <- data.frame()
  taxon_summaries[[grouping_column]] <- character(0)
  for(t in traits) {
    if(progress) cli::cli_progress_step(paste0("Summarizing trait '", t,"'."))
    expected_type <- traits4models::HarmonizedTraitDefinition$Type[traits4models::HarmonizedTraitDefinition$Notation == t]
    trait_table  <- get_trait_data(harmonized_trait_path, trait_name = t, taxon_selection_column = grouping_column, taxon_selection = taxon_selection,
                                   output_format = "long", progress = progress, verbose = verbose)

    if(expected_type %in% c("Integer", "String")) {
      if(!(summary_function %in% c("n", "mode", "weightedmode")))  cli::cli_abort(paste0("Cannot evaluate summary function '", summary_function, "' on an integer/categorical trait '", t,"'."))
    }
    else if(expected_type %in% c("Numeric")) {
      if(!(summary_function %in% c("n", "median", "mean", "sd", "var", "quantile",
                                   "weightedmean", "weightedmedian", "weightedsd", "weightedvar", "weightedquantile")))  cli::cli_abort(paste0("Cannot evaluate summary function '", summary_function, "' on an integer/categorical trait '", t,"'."))
    }
    if(t %in% names(scalar_functions)) {
      trait_table <- trait_table |>
        dplyr::mutate(Value = do.call(scalar_functions[[t]], list(.data[["Value"]])))
    }

    if(nrow(trait_table)>0) {
      if(priorization) { ## Adds priority by order
        trait_summary_table <- trait_table |>
          dplyr::group_by(.data[[grouping_column]]) |>
          dplyr::filter(.data[["Priority"]]==1) |>
          dplyr::summarize(Value = .summary_evaluation(summary_function = summary_function,
                                                       summary_params = summary_params,
                                                       expected_type = expected_type,
                                                       values = .data[["Value"]],
                                                       levels = .data[["Level"]],
                                                       df_levels = df_levels))
        trait_summary_table_P2 <- trait_table |>
          dplyr::group_by(.data[[grouping_column]]) |>
          dplyr::filter(.data[["Priority"]]==2) |>
          dplyr::summarize(Value = .summary_evaluation(summary_function = summary_function,
                                                       summary_params = summary_params,
                                                       expected_type = expected_type,
                                                       values = .data[["Value"]],
                                                       levels = .data[["Level"]],
                                                       df_levels = df_levels)) |>
          dplyr::filter(!(.data[[grouping_column]] %in% trait_summary_table[[grouping_column]]))
        trait_summary_table <- dplyr::bind_rows(trait_summary_table, trait_summary_table_P2)
        trait_summary_table_P3 <- trait_table |>
          dplyr::group_by(.data[[grouping_column]]) |>
          dplyr::filter(.data[["Priority"]]==3) |>
          dplyr::summarize(Value = .summary_evaluation(summary_function = summary_function,
                                                       summary_params = summary_params,
                                                       expected_type = expected_type,
                                                       values = .data[["Value"]],
                                                       levels = .data[["Level"]],
                                                       df_levels = df_levels))|>
          dplyr::filter(!(.data[[grouping_column]] %in% trait_summary_table[[grouping_column]]))
        trait_summary_table <- dplyr::bind_rows(trait_summary_table, trait_summary_table_P3)
      } else {
        trait_summary_table <- trait_table |>
          dplyr::group_by(.data[[grouping_column]]) |>
          dplyr::summarize(Value = .summary_evaluation(summary_function = summary_function,
                                                       summary_params = summary_params,
                                                       expected_type = expected_type,
                                                       values = .data[["Value"]],
                                                       levels = .data[["Level"]],
                                                       df_levels = df_levels))
      }
      names(trait_summary_table)[2] <- t
      taxon_summaries <- taxon_summaries |>
        dplyr::full_join(trait_summary_table, by=grouping_column)
    } else {
      if(expected_type=="Numeric") {
        taxon_summaries[[t]] <- as.numeric(rep(NA, nrow(taxon_summaries)))
      } else if(expected_type=="Integer") {
        taxon_summaries[[t]] <- as.integer(rep(NA, nrow(taxon_summaries)))
      } else if(expected_type=="String") {
        taxon_summaries[[t]] <- as.character(rep(NA, nrow(taxon_summaries)))
      }
    }
  }
  taxon_summaries <- taxon_summaries |>
    dplyr::arrange(.data[[grouping_column]])
  # print(head(taxon_summaries))
  return(taxon_summaries)
}

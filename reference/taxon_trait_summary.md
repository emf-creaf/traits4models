# Taxon trait summaries

Allows evaluating summary functions (e.g. means, sd, median, mode,
quantiles, etc.) on trait data by taxon

## Usage

``` r
taxon_trait_summary(
  harmonized_trait_path,
  traits = NULL,
  taxonomic_level = "species",
  taxon_selection = NULL,
  summary_function = "weightedmean",
  summary_params = list(na.rm = TRUE),
  scalar_functions = NULL,
  priorization = TRUE,
  aggregation_level_weights = c(individual = 1, population = 10, taxon = 100),
  progress = TRUE,
  verbose = TRUE
)
```

## Arguments

- harmonized_trait_path:

  The path to harmonized trait data files (.rds or .csv format).

- traits:

  A character vector with the set of traits to summarize. If `NULL`,
  then all available traits are summarized.

- taxonomic_level:

  Taxonomic level for summary: either 'species' (acceptedName), 'genus'
  or 'family'

- taxon_selection:

  String vector with taxon names to perform selection. By default, all
  taxa are returned.

- summary_function:

  A function (statistic) to summarize numeric values for the same
  taxonomic entity (see details).

- summary_params:

  A list of summary function params (by default `na.rm=TRUE`).

- scalar_functions:

  A named list of scalar functions for traits needing transformation of
  units or scaling before summary.

- priorization:

  A boolean flag to perform priorization of some data sources over
  others.

- aggregation_level_weights:

  A vector of weights to be applied to different aggregation levels when
  calculating numeric averages.

- progress:

  A boolean flag to prompt progress.

- verbose:

  A boolean flag to prompt detailed process information.

## Value

A data frame with taxon summary function values for the set of indicated
traits.

## Details

If `priorization = TRUE` and column `priority_column` is available in
data sources, the function will prioritize sources with higher priority
first, filling parameters with them before inspecting data sources of
lower priority.

For categorical traits the following options are possible using
`summary_function` and `summary_params`:

- weightedmode - Weighted mode using aggregation level weights.

- mode - The usual statistic functions.

For numerical traits the following options are possible using
`summary_function` and `summary_params`:

- weightedmean, weightedmedian, weightedquantile, weightedvar,
  weightedsd - Weighted statistics using aggregation level weights.

- mean, median, quantile, var, sd, ... - The usual statistic functions.

## Examples

``` r
if (FALSE) { # \dontrun{
 # List harmonized trait files
 DB_path <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/traits_and_models/"
 harmonized_trait_path <- paste0(DB_path,"data/harmonized_trait_sources")

 # Get family-level weighted means for two specific traits
 taxon_trait_summary(harmonized_trait_path, c("SLA", "Gswmax"),
                     taxonomic_level = "family", progress = FALSE)

} # }
```

# Trait filling from harmonized data

Fills species parameter table for medfate with trait data from
harmonized data sources

## Usage

``` r
fill_medfate_traits(
  SpParams,
  harmonized_trait_path,
  parameters = NULL,
  priorization = TRUE,
  erase_previous = FALSE,
  replace_previous = FALSE,
  progress = TRUE,
  verbose = FALSE
)
```

## Arguments

- SpParams:

  A species parameter data frame to be filled for package medfate.

- harmonized_trait_path:

  A directory path were RDS files with harmonized trait databases are.

- parameters:

  A string vector of parameters to be populated. If `NULL` then all
  possible medfate parameters are parsed.

- priorization:

  A boolean flag to perform priorization of some data sources over
  others.

- erase_previous:

  A boolean flag to indicate that all previous values should be set to
  NA before populating with new data

- replace_previous:

  A boolean flag to indicate that non-missing previous values should be
  replaced with new data

- progress:

  A boolean flag to prompt progress.

- verbose:

  A boolean flag to prompt detailed process information.

## Value

A modified data frame of medfate species parameters

## Details

The function processes multiple parameters of medfate SpParams table. It
identifies the row to modify by matching column `'AcceptedName'` of
SpParams with the column `acceptedName` of harmonized trait parameter
sources. If the target taxon is a species, values are taken from those
rows in trait_table where species names match. If the target taxon is a
genus, then values are taken from those rows where genus is the same. If
`priorization = TRUE` and column `priority_column` is available in data
sources, the function will prioritize sources with higher priority
first, filling parameters with them before inspecting data sources of
lower priority.

## See also

[`harmonize_taxonomy_WFO`](https://emf-creaf.github.io/traits4models/reference/harmonize_taxonomy_WFO.md)

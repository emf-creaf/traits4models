# Allometry filling from harmonized data

Function that uses harmonized allometric equations to fill medfate
allometric coefficients for taxa on the basis of their accepted name.

## Usage

``` r
fill_medfate_allometries(
  SpParams,
  harmonized_allometry_path,
  responses = NULL,
  priorization = TRUE,
  erase_previous = TRUE,
  replace_previous = TRUE,
  progress = TRUE,
  verbose = FALSE
)
```

## Arguments

- SpParams:

  A data frame of medfate species parameters to be populated.

- harmonized_allometry_path:

  The path to harmonized trait data files (.rds or .csv format).

- responses:

  A string vector with responses to be filled. If NULL, then all
  responses are processed.

- priorization:

  A boolean flag to perform priorization of some data sources over
  others (not yet implemented!).

- erase_previous:

  A boolean flag to indicate that all previous values should be set to
  NA before populating with new data

- replace_previous:

  A boolean flag to indicate that non-missing previous values should be
  replaced with new data

- progress:

  A boolean flag to prompt progress.

- verbose:

  A boolean flag to indicate extra console output.

## Value

A modified data frame of medfate species parameters.

## See also

[`init_medfate_params`](https://emf-creaf.github.io/traits4models/reference/init_medfate_params.md)

## Author

Miquel De Cáceres Ainsa, EMF-CREAF

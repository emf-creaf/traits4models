# Complete strict parameters for medfate

Taxonomy-based imputation of strict parameters in medfate.

## Usage

``` r
complete_medfate_strict(
  SpParams,
  params = NULL,
  progress = TRUE,
  verbose = FALSE
)
```

## Arguments

- SpParams:

  A species parameter data frame to be completed

- params:

  A character vector with strict parameters to be completed. If missing,
  all strict parameters will be checked for completion.

- progress:

  A boolean flag to prompt progress.

- verbose:

  A boolean flag to prompt detailed process information.

## Value

A species parameter data frame with imputed strict parameter values

## Details

The function performs imputation of strict parameters on the basis of
non-missing values in taxonomically-close taxa. Strict parameters are
defined as such in
[`SpParamsDefinition`](https://emf-creaf.github.io/medfate/reference/SpParams.html).
First, if the taxon with missing values is a subspecies, the function
looks for values at the species level. Missing values at the species
level are filled with data at the genus, family, order or group levels,
depending on the availability of information. Quantitative values are
averaged, whereas for qualitative values the first non-missing value is
used. Beware that imputation can result in errors if the initial amount
of information is scarce. Therefore, users should revised the output of
this function and correct potential errors manually, whenever detected.

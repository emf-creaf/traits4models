# Checks species parameter tables

Checks that the input data frame has the appropriate format and data for
simulations with medfate.

## Usage

``` r
check_medfate_params(x, verbose = TRUE)
```

## Arguments

- x:

  A data frame with species parameter data

- verbose:

  A boolean flag to prompt detailed process information.

## Value

An (invisible) data frame with missing values in strict parameters.

## Details

The function performs the following checks:

- The input is a data frame.

- All parameter names defined in
  [`SpParamsDefinition`](https://emf-creaf.github.io/medfate/reference/SpParams.html)
  should be listed as columns.

- Strict parameters should not contain missing values. Strict parameters
  are defined as such in
  [`SpParamsDefinition`](https://emf-creaf.github.io/medfate/reference/SpParams.html).

## Examples

``` r
check_medfate_params(SpParamsES)
#> ✔ The data frame is acceptable as species parameter table for medfate.
check_medfate_params(SpParamsFR)
#> ✔ The data frame is acceptable as species parameter table for medfate.
check_medfate_params(SpParamsUS)
#> ✔ The data frame is acceptable as species parameter table for medfate.
check_medfate_params(SpParamsAU)
#> ✔ The data frame is acceptable as species parameter table for medfate.
```

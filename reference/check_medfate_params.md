# Checks species parameter tables

Checks that the input data frame has the appropriate format and data for
simulations with medfate.

## Usage

``` r
check_medfate_params(x, check_consistency = TRUE, verbose = TRUE)
```

## Arguments

- x:

  A data frame with species parameter data

- verbose:

  A boolean flag to prompt detailed process information.

- check_consitency:

  A boolean flag to force checking ecological and physiological
  consistence of parameter values.

## Value

An (invisible) list with two data frames, one with missing values in
strict parameters and the other with plant names whose parameters
(observed or imputed) do not conform to physiological rules.

## Details

The function can perform two kinds of parameter check.

A formal check is always performed with the following rules:

- The input is a data frame.

- All parameter names defined in
  [`SpParamsDefinition`](https://emf-creaf.github.io/medfate/reference/SpParams.html)
  should be listed as columns.

- Strict parameters should not contain missing values. Strict parameters
  are defined as such in
  [`SpParamsDefinition`](https://emf-creaf.github.io/medfate/reference/SpParams.html).

If `check_consistency = TRUE`, the function also performs the following
consistency checks for the parameter values of each species:

1.  Rule 1. Stem hydraulic vulnerability is not larger than leaf
    hydraulic vulnerability (i.e. VCstem_P50 not less negative than
    VCleaf_P50).

2.  Rule 2. Stem hydraulic vulnerability is not larger than root
    hydraulic vulnerability (i.e. VCstem_P50 not less negative than
    VCroot_P50).

3.  Rule 3. Stem hydraulic vulnerability is not larger than the water
    potential at 50% stomatal conductance (i.e. VCstem_P50 not less
    negative than Gs_P50).

4.  Rule 4. Stem hydraulic vulnerability curve is consistent (VCstem_P88
    \< VCstem_P50 \< VCstem_P12).

5.  Rule 5. Leaf hydraulic vulnerability curve is consistent (VCleaf_P88
    \< VCleaf_P50 \< VCleaf_P12).

6.  Rule 6. Root hydraulic vulnerability curve is consistent (VCroot_P88
    \< VCroot_P50 \< VCroot_P12).

Parameter consistency is conducted including imputation of missing
values, according to medfate inbuilt parameter estimation.

## Examples

``` r
check_medfate_params(SpParamsES, check_consistency = FALSE)
#> ✔ The data frame is formally acceptable as species parameter table for medfate.
check_medfate_params(SpParamsFR, check_consistency = FALSE)
#> ✔ The data frame is formally acceptable as species parameter table for medfate.
check_medfate_params(SpParamsUS, check_consistency = FALSE)
#> ✔ The data frame is formally acceptable as species parameter table for medfate.
check_medfate_params(SpParamsAU, check_consistency = FALSE)
#> ✔ The data frame is formally acceptable as species parameter table for medfate.
```

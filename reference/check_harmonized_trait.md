# Check data harmonization

Functions to check that data tables (trait tables or allometry tables)
have the appropriate format and data for model parameter filling.

## Usage

``` r
check_harmonized_trait(x, verbose = TRUE)

check_harmonized_trait_dir(
  harmonized_trait_path,
  update_version = FALSE,
  verbose = TRUE
)

check_harmonized_allometry(x, verbose = TRUE)

check_harmonized_allometry_dir(
  harmonized_allometry_path,
  update_version = FALSE,
  verbose = TRUE
)
```

## Arguments

- x:

  A data frame with harmonized trait data or harmonized allometry data.

- verbose:

  A logical flag indicating extra console output (information alerts and
  check success). If `FALSE`, only errors are reported.

- harmonized_trait_path:

  The path to harmonized trait data files (.rds or .csv format).

- update_version:

  If `TRUE` and the package is acceptable, it updates the package
  version and stores the harmonized file.

- harmonized_allometry_path:

  The path to harmonized allometry data files (.rds or .csv format).

## Value

- Functions `check_harmonized_trait()` and
  `check_harmonized_allometry()` return an (invisible) logical
  indicating whether the data frame is acceptable or not.

- Functions `check_harmonized_trait_dir()` and
  `check_harmonized_allometry_dir()` return an (invisible) logical
  vector indicating those files whose data is acceptable or not.

## Details

Function `check_harmonized_trait()` checks that the input data frame
conforms to the following requirements:

1.  Has columns called `originalName`, `acceptedName`,
    `acceptedNameAuthorship`, `family`, `genus`, `specificEpithet`, and
    `taxonRank`, as returned by function
    [`harmonize_taxonomy_WFO`](https://emf-creaf.github.io/traits4models/reference/harmonize_taxonomy_WFO.md).

2.  Trait data can be in wide-format or long-format. When supplied in
    wide format, the names of trait columns are either a valid trait
    names according to the notation in
    [`HarmonizedTraitDefinition`](https://emf-creaf.github.io/traits4models/reference/HarmonizedTraitDefinition.md)
    and the type of column needs to match the corresponding type. In
    this format trait units cannot be checked and the same reference
    applies to all traits. When supplied in long format, the following
    columns are required:

    - `Trait` - A character describing the the name of the trait, with
      values according to the notation in
      [`HarmonizedTraitDefinition`](https://emf-creaf.github.io/traits4models/reference/HarmonizedTraitDefinition.md).

    - `Value` - A character or numeric containing the trait value. For
      numeric traits, values should be coercible to numbers via
      [`as.numeric`](https://rdrr.io/r/base/numeric.html).

    - `Units` - A character describing the units of the trait, with
      values according to units in
      [`HarmonizedTraitDefinition`](https://emf-creaf.github.io/traits4models/reference/HarmonizedTraitDefinition.md).

3.  The names of the remaining columns are:

    - `Reference` - A string describing the harmonized trait data
      source, i.e. normally a reference.

    - `DOI` - A string describing the DOI of the harmonized trait data
      source.

    - `OriginalReference` - A string describing the original source of
      trait data (if `Reference` is a compilation).

    - `OriginalDOI` - A string describing the DOI of the original source
      of trait data (if `Reference` is a compilation).

    - `Priority` - A numeric value to prioritize some data values
      (sources) over others (1 means highest priority).

    - `checkVersion` - A string of the package version used for
      harmonization checking.

    Columns `OriginalReference` and `OriginalDOI` are optional, and the
    remaining columns are recommended.

4.  Trait value columns contain non-missing data.

5.  Numeric trait columns do not contain values beyond expected ranges
    (when defined).

6.  Trait columns do not contain values different than accepted values
    (when defined, typically for categorical traits). Lower-upper cases
    are ignored.

Function `check_harmonized_allometry()` checks that the input data frame
conforms to the following requirements:

1.  Has columns called `originalName`, `acceptedName`,
    `acceptedNameAuthorship`, `family`, `genus`, `specificEpithet`, and
    `taxonRank`, as returned by function
    [`harmonize_taxonomy_WFO`](https://emf-creaf.github.io/traits4models/reference/harmonize_taxonomy_WFO.md).

2.  Has columns called `Response` and `Equation`. Columns `Priority`,
    `Reference` and `DOI` are recommended.

3.  Column `Response` identifies the response variable.

4.  Column `ResponseDescription` contains a longer description of the
    response variable.

5.  Columns `Predictor1`, `Predictor2`, ... identify predictor
    variables.

6.  Columns `PredictorDescription1`, `PredictorDescription2`, ...
    contain longer descriptions of predictor variables.

7.  Allometry coefficient columns should be named `a`, `b`, `c`, ...,
    `f` and contain numeric values.

Functions `check_harmonized_trait_dir()` and
`check_harmonized_allometry_dir()` allow checking the appropriateness of
multiple harmonized files at once.

## See also

[`harmonize_taxonomy_WFO`](https://emf-creaf.github.io/traits4models/reference/harmonize_taxonomy_WFO.md),
[`HarmonizedTraitDefinition`](https://emf-creaf.github.io/traits4models/reference/HarmonizedTraitDefinition.md)

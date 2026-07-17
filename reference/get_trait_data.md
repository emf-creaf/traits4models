# Load harmonized trait/allometry data

Functions to load harmonized trait data and harmonized allometry data

## Usage

``` r
load_harmonized_trait_tables(
  harmonized_trait_path,
  check = TRUE,
  progress = TRUE
)

load_harmonized_allometry_tables(
  harmonized_allometry_path,
  check = TRUE,
  progress = TRUE
)

get_trait_data(
  harmonized_trait_path,
  trait_name,
  taxon_selection_column = NULL,
  taxon_selection = NULL,
  output_format = "long",
  progress = TRUE,
  verbose = TRUE
)

get_allometry_data(
  harmonized_allometry_path,
  response,
  progress = TRUE,
  verbose = TRUE
)

get_taxon_data(
  harmonized_path,
  accepted_name,
  type = "trait",
  progress = TRUE,
  verbose = TRUE
)
```

## Arguments

- harmonized_trait_path:

  The path to harmonized trait data files (.rds or .csv format).

- check:

  A boolean flag to check harmonization and exclude non-acceptable data.

- progress:

  A boolean flag to prompt progress.

- harmonized_allometry_path:

  The path to harmonized allometry data files (.rds or .csv format).

- trait_name:

  A string of an accepted trait name, according to
  [`HarmonizedTraitDefinition`](https://emf-creaf.github.io/traits4models/reference/HarmonizedTraitDefinition.md).

- taxon_selection_column:

  String indicating the column (e.g. "originalName", "acceptedName",
  "genus" or "family") that will be used for taxon selection. By
  default, all taxa are returned.

- taxon_selection:

  String vector with taxon names to perform selection. By default, all
  taxa are returned.

- output_format:

  Either "long" or "wide", to indicate output format for trait columns.

- verbose:

  A boolean flag to prompt detailed process information.

- response:

  String indicating a response variable for allometric equations.

- harmonized_path:

  The path to harmonized trait or allometry data files (.rds or .csv
  format).

- accepted_name:

  String of an accepted taxon name.

- type:

  A string with data type, either "trait" or "allometry".

## Value

Function `load_harmonized_trait_tables()` returns the list with all
harmonized trait data tables.

Function `load_harmonized_allometry_tables()` returns the list with all
harmonized allometry data tables.

Function `get_trait_data()` returns a data frame (in long trait format)
with the pooled information for a given trait, or an empty data frame if
not found.

Function `get_taxon_data()` returns a data frame (in long trait format)
with the pooled information (traits or allometries) for a given taxon,
or an empty data frame if not found.

Function `get_allometry_data()` returns a data frame with allometric
equations.

## Details

Functions `load_harmonized_trait_tables()` and
`load_harmonized_allometry_tables()` allows loading all kinds of trait
data tables, including those that do not pass harmonization check, if
`check = FALSE`. In contrast, functions `get_trait_data()`,
`get_taxon_data()` and `get_taxon_trait_means()` only return data from
data sets passing harmonization checks (see functions
[`check_harmonized_trait`](https://emf-creaf.github.io/traits4models/reference/check_harmonized_trait.md)
and
[`check_harmonized_allometry`](https://emf-creaf.github.io/traits4models/reference/check_harmonized_trait.md)).

## See also

[`check_harmonized_trait`](https://emf-creaf.github.io/traits4models/reference/check_harmonized_trait.md)

## Examples

``` r
if (FALSE) { # \dontrun{
 # List harmonized trait files
 DB_path <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/traits_and_models/"
 harmonized_trait_path <- paste0(DB_path,"data/harmonized_trait_sources")
 list.files(path = harmonized_trait_path)

 # Load all harmonized trait data
 l <- load_harmonized_trait_tables(harmonized_trait_path)
 head(l[[1]])

 # Get data for one specific trait
 get_trait_data(harmonized_trait_path, "SLA")

 # Get trait data for one specific taxon
 get_taxon_data(harmonized_trait_path, "Pinus halepensis")

 # List harmonized allometry files
 harmonized_allometry_path = "~/OneDrive/EMF_datasets/AllometryDatabases/Products/harmonized"
 list.files(path = harmonized_allometry_path)

 # Load all harmonized allometry data
 l <- load_harmonized_allometry_tables(harmonized_allometry_path)
 head(l[[1]])

 # Get allometry data for one specific response
 get_allometry_data(harmonized_allometry_path, "FoliarBiomass")
} # }
```

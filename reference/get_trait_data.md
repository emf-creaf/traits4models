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
  output_format = "long",
  progress = TRUE
)

get_allometry_data(harmonized_allometry_path, response, progress = TRUE)

get_taxon_data(harmonized_trait_path, accepted_name, progress = TRUE)

get_taxon_trait_means(
  harmonized_trait_path,
  taxon_level = "species",
  traits = NULL,
  progress = TRUE
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

- output_format:

  Either "long" or "wide", to indicate output format for trait columns.

- response:

  String indicating a response variable for allometric equations.

- accepted_name:

  String of an accepted taxon name.

- taxon_level:

  Taxon level for grouping: either 'species' (acceptedName), 'genus' or
  'family'

- traits:

  A character vector with the set of traits to summarize. If `NULL`,
  then all available traits are summarized.

## Value

Function `load_harmonized_trait_tables()` returns the list with all
harmonized trait data tables.

Function `load_harmonized_allometry_tables()` returns the list with all
harmonized allometry data tables.

Function `get_trait_data()` returns a data frame (in long trait format)
with the pooled information for a given trait, or an empty data frame if
not found.

Function `get_taxon_data()` returns a data frame (in long trait format)
with the pooled information for a given taxon, or an empty data frame if
not found.

Function `get_taxon_trait_means()` returns a data frame with taxon
averages (or most frequent values) for the set of indicated traits.

Function `get_allometry_data()` returns a data frame with allometric
equations.

## Details

Function `load_harmonized_trait_tables()` allows loading all kinds of
trait data tables, including those that do not pass harmonization check,
if `check = FALSE`. In contrast, functions `get_trait_data()`,
`get_taxon_data()` and `get_taxon_trait_means()` only return data from
data sets passing harmonization checks (see function
[`check_harmonized_trait`](https://emf-creaf.github.io/traits4models/reference/check_harmonized_trait.md)).

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

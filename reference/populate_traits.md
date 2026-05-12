# Populate species parameters from trait data

Internal function to fill medfate species trait parameters from a data
table of species traits

## Usage

``` r
populate_traits(
  SpParams,
  trait_table,
  trait_mapping,
  taxon_column,
  genus_column = NULL,
  priority_column = NULL,
  character_traits = FALSE,
  summary_function = "median",
  summary_params = list(na.rm = TRUE),
  scalar_functions = NULL,
  replace_previous = FALSE,
  erase_previous = FALSE,
  verbose = FALSE
)
```

## Arguments

- SpParams:

  A data frame of medfate species parameters to be populated.

- trait_table:

  A data frame with functional traits in columns and plants in rows.

- trait_mapping:

  A named string vector specifying which trait data column should be
  used to populate each medfate param. Elements are data base columns
  and names are medfate params.

- taxon_column:

  A string identifying the column in `trait_table` that identifies taxon
  names).

- genus_column:

  A string identifying the column in `trait_table` that identifies
  genus. If `NULL` then genus are taken from first word of taxon names.

- priority_column:

  A string identifying the column in `trait_table` that identifies the
  priority of some data sources above others (lower values of priority
  are processed earlier).

- character_traits:

  Boolean flag to treat traits as character-valued

- summary_function:

  A function to summarize multiple values for the same taxonomic entity.
  By default, median values are taken for quantitative traits and the
  most frequent value is taken for qualitative traits.

- summary_params:

  A list of summary function params (by default `na.rm=TRUE`).

- scalar_functions:

  A named list of scalar functions for traits needing transformation of
  units or scaling. Names are medfate params.

- replace_previous:

  A boolean flag to indicate that non-missing previous values should be
  replaced with new data

- erase_previous:

  A boolean flag to indicate that all previous values should be set to
  NA before populating with new data

- verbose:

  A boolean flag to indicate extra console output

## Value

A modified data frame of medfate species parameters

## Details

Matches column 'AcceptedName' of SpParams with trait parameter sources.
If the target taxon is a species, values are taken from those rows in
trait_table where species names match. If the target taxon is a genus,
then values are taken from those rows where genus is the same. If column
`priority_column` is supplied, the function will prioritize sources with
higher priority first.

## See also

[`init_medfate_params`](https://emf-creaf.github.io/traits4models/reference/init_medfate_params.md)

## Author

Miquel De Cáceres Ainsa, EMF-CREAF

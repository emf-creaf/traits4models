# Initializes species parameter table

Creates an empty plant parameter table for medfate, populating taxonomic
information if desired.

## Usage

``` r
init_medfate_params(x, complete_rows = TRUE, sort = TRUE, verbose = FALSE)
```

## Arguments

- x:

  A data frame with columns as returned by function
  [`harmonize_taxonomy_WFO`](https://emf-creaf.github.io/traits4models/reference/harmonize_taxonomy_WFO.md).

- complete_rows:

  Boolean flag to indicate that extra rows should be added for cited
  species/genera (if `fill_taxonomy = TRUE`).

- sort:

  Boolean flag to force sorting in ascending order by `Name`.

- verbose:

  A boolean flag to indicate extra console output.

## Value

A data frame with empty species parameter values suitable for medfate
simulations. The data frame will normally contain more rows if
`complete_rows` is set to TRUE.

## Details

Taxonomy is taken from the input data frame, and internal data obtained
from package taxize is used for the taxa above family, only.

## See also

[`SpParamsMED`](https://emf-creaf.github.io/medfate/reference/SpParams.html)

## Author

Miquel De Cáceres Ainsa, EMF-CREAF

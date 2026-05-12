# Fills parameters from inventory data

Extracts species parameter values from forest inventory data

## Usage

``` r
fill_medfate_inventory_traits(
  SpParams,
  x,
  quantile_Hmed = 0.5,
  quantile_Hmax = 0.99,
  quantile_fHDmin = 0.05,
  quantile_fHDmax = 0.95,
  progress = TRUE,
  verbose = FALSE
)
```

## Arguments

- SpParams:

  A species parameter data frame to be filled.

- x:

  A list of
  [`forest`](https://emf-creaf.github.io/medfate/reference/forest.html)
  objects, or a data frame with a column called `forest`, whose elements
  are of class
  [`forest`](https://emf-creaf.github.io/medfate/reference/forest.html).

- quantile_Hmed:

  Quantile for Hmed

- quantile_Hmax:

  Quantile for Hmax

- quantile_fHDmin:

  Quantile for fHDmin

- quantile_fHDmax:

  Quantile for fHDmax

- progress:

  A boolean flag to prompt progress.

- verbose:

  A boolean flag to indicate extra console output.

## Value

A modified data frame of medfate species parameters

## Details

This function fills information of the species parameter table from the
data of the target forest inventory where simulations are to be
conducted. Matching is performed between \`Species\` of the forest
inventory data and \`Name\` of the species parameter table. The
following information is extracted:

- `GrowthForm`: Growth form according to the usage in the forest
  inventory. For example, if the species is cited in `treeData` tables
  but not in `shrubData` tables, then growth form will be `"Tree"`.

- `Hmax`: Maximum tree/shrub height (cm), according to the \`Height\`
  column in `treeData` or `shrubData` and `quantile_Hmax` parameter.

- `Hmed`: Median tree/shrub height (cm), according to the \`Height\`
  column in `treeData` or `shrubData` and `quantile_Hmed` parameter.

- `fHDmin, fHDmax`: Minimum or maximum height to diameter ratio for
  trees, according to the \`Height\` and \`DBH\` columns in `treeData`
  and `quantile_fHDmin` or `quantile_fHDmax` parameters, respectively.

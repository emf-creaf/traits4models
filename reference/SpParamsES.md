# Data tables with species parameter definition and values for different countries

A data sets of species parameter tables resulting from existing
databases to be used in conjunction with national forest inventory data.

## Format

- Data frames `SpParamsES` (for Spain), `SpParamsFR` (for France),
  `SpParamsAU` (for Australia), `SpParamsUS` (for US) and `SpParamsZM`
  (for Zambia) have species or genus as rows and column names equal to
  parameter names in `SpParamsDefinition` (the latter from package
  `medfate`).

## Details

`SpParamsES`, `SpParamsFR`, `SpParamsAU`, `SpParamsUS` and `SpParamsZM`
are species parameter data frames designed to be used with National
Forest Inventories of Spain, France, Australia and USA, respectively.

Details of the procedures used to obtain the species parameter tables
can be found in an article at
https://emf-creaf.github.io/traits4models/.

## Examples

``` r
data(SpParamsES)
data(SpParamsFR)
data(SpParamsUS)
data(SpParamsAU)
data(SpParamsZM)
```

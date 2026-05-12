# Harmonizes taxonomy

Harmonizes plant taxonomy according to World Flora Online

## Usage

``` r
harmonize_taxonomy_WFO(db, WFO_backbone_file, progress = TRUE, verbose = FALSE)
```

## Arguments

- db:

  Data frame to harmonize, with species names in a column called
  'originalName'. The data frame will normally include other columns
  which are transferred unaltered to the output data frame.

- WFO_backbone_file:

  Path to file containing the backbone of World Flora Online.

- progress:

  A boolean flag to prompt progress.

- verbose:

  A boolean flag to print console output with matching information.

## Value

A data frame with columns:

- `originalName`: Original taxon name given in the input data frame.

- `acceptedName`: Accepted taxon name according to World Flora Online.

- `acceptedNameAuthorship`: Accepted taxon name with authorship
  according to World Flora Online.

- `family`: Taxonomic family of the accepted taxon.

- `genus`: Taxonomic genus of the accepted taxon.

- `specificEpithet`: Specific epithet of the accepted taxon.

- `taxonRank`: Taxonomic rank of the accepted taxon (e.g. "species",
  "subspecies", "genus", ...).

Additional columns may be present, coming from the input data frame.
These are left unmodified.

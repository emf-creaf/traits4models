# Allometry database harmonization

## Introduction

The availability of plant allometric equations continuously increases,
as additional efforts are made in observational studies. This means that
multiple data sources need to be harmonized before species parameter
tables are build, in terms of:

    a) Nomenclature of the response and predictor variables.
    b) Allometric equation fitted.
    c) Taxonomy of the measured biological entities. 

The result of this harmonization needs to be stored in a harmonized
format for subsequent use when creating species parameter tables. This
vignette illustrates the harmonization procedures for an example data
set using package **trait4models** and the usual **tidyverse** packages.
Harmonization of plant trait data is explained in a companion vignette.

### Required packages

Assuming we have **traits4models** installed, we load it and other
common packages that we will employ in this vignette:

``` r

library(traits4models)
library(tidyverse)
library(readr)
```

### Example dataset

As an example of allometric equations, we will use a small compilation
of crown width models, from different sources, that depend on diameter
at breast height:

``` r

DB_path <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/traits_and_models/"
db <- openxlsx::read.xlsx(paste0(DB_path, "data-raw/raw_allometry_data/TreeAllometries/TreeAllometries.xlsx"),
                                 sheet= "Tree_CW_models")
```

The data looks as follows:

``` r

db
```

    ##                 Name      a_cw   b_cw                           Source
    ## 1         Abies alba 1.0963648 0.5380    Hasenauer (1997) - Abies alba
    ## 2               Acer 1.5189207 0.5285     Hasenauer (1997) - Acer spp.
    ## 3    Castanea sativa 1.3065630 0.5856             Condés & Sterba 2005
    ## 4    Fagus sylvatica 0.8386180 0.7347             Condés & Sterba 2005
    ## 5           Fraxinus 1.1951838 0.5665 Hasenauer (1997) - Fraxinus spp.
    ## 6   Pinus halepensis 0.6415296 0.7310             Condés & Sterba 2005
    ## 7        Pinus nigra 0.5076312 0.7699             Condés & Sterba 2005
    ## 8     Pinus pinaster 0.3889517 0.8307             Condés & Sterba 2005
    ## 9      Pinus radiata 0.8418950 0.6353             Condés & Sterba 2005
    ## 10  Pinus sylvestris 0.7474409 0.6716             Condés & Sterba 2005
    ## 11     Quercus ilex  0.5681897 0.7974             Condés & Sterba 2005
    ## 12 Quercus pyrenaica 0.7469926 0.7257             Condés & Sterba 2005
    ## 13     Quercus robur 0.0716000 0.6563             Condés & Sterba 2005
    ## 14     Quercus suber 0.4473000 0.7918     Sánchez-Gonzalez et al. 2007
    ## 15             Pinus 0.7474409 0.6716                 Pinus sylvestris
    ## 16           Quercus 0.5681897 0.7974                           Q.ilex
    ## 17             Abies 1.0963648 0.5380                       Abies alba

## Harmonizing equations and notation

The allometric equation has two parameters that are used to estimate
crown width from diameter at breast height (DBH). We start our
harmonization by subsetting the necessary data:

``` r

db_var <- db |>
  dplyr::rename(originalName = "Name",
                a = "a_cw",
                b = "b_cw",
                Reference = "Source") 
```

Importantly, for the allometry to be used, we need to specify the
equation:

``` r

db_var <- db_var |>
  dplyr::mutate(Equation = "CrownWidth = a·DBH^b")
```

We also add information on the response variable and predictor variable:

``` r

db_var <- db_var |>
                dplyr::mutate(Response = "CrownWidth",
                ResponseDescription = "Crown width (m)",
                Predictor1 = "DBH",
                PredictorDescription1 = "Diameter at breast height (cm)")
```

If we want **traits4models** to correctly interpret the allometric
equation at the stage of model parameter filling, it is important that
columns `Equation` and `Response` are appropriately filled. Note, in
addition, that the allometric equation starts with the name of the
response on the left hand side of the equation.

Finally, we may add the priority of processing:

``` r

db_var <- db_var |>
          dplyr::mutate(Priority = 1) |>
          dplyr::relocate(Reference, .after = PredictorDescription1)
```

The database after harmonizing allometry information looks as follows:

``` r

db_var
```

    ##         originalName         a      b             Equation   Response
    ## 1         Abies alba 1.0963648 0.5380 CrownWidth = a·DBH^b CrownWidth
    ## 2               Acer 1.5189207 0.5285 CrownWidth = a·DBH^b CrownWidth
    ## 3    Castanea sativa 1.3065630 0.5856 CrownWidth = a·DBH^b CrownWidth
    ## 4    Fagus sylvatica 0.8386180 0.7347 CrownWidth = a·DBH^b CrownWidth
    ## 5           Fraxinus 1.1951838 0.5665 CrownWidth = a·DBH^b CrownWidth
    ## 6   Pinus halepensis 0.6415296 0.7310 CrownWidth = a·DBH^b CrownWidth
    ## 7        Pinus nigra 0.5076312 0.7699 CrownWidth = a·DBH^b CrownWidth
    ## 8     Pinus pinaster 0.3889517 0.8307 CrownWidth = a·DBH^b CrownWidth
    ## 9      Pinus radiata 0.8418950 0.6353 CrownWidth = a·DBH^b CrownWidth
    ## 10  Pinus sylvestris 0.7474409 0.6716 CrownWidth = a·DBH^b CrownWidth
    ## 11     Quercus ilex  0.5681897 0.7974 CrownWidth = a·DBH^b CrownWidth
    ## 12 Quercus pyrenaica 0.7469926 0.7257 CrownWidth = a·DBH^b CrownWidth
    ## 13     Quercus robur 0.0716000 0.6563 CrownWidth = a·DBH^b CrownWidth
    ## 14     Quercus suber 0.4473000 0.7918 CrownWidth = a·DBH^b CrownWidth
    ## 15             Pinus 0.7474409 0.6716 CrownWidth = a·DBH^b CrownWidth
    ## 16           Quercus 0.5681897 0.7974 CrownWidth = a·DBH^b CrownWidth
    ## 17             Abies 1.0963648 0.5380 CrownWidth = a·DBH^b CrownWidth
    ##    ResponseDescription Predictor1          PredictorDescription1
    ## 1      Crown width (m)        DBH Diameter at breast height (cm)
    ## 2      Crown width (m)        DBH Diameter at breast height (cm)
    ## 3      Crown width (m)        DBH Diameter at breast height (cm)
    ## 4      Crown width (m)        DBH Diameter at breast height (cm)
    ## 5      Crown width (m)        DBH Diameter at breast height (cm)
    ## 6      Crown width (m)        DBH Diameter at breast height (cm)
    ## 7      Crown width (m)        DBH Diameter at breast height (cm)
    ## 8      Crown width (m)        DBH Diameter at breast height (cm)
    ## 9      Crown width (m)        DBH Diameter at breast height (cm)
    ## 10     Crown width (m)        DBH Diameter at breast height (cm)
    ## 11     Crown width (m)        DBH Diameter at breast height (cm)
    ## 12     Crown width (m)        DBH Diameter at breast height (cm)
    ## 13     Crown width (m)        DBH Diameter at breast height (cm)
    ## 14     Crown width (m)        DBH Diameter at breast height (cm)
    ## 15     Crown width (m)        DBH Diameter at breast height (cm)
    ## 16     Crown width (m)        DBH Diameter at breast height (cm)
    ## 17     Crown width (m)        DBH Diameter at breast height (cm)
    ##                           Reference Priority
    ## 1     Hasenauer (1997) - Abies alba        1
    ## 2      Hasenauer (1997) - Acer spp.        1
    ## 3              Condés & Sterba 2005        1
    ## 4              Condés & Sterba 2005        1
    ## 5  Hasenauer (1997) - Fraxinus spp.        1
    ## 6              Condés & Sterba 2005        1
    ## 7              Condés & Sterba 2005        1
    ## 8              Condés & Sterba 2005        1
    ## 9              Condés & Sterba 2005        1
    ## 10             Condés & Sterba 2005        1
    ## 11             Condés & Sterba 2005        1
    ## 12             Condés & Sterba 2005        1
    ## 13             Condés & Sterba 2005        1
    ## 14     Sánchez-Gonzalez et al. 2007        1
    ## 15                 Pinus sylvestris        1
    ## 16                           Q.ilex        1
    ## 17                       Abies alba        1

## Taxonomic harmonization

Package **traits4models** currently relies on [World Flora
Online](https://www.worldfloraonline.org/) for taxonomic harmonization,
via package **WorldFlora** (Kindt 2020) available at
[CRAN](https://cran.r-project.org/package=WorldFlora). This latter
package requires a static copy of the World Flora Online Taxonomic
Backbone data that can be downloaded from the [World Flora
Online](https://www.worldfloraonline.org/) website. Note that you should
use a different DOI reference when reporting your harmonization
procedures. We are using here
[v.2024.06](https://doi.org/10.5281/zenodo.7460141). We assume the user
has already downloaded the backbone and stored it in a file called
*classification.csv*.

``` r

WFO_file <- paste0(DB_path, 
                   "data-raw/wfo_backbone/classification.csv")
```

Taxonomic harmonization is done by calling function
[`harmonize_taxonomy_WFO()`](https://emf-creaf.github.io/traits4models/reference/harmonize_taxonomy_WFO.md)
with a data frame (where notation and units are already harmonized) and
the path to the WFO backbone (we omit the console output):

``` r

db_post <- traits4models::harmonize_taxonomy_WFO(db_var, WFO_file)
```

The function requires that the input data frame contains a column called
`originalName`, to identify the original taxa names (additional columns
are simply transferred to the output). It performs both direct and fuzzy
matching, which may lead to large processing time for large datasets. If
we inspect the resulting data frame, we will see the additional columns,
informing about accepted names and parent taxonomic entities:

``` r

head(db_post)
```

    ##       originalName     acceptedName acceptedNameAuthorship      family    genus
    ## 1       Abies alba       Abies alba                  Mill.    Pinaceae    Abies
    ## 2             Acer             Acer                     L. Sapindaceae     Acer
    ## 3  Castanea sativa  Castanea sativa                  Mill.    Fagaceae Castanea
    ## 4  Fagus sylvatica  Fagus sylvatica                     L.    Fagaceae    Fagus
    ## 5         Fraxinus         Fraxinus           Tourn. ex L.    Oleaceae Fraxinus
    ## 6 Pinus halepensis Pinus halepensis                  Mill.    Pinaceae    Pinus
    ##   specificEpithet taxonRank         a      b             Equation   Response
    ## 1            alba   species 1.0963648 0.5380 CrownWidth = a·DBH^b CrownWidth
    ## 2            <NA>     genus 1.5189207 0.5285 CrownWidth = a·DBH^b CrownWidth
    ## 3          sativa   species 1.3065630 0.5856 CrownWidth = a·DBH^b CrownWidth
    ## 4       sylvatica   species 0.8386180 0.7347 CrownWidth = a·DBH^b CrownWidth
    ## 5            <NA>     genus 1.1951838 0.5665 CrownWidth = a·DBH^b CrownWidth
    ## 6      halepensis   species 0.6415296 0.7310 CrownWidth = a·DBH^b CrownWidth
    ##   ResponseDescription Predictor1          PredictorDescription1
    ## 1     Crown width (m)        DBH Diameter at breast height (cm)
    ## 2     Crown width (m)        DBH Diameter at breast height (cm)
    ## 3     Crown width (m)        DBH Diameter at breast height (cm)
    ## 4     Crown width (m)        DBH Diameter at breast height (cm)
    ## 5     Crown width (m)        DBH Diameter at breast height (cm)
    ## 6     Crown width (m)        DBH Diameter at breast height (cm)
    ##                          Reference Priority
    ## 1    Hasenauer (1997) - Abies alba        1
    ## 2     Hasenauer (1997) - Acer spp.        1
    ## 3             Condés & Sterba 2005        1
    ## 4             Condés & Sterba 2005        1
    ## 5 Hasenauer (1997) - Fraxinus spp.        1
    ## 6             Condés & Sterba 2005        1

## Checking harmonized allometry data

The packages includes function
[`check_harmonized_allometry()`](https://emf-creaf.github.io/traits4models/reference/check_harmonized_trait.md)
to check whether a given data frame conforms in structure and content to
what is later required for parameter table filling:

``` r

check_harmonized_allometry(db_post)
```

    ## ℹ Column 'DOI' should preferably be defined.

    ## ℹ Column 'checkVersion' should preferably be defined.

    ## ✔ The data frame is acceptable as harmonized allometry data source.

In this case, the data set is ready to be used in parameter estimation.
As an example, we can run the same checking function with the data base
before taxonomic harmonization:

``` r

check_harmonized_allometry(db_var)
```

    ## ! Required columns missing: acceptedName acceptedNameAuthorship family genus specificEpithet taxonRank

    ## ℹ Column 'DOI' should preferably be defined.

    ## ℹ Column 'checkVersion' should preferably be defined.

    ## ! The data frame is not acceptable as harmonized allometry data source.

## Storing harmonized dataset

Harmonized data tables can be stored in **.csv** text format or
compressed **.rds** format.

``` r

file_out <- paste0(DB_path, "data/harmonized_allometry_sources/tree_crown_width_europe.csv")
write.csv2(db_post, file_out, row.names = FALSE)
```

Note that all tables should be stored in the same directory, here in the
`data/harmonized_allometry_sources/` path, for populate model
parameters.

## Accessing harmonized allometry data

### List of harmonized allometry databases

As mentioned in the introduction, here it is assumed that a set of plant
allometry databases have been harmonized. In our case, harmonized data
files have been stored in the following path:

``` r

harmonized_allometry_path <- paste0(DB_path, "data/harmonized_allometry_sources")
```

We can list the set of harmonized sources using:

``` r

allometry_files <- list.files(path = harmonized_allometry_path, full.names = FALSE)
allometry_files
```

    ##  [1] "shrub_crown_area_catalonia.csv"                  
    ##  [2] "shrub_fine_fuel_biomass_catalonia.csv"           
    ##  [3] "shrub_total_biomass_catalonia.csv"               
    ##  [4] "tree_bark_thickness_catalonia.csv"               
    ##  [5] "tree_crown_ratio_europe.csv"                     
    ##  [6] "tree_crown_width_europe.csv"                     
    ##  [7] "tree_foliar_biomass_calvoalvarado_et_al_2024.csv"
    ##  [8] "tree_foliar_biomass_catalonia.csv"               
    ##  [9] "tree_foliar_biomass_eslamdoust_et_al_2017.csv"   
    ## [10] "tree_foliar_biomass_thom_et_al_2024.csv"

### Querying allometry data for particular response variable

Before filling any species parameter table, it may be useful to inspect
the amount of information available for particular response. Package
**trait4models** provides function
[`get_allometry_data()`](https://emf-creaf.github.io/traits4models/reference/get_trait_data.md):

``` r

fb_data <- get_allometry_data(harmonized_allometry_path, "FoliarBiomass",
                              progress = FALSE)
head(fb_data)
```

    ##              originalName           acceptedName acceptedNameAuthorship
    ## 1 Tetragastris panamensis    Protium stevensonii         (Standl.) Daly
    ## 2         Virola koschnii        Virola koschnyi                  Warb.
    ## 3       Carapa guianensis      Carapa guianensis                  Aubl.
    ## 4     Vochysia ferruginea    Vochysia ferruginea                  Mart.
    ## 5  Pentaclethra macroloba Pentaclethra macroloba        (Willd.) Kuntze
    ## 6              Abies alba             Abies alba                  Mill.
    ##          family        genus specificEpithet taxonRank
    ## 1   Burseraceae      Protium     stevensonii   species
    ## 2 Myristicaceae       Virola        koschnyi   species
    ## 3     Meliaceae       Carapa      guianensis   species
    ## 4  Vochysiaceae     Vochysia      ferruginea   species
    ## 5      Fabaceae Pentaclethra       macroloba   species
    ## 6      Pinaceae        Abies            alba   species
    ##                                         Equation         a        b
    ## 1                        FoliarBiomass = a·DBH^b 0.0400000 1.737000
    ## 2                        FoliarBiomass = a·DBH^b 0.0020000 2.468000
    ## 3                        FoliarBiomass = a·DBH^b 0.0120000 2.089000
    ## 4                        FoliarBiomass = a·DBH^b 0.6730000 1.058000
    ## 5                        FoliarBiomass = a·DBH^b 0.9580000 0.757000
    ## 6 FoliarBiomass = a·DBH^b·exp(c·BAL)·DBH^(d·BAL) 0.1231138 1.452404
    ##                                                                                                                                                                                                  Reference
    ## 1 Calvo-Alvarado et al. (2008) Allometric relationships predicting foliar biomass and leaf area:sapwood area ratio from tree height in five Costa Rican rain forest species. Tree Physiology 28: 1601-1608
    ## 2 Calvo-Alvarado et al. (2008) Allometric relationships predicting foliar biomass and leaf area:sapwood area ratio from tree height in five Costa Rican rain forest species. Tree Physiology 28: 1601-1608
    ## 3 Calvo-Alvarado et al. (2008) Allometric relationships predicting foliar biomass and leaf area:sapwood area ratio from tree height in five Costa Rican rain forest species. Tree Physiology 28: 1601-1608
    ## 4 Calvo-Alvarado et al. (2008) Allometric relationships predicting foliar biomass and leaf area:sapwood area ratio from tree height in five Costa Rican rain forest species. Tree Physiology 28: 1601-1608
    ## 5 Calvo-Alvarado et al. (2008) Allometric relationships predicting foliar biomass and leaf area:sapwood area ratio from tree height in five Costa Rican rain forest species. Tree Physiology 28: 1601-1608
    ## 6                                                                                                                                                                                          Calculated IEFC
    ##        Response ResponseDescription Predictor1          PredictorDescription1
    ## 1 FoliarBiomass Foliar biomass (kg)        DBH Diameter at breast height (cm)
    ## 2 FoliarBiomass Foliar biomass (kg)        DBH Diameter at breast height (cm)
    ## 3 FoliarBiomass Foliar biomass (kg)        DBH Diameter at breast height (cm)
    ## 4 FoliarBiomass Foliar biomass (kg)        DBH Diameter at breast height (cm)
    ## 5 FoliarBiomass Foliar biomass (kg)        DBH Diameter at breast height (cm)
    ## 6 FoliarBiomass Foliar biomass (kg)        DBH Diameter at breast height (cm)
    ##   Priority checkVersion  c  d Predictor2              PredictorDescription2
    ## 1        2        0.2.3 NA NA       <NA>                               <NA>
    ## 2        2        0.2.3 NA NA       <NA>                               <NA>
    ## 3        2        0.2.3 NA NA       <NA>                               <NA>
    ## 4        2        0.2.3 NA NA       <NA>                               <NA>
    ## 5        2        0.2.3 NA NA       <NA>                               <NA>
    ## 6        1        0.2.3  0  0        BAL Basal area of larger trees (m2/ha)

## References

Kindt, Roeland. 2020. “WorldFlora: An R Package for Exact and Fuzzy
Matching of Plant Names Against the World Flora Online Taxonomic
Backbone Data.” *Applications in Plant Sciences* 8 (9): e11388.
<https://doi.org/10.1002/aps3.11388>.

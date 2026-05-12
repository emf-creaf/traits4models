# Building species parameter tables for medfate

## Introduction

### Aim

This vignette describes the procedures used to create and fill species
parameter data tables (such as `SpParamsES`, `SpParamsFR` and
`SpParamsUS` included in package **traits4models**) for simulations with
packages **medfate** and **medfateland**. Here it is assumed that a set
of plant trait and allometry databases have been harmonized and will be
used as source data for parameter estimation (see vignettes [Trait
database
harmonization](https://emf-creaf.github.io/traits4models/articles/TraitDatabaseHarmonization.html)
and [Allometry database
harmonization](https://emf-creaf.github.io/traits4models/articles/AllometryDatabaseHarmonization.html)).

> **IMPORTANT**: This vignette is not self-contained, in the sense that
> it cannot be reproduced without access to data sets that are not
> included. Nevertheless, it is intended to serve as example of species
> parameterization for other regions.

### Required packages

Assuming we have **medfate** and **traits4models** correctly installed,
we load them and other common packages that we will employ in this
vignette:

``` r

library(medfate)
library(traits4models)
library(tidyverse)
library(openxlsx)
library(sf)
```

### Target parameters

The set of species parameters needed to run the models included in
**medfate** are described in a data frame called `SpParamsDefinition`
included in the package. It is obvious that **medfate** requires a sheer
number of parameters cannot be searched for manually, even for a small
number of target species. Hence, it is important to draw values from
plant allometry and plant trait data bases. Not all model parameters
have their counterpart in publicly-available databases, and even when
using data bases it is unlikely that one will find appropriate parameter
values for all species. To help with this situation, **medfate** has
*inbuilt imputation procedures* to estimate missing values for many
parameters before starting model simulations. However, imputed parameter
estimates will normally be worse than estimates coming from data bases.
Hence, process-based model users should put a strong effort in finding
source parameter data before starting simulations.

### Steps to build a species parameter table

The following sections describe different steps that we used to obtain
the species parameter table, which can be grouped into six main steps:

| \# | Step | Function(s) |
|----|----|----|
| 1 | Initialize the parameter table with target taxonomic entities | [`init_medfate_params()`](https://emf-creaf.github.io/traits4models/reference/init_medfate_params.md) |
| 2 | Populate species parameters from forest inventory data | [`fill_medfate_inventory_traits()`](https://emf-creaf.github.io/traits4models/reference/fill_medfate_inventory_traits.md) |
| 3 | Populate plant allometric coefficients from suitable databases | [`fill_medfate_allometries()`](https://emf-creaf.github.io/traits4models/reference/fill_medfate_allometries.md) |
| 4 | Populate plant functional traits from harmonized data bases | [`fill_medfate_traits()`](https://emf-creaf.github.io/traits4models/reference/fill_medfate_traits.md) |
| 5 | Checking the final parameter table | [`check_medfate_params()`](https://emf-creaf.github.io/traits4models/reference/check_medfate_params.md) |
| 6 | If necessary, completing strict parameters | [`complete_medfate_strict()`](https://emf-creaf.github.io/traits4models/reference/complete_medfate_strict.md) |

The remainder of this vignette illustrates each of these steps.

## Initializing a species parameter table

### Target taxonomic entities

The species parameter table for **medfate** starts with a vector of
species names. Here, we will assume that those names correspond to taxa
(genus, species, subspecies, …) mentioned in a national forest
inventory, more specifically the Spanish National Forest Inventory
(SFI). Note that taxon names in a forest inventory will correspond with
concepts of their own taxonomic reference. Hence, it is important to
harmonize taxonomy with the reference used in **traits4models**,
i.e. [World Flora Online](https://www.worldfloraonline.org/), for which
function
[`harmonize_taxonomy_WFO()`](https://emf-creaf.github.io/traits4models/reference/harmonize_taxonomy_WFO.md)
may be useful.

Here we will assume that such harmonization has been conducted for the
SFI taxon list that is included as external data in the package:

``` r

file <- system.file("extdata", "NFI_ES_mapping.csv", package = "traits4models")
```

``` r

NFI_ES_mapping <- read.table(file, sep=";", header=TRUE, na.strings = "") |>
  tibble::as_tibble() |>
  dplyr::mutate(NFICode = as.character(NFICode))
NFI_ES_mapping
```

    ## # A tibble: 480 × 10
    ##    NFICode NFIName             originalName  originalNameAuthorship acceptedName
    ##    <chr>   <chr>               <chr>         <chr>                  <chr>       
    ##  1 31      Abies alba          Abies alba    Mill.                  Abies alba  
    ##  2 32      Abies pinsapo       Abies pinsapo Boiss.                 Abies pinsa…
    ##  3 307     Acacia dealbata     Acacia dealb… Link                   Acacia deal…
    ##  4 207     Acacia melanoxylon  Acacia melan… R.Br. in W.T.Aiton     Acacia mela…
    ##  5 7       Acacia spp.         Acacia        NA                     Acacia      
    ##  6 76      Acer campestre      Acer campest… L.                     Acer campes…
    ##  7 276     Acer monspessulanum Acer monspes… L.                     Acer monspe…
    ##  8 376     Acer negundo        Acer negundo  L.                     Acer negundo
    ##  9 476     Acer opalus         Acer opalus   Mill.                  Acer opalus 
    ## 10 676     Acer platanoides    Acer platano… L.                     Acer platan…
    ## # ℹ 470 more rows
    ## # ℹ 5 more variables: acceptedNameAuthorship <chr>, family <chr>, genus <chr>,
    ## #   specificEpithet <chr>, taxonRank <chr>

Here `NFIName` corresponds to the name used in the Spanish forest
inventory, while `originalName` contains the names that where used for
taxonomy harmonization with function
[`harmonize_taxonomy_WFO()`](https://emf-creaf.github.io/traits4models/reference/harmonize_taxonomy_WFO.md),
and `acceptedName` corresponds to the accepted name according to World
Flora Online. Column `originalName` has the same values as `NFIName` for
a scientific taxa, but is otherwise missing. Non-scientific plant names
(e.g. ‘Cultivated trees’, ‘Other conifers’) or taxa names that could not
be identified cannot be used in medfate simulations because they cannot
be matched with plant trait data. Normally, we will need to filter those
names using:

``` r

spp_filt <- NFI_ES_mapping |>
  dplyr::filter(!is.na(acceptedName)) 
```

### Initialization

Package **traits4models** provides function
[`init_medfate_params()`](https://emf-creaf.github.io/traits4models/reference/init_medfate_params.md)
to initialize a species parameter table for **medfate** according to the
names given in a data frame with taxonomic information, here
`NFI_ES_mapping`. Column `originalName` will be used to match the
`Species` names in `forest` objects derived from the Spanish forest
inventory, while column `acceptedName` will be used to ensure that
matching with trait information is done using the appropriate
(harmonized) taxonomic nomenclature. Other columns are used to fill in
taxonomy:

``` r

SpParams <- traits4models::init_medfate_params(spp_filt,
                                               verbose = FALSE)
```

The result is an empty data frame, except for the first columns, which
correspond to taxonomic hierarchy of each entity (genus, family, group,
…), where taxonomic information from lower rank (up to family) comes
from the input data frame; and that of higher rank (order, group) is
completed using internal package data obtained from package **taxize**.
The number of rows in the output data frame can be larger than the
input, because by default the function creates additional rows for
species (when only sub-species of that species have been cited) or genus
(when species of that genus have been cited).

``` r

SpParams |> tibble::as_tibble()
```

    ## # A tibble: 548 × 156
    ##    Name         SpIndex AcceptedName Species Genus Family Order Group GrowthForm
    ##    <chr>          <int> <chr>        <chr>   <chr> <chr>  <chr> <chr> <lgl>     
    ##  1 Abies              0 Abies        NA      Abies Pinac… Pina… Gymn… NA        
    ##  2 Abies alba         1 Abies alba   Abies … Abies Pinac… Pina… Gymn… NA        
    ##  3 Abies pinsa…       2 Abies pinsa… Abies … Abies Pinac… Pina… Gymn… NA        
    ##  4 Acacia             3 Acacia       Acacia  Acac… Fabac… Faba… Angi… NA        
    ##  5 Acacia deal…       4 Acacia deal… Acacia… Acac… Fabac… Faba… Angi… NA        
    ##  6 Acacia mela…       5 Acacia mela… Acacia… Acac… Fabac… Faba… Angi… NA        
    ##  7 Acer               6 Acer         Acer    Acer  Sapin… Sapi… Angi… NA        
    ##  8 Acer               7 Acer         Acer    Acer  Sapin… Sapi… Angi… NA        
    ##  9 Acer campes…       8 Acer campes… Acer c… Acer  Sapin… Sapi… Angi… NA        
    ## 10 Acer monspe…       9 Acer monspe… Acer m… Acer  Sapin… Sapi… Angi… NA        
    ## # ℹ 538 more rows
    ## # ℹ 147 more variables: LifeForm <lgl>, LeafShape <lgl>, LeafSize <lgl>,
    ## #   PhenologyType <lgl>, DispersalType <lgl>, Hmed <lgl>, Hmax <lgl>,
    ## #   Dmax <lgl>, Z50 <lgl>, Z95 <lgl>, fHDmin <lgl>, fHDmax <lgl>, a_ash <lgl>,
    ## #   b_ash <lgl>, a_bsh <lgl>, b_bsh <lgl>, a_btsh <lgl>, b_btsh <lgl>,
    ## #   cr <lgl>, BTsh <lgl>, a_fbt <lgl>, b_fbt <lgl>, c_fbt <lgl>, a_cr <lgl>,
    ## #   b_1cr <lgl>, b_2cr <lgl>, b_3cr <lgl>, c_1cr <lgl>, c_2cr <lgl>, …

## Filling structural parameters from forest inventory data

Some species parameters can be withdrawn from the data of the target
forest inventory where simulations are to be conducted. We load a data
frame (actually an `sf` object) containing data from the Third Spanish
Forest Inventory:

``` r

sf_IFN3 <- readRDS("~/OneDrive/mcaceres_work/model_initialisation/medfate_initialisation/IFN2medfate/data/SpParamsES/IFN3/soilmod/IFN3_spain_soilmod_WGS84.rds")
sf_IFN3
```

    ## Simple feature collection with 98996 features and 11 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: -18.13626 ymin: 27.70124 xmax: 4.29737 ymax: 43.7617
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 98,996 × 12
    ##    id                         geometry  year plot  country version forest      
    ##  * <chr>                   <POINT [°]> <int> <chr> <chr>   <chr>   <list>      
    ##  1 01_0001_xx_A4… (-3.024625 43.21778)  2005 0001  ES      ifn3    <forest [5]>
    ##  2 01_0002_xx_A4… (-3.024621 43.20878)  2005 0002  ES      ifn3    <forest [5]>
    ##  3 01_0003_NN_A3… (-3.012307 43.19077)  2005 0003  ES      ifn3    <forest [5]>
    ##  4 01_0003_xx_A3… (-3.012307 43.19077)  2005 0003  ES      ifn3    <forest [5]>
    ##  5 01_0004_NN_A3…        (-3 43.19077)  2005 0004  ES      ifn3    <forest [5]>
    ##  6 01_0004_xx_A3…        (-3 43.19077)  2005 0004  ES      ifn3    <forest [5]>
    ##  7 01_0005_NN_A1… (-2.988971 43.18907)  2005 0005  ES      ifn3    <forest [5]>
    ##  8 01_0006_NN_A1… (-2.975386 43.19077)  2005 0006  ES      ifn3    <forest [5]>
    ##  9 01_0007_xx_A4… (-2.987695 43.18177)  2005 0007  ES      ifn3    <forest [5]>
    ## 10 01_0008_xx_A4…  (-2.97539 43.18176)  2005 0008  ES      ifn3    <forest [5]>
    ## # ℹ 98,986 more rows
    ## # ℹ 5 more variables: forest_unfiltered <list>, elevation <dbl>, slope <dbl>,
    ## #   aspect <dbl>, soil <list>

Column `forest` contains the forest inventory plots that will be used to
withdraw parameters. For example, the second `forest` object is:

``` r

sf_IFN3$forest[[2]]
```

    ## $treeData
    ## # A tibble: 30 × 9
    ##    Species       SpeciesCode   DBH Height     N   Z50   Z95 tree_ifn2 tree_ifn3
    ##    <chr>         <chr>       <dbl>  <dbl> <dbl> <dbl> <dbl>     <int>     <int>
    ##  1 Pinus radiata 28           12.0   1000 127.     NA    NA         0         1
    ##  2 Pinus radiata 28           12.6   1050  31.8    NA    NA         0         2
    ##  3 Pinus radiata 28           17.0   1050  31.8    NA    NA         0         3
    ##  4 Pinus radiata 28           16.2   1100  31.8    NA    NA         0         4
    ##  5 Pinus radiata 28           17.2   1050  31.8    NA    NA         0         5
    ##  6 Pinus radiata 28           17.0   1150  31.8    NA    NA         0         6
    ##  7 Pinus radiata 28           14.4   1000  31.8    NA    NA         0         7
    ##  8 Pinus radiata 28           10.8   1000 127.     NA    NA         0         8
    ##  9 Pinus radiata 28           20.5   1100  31.8    NA    NA         0         9
    ## 10 Pinus radiata 28           13.2   1000  31.8    NA    NA         0        10
    ## # ℹ 20 more rows
    ## 
    ## $shrubData
    ## # A tibble: 5 × 6
    ##   Species             SpeciesCode Height Cover   Z50   Z95
    ##   <chr>               <chr>        <dbl> <dbl> <dbl> <dbl>
    ## 1 Ulex spp.           1103           100     2    NA    NA
    ## 2 Daboecia cantabrica 120             30     2    NA    NA
    ## 3 Rubus ulmifolius    3121           150     5    NA    NA
    ## 4 Erica cinerea       5102           150     2    NA    NA
    ## 5 Cistus salviifolius 6101            20     1    NA    NA
    ## 
    ## $herbCover
    ## [1] NA
    ## 
    ## $herbHeight
    ## [1] NA
    ## 
    ## $seedBank
    ## [1] Species Percent
    ## <0 rows> (or 0-length row.names)
    ## 
    ## attr(,"class")
    ## [1] "forest" "list"

**traits4models** provides function
[`fill_medfate_inventory_traits()`](https://emf-creaf.github.io/traits4models/reference/fill_medfate_inventory_traits.md)
to perform the parameter extraction:

``` r

SpParams<- traits4models::fill_medfate_inventory_traits(SpParams, sf_IFN3, 
                                                        progress = FALSE)
```

Species matching is performed between column `Species` of the tree or
shrub data tables and `Name` of the species parameter table; and the
following information is extracted:

- `GrowthForm`: Growth form according to the usage in the forest
  inventory. For example, if the species is cited in `treeData` tables
  but not in `shrubData` tables, then growth form will be `"Tree"`.
- `Hmax`: Maximum tree/shrub height (cm), according to the `Height`
  column in `treeData` or `shrubData` and `quantile_Hmax` parameter.
- `Hmed`: Median tree/shrub height (cm), according to the `Height`
  column in `treeData` or `shrubData` and `quantile_Hmed` parameter.
- `fHDmin`, `fHDmax`: Minimum or maximum height to diameter ratio for
  trees, according to the `Height` and `DBH` columns in `treeData` and
  `quantile_fHDmin` or `quantile_fHDmax` parameters, respectively.

## Filling allometric coefficients

As mentioned in the introduction, here it is assumed that a set of plant
allometry databases have been harmonized. In our case, harmonized data
files have been stored as **.rds** or **.csv** files in the following
path:

``` r

harmonized_allometry_path <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/traits_and_models/data/harmonized_allometry_sources"
```

Function
[`fill_medfate_allometries()`](https://emf-creaf.github.io/traits4models/reference/fill_medfate_allometries.md)
performs trait parameter filling for **medfate** using the available
data:

``` r

SpParams<- traits4models::fill_medfate_allometries(SpParams, 
                                                   harmonized_allometry_path, 
                                                   progress = TRUE, verbose = FALSE)
```

    ## ℹ Processing response: FoliarBiomass

    ## ✔ Processing response: FoliarBiomass [35ms]

    ## 

    ## ℹ Processing response: CrownRatio

    ## ✔ Processing response: CrownRatio [23ms]

    ## 

    ## ℹ Processing response: CrownWidth

    ## ✔ Processing response: CrownWidth [20ms]

    ## 

    ## ℹ Processing response: BarkThickness

    ## ✔ Processing response: BarkThickness [17ms]

    ## 

    ## ℹ Processing response: CrownArea

    ## ✔ Processing response: CrownArea [16ms]

    ## 

    ## ℹ Processing response: FineFuelBiomass

    ## ✔ Processing response: FineFuelBiomass [16ms]

    ## 

    ## ℹ Processing response: TotalBiomass

    ## ✔ Processing response: TotalBiomass [16ms]

    ## 

## Filling parameters from harmonized trait databases

As mentioned in the introduction, here it is assumed that a set of plant
trait databases have been harmonized. In our case, harmonized data files
have been stored as **.rds** or **.csv** files in the following path:

``` r

harmonized_trait_path <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/traits_and_models/data/harmonized_trait_sources"
```

Function
[`fill_medfate_traits()`](https://emf-creaf.github.io/traits4models/reference/fill_medfate_traits.md)
performs trait parameter filling for **medfate** using the available
data:

``` r

SpParams<- traits4models::fill_medfate_traits(SpParams, harmonized_trait_path, 
                                              progress = TRUE, verbose = FALSE)
```

    ## ℹ Processing parameter: GrowthForm

    ## ✔ Processing parameter: GrowthForm [12.2s]

    ## 

    ## ℹ Processing parameter: LifeForm

    ## ✔ Processing parameter: LifeForm [379ms]

    ## 

    ## ℹ Processing parameter: LeafShape

    ## ✔ Processing parameter: LeafShape [1.6s]

    ## 

    ## ℹ Processing parameter: PhenologyType

    ## ✔ Processing parameter: PhenologyType [882ms]

    ## 

    ## ℹ Processing parameter: DispersalType

    ## ✔ Processing parameter: DispersalType [2.1s]

    ## 

    ## ℹ Processing parameter: t0gdd

    ## ✔ Processing parameter: t0gdd [75ms]

    ## 

    ## ℹ Processing parameter: Tbgdd

    ## ✔ Processing parameter: Tbgdd [92ms]

    ## 

    ## ℹ Processing parameter: Sgdd

    ## ✔ Processing parameter: Sgdd [72ms]

    ## 

    ## ℹ Processing parameter: Phsen

    ## ✔ Processing parameter: Phsen [90ms]

    ## 

    ## ℹ Processing parameter: Tbsen

    ## ✔ Processing parameter: Tbsen [70ms]

    ## 

    ## ℹ Processing parameter: xsen

    ## ✔ Processing parameter: xsen [90ms]

    ## 

    ## ℹ Processing parameter: ysen

    ## ✔ Processing parameter: ysen [70ms]

    ## 

    ## ℹ Processing parameter: Ssen

    ## ✔ Processing parameter: Ssen [90ms]

    ## 

    ## ℹ Processing parameter: LeafSize

    ## ✔ Processing parameter: LeafSize [1.2s]

    ## 

    ## ℹ Processing parameter: Dmax

    ## ✔ Processing parameter: Dmax [321ms]

    ## 

    ## ℹ Processing parameter: Hmax

    ## ✔ Processing parameter: Hmax [2s]

    ## 

    ## ℹ Processing parameter: Hmed

    ## ✔ Processing parameter: Hmed [1.8s]

    ## 

    ## ℹ Processing parameter: cr

    ## ✔ Processing parameter: cr [92ms]

    ## 

    ## ℹ Processing parameter: Gs_P50

    ## ✔ Processing parameter: Gs_P50 [97ms]

    ## 

    ## ℹ Processing parameter: maxFMC

    ## ✔ Processing parameter: maxFMC [1.9s]

    ## 

    ## ℹ Processing parameter: minFMC

    ## ✔ Processing parameter: minFMC [2.4s]

    ## 

    ## ℹ Processing parameter: Kmax_stemxylem

    ## ✔ Processing parameter: Kmax_stemxylem [204ms]

    ## 

    ## ℹ Processing parameter: Vmax298

    ## ✔ Processing parameter: Vmax298 [217ms]

    ## 

    ## ℹ Processing parameter: Jmax298

    ## ✔ Processing parameter: Jmax298 [204ms]

    ## 

    ## ℹ Processing parameter: Z95

    ## ✔ Processing parameter: Z95 [326ms]

    ## 

    ## ℹ Processing parameter: LeafAngle

    ## ✔ Processing parameter: LeafAngle [383ms]

    ## 

    ## ℹ Processing parameter: LeafAngleSD

    ## ✔ Processing parameter: LeafAngleSD [354ms]

    ## 

    ## ℹ Processing parameter: LeafDensity

    ## ✔ Processing parameter: LeafDensity [664ms]

    ## 

    ## ℹ Processing parameter: WoodDensity

    ## ✔ Processing parameter: WoodDensity [1.2s]

    ## 

    ## ℹ Processing parameter: SRL

    ## ✔ Processing parameter: SRL [171ms]

    ## 

    ## ℹ Processing parameter: r635

    ## ✔ Processing parameter: r635 [74ms]

    ## 

    ## ℹ Processing parameter: pDead

    ## ✔ Processing parameter: pDead [96ms]

    ## 

    ## ℹ Processing parameter: SAV

    ## ✔ Processing parameter: SAV [69ms]

    ## 

    ## ℹ Processing parameter: HeatContent

    ## ✔ Processing parameter: HeatContent [97ms]

    ## 

    ## ℹ Processing parameter: LeafPI0

    ## ✔ Processing parameter: LeafPI0 [121ms]

    ## 

    ## ℹ Processing parameter: LeafEPS

    ## ✔ Processing parameter: LeafEPS [120ms]

    ## 

    ## ℹ Processing parameter: LeafAF

    ## ✔ Processing parameter: LeafAF [92ms]

    ## 

    ## ℹ Processing parameter: SLA

    ## ✔ Processing parameter: SLA [2s]

    ## 

    ## ℹ Processing parameter: Al2As

    ## ✔ Processing parameter: Al2As [183ms]

    ## 

    ## ℹ Processing parameter: conduit2sapwood

    ## ✔ Processing parameter: conduit2sapwood [73ms]

    ## 

    ## ℹ Processing parameter: LeafWidth

    ## ✔ Processing parameter: LeafWidth [842ms]

    ## 

    ## ℹ Processing parameter: LeafDuration

    ## ✔ Processing parameter: LeafDuration [132ms]

    ## 

    ## ℹ Processing parameter: Gswmax

    ## ✔ Processing parameter: Gswmax [99ms]

    ## 

    ## ℹ Processing parameter: Gswmin

    ## ✔ Processing parameter: Gswmin [84ms]

    ## 

    ## ℹ Processing parameter: VCleaf_P50

    ## ✔ Processing parameter: VCleaf_P50 [153ms]

    ## 

    ## ℹ Processing parameter: VCleaf_P12

    ## ✔ Processing parameter: VCleaf_P12 [86ms]

    ## 

    ## ℹ Processing parameter: VCleaf_P88

    ## ✔ Processing parameter: VCleaf_P88 [112ms]

    ## 

    ## ℹ Processing parameter: VCleaf_slope

    ## ✔ Processing parameter: VCleaf_slope [74ms]

    ## 

    ## ℹ Processing parameter: VCstem_P50

    ## ✔ Processing parameter: VCstem_P50 [220ms]

    ## 

    ## ℹ Processing parameter: VCstem_P12

    ## ✔ Processing parameter: VCstem_P12 [126ms]

    ## 

    ## ℹ Processing parameter: VCstem_P88

    ## ✔ Processing parameter: VCstem_P88 [145ms]

    ## 

    ## ℹ Processing parameter: VCstem_slope

    ## ✔ Processing parameter: VCstem_slope [105ms]

    ## 

    ## ℹ Processing parameter: VCroot_P50

    ## ✔ Processing parameter: VCroot_P50 [109ms]

    ## 

    ## ℹ Processing parameter: VCroot_P12

    ## ✔ Processing parameter: VCroot_P12 [73ms]

    ## 

    ## ℹ Processing parameter: VCroot_P88

    ## ✔ Processing parameter: VCroot_P88 [95ms]

    ## 

    ## ℹ Processing parameter: VCroot_slope

    ## ✔ Processing parameter: VCroot_slope [73ms]

    ## 

    ## ℹ Processing parameter: Nleaf

    ## ✔ Processing parameter: Nleaf [1.4s]

    ## 

    ## ℹ Processing parameter: Nsapwood

    ## ✔ Processing parameter: Nsapwood [187ms]

    ## 

    ## ℹ Processing parameter: Nfineroot

    ## ✔ Processing parameter: Nfineroot [163ms]

    ## 

    ## ℹ Processing parameter: SeedMass

    ## ✔ Processing parameter: SeedMass [3.6s]

    ## 

    ## ℹ Processing parameter: SeedLongevity

    ## ✔ Processing parameter: SeedLongevity [181ms]

    ## 

    ## ℹ Processing parameter: WoodC

    ## ✔ Processing parameter: WoodC [106ms]

    ## 

    ## ℹ Processing parameter: CCleaf

    ## ✔ Processing parameter: CCleaf [94ms]

    ## 

    ## ℹ Processing parameter: CCsapwood

    ## ✔ Processing parameter: CCsapwood [65ms]

    ## 

    ## ℹ Processing parameter: CCfineroot

    ## ✔ Processing parameter: CCfineroot [86ms]

    ## 

    ## ℹ Processing parameter: RSSG

    ## ✔ Processing parameter: RSSG [78ms]

    ## 

    ## ℹ Processing parameter: SeedProductionDiameter

    ## ✔ Processing parameter: SeedProductionDiameter [7ms]

    ## 

Parameter filling sometimes implies translating trait data into the
units required for model parameters. The function identifies the row of
the species parameter table to modify by matching column `AcceptedName`
of `SpParams` with the column `acceptedName` of the harmonized trait
data sources. When multiple values of a given species are available, the
function takes the median value (for quantitative traits) or the mode
(i.e. the most frequent value; for qualitative traits). In addition,
data sources of higher priority are processed first. Thus, data sources
of lower priority being used for those cases not covered by prioritized
sources.

## Checking species parameter tables

Before using a given species parameter table, it is important to verify
that the table does not contain missing values in key parameters. In
medfate, those parameters that are always required non-missing values
are called `Strict` (see `SpParamsDefinition` in package **medfate**).
Function
[`check_medfate_params()`](https://emf-creaf.github.io/traits4models/reference/check_medfate_params.md)
checks, among other things, that strict parameters do not contain
missing values:

``` r

check_medfate_params(SpParams)
```

    ## ! Strict parameter column 'GrowthForm' should not contain any missing value.

    ## ! Strict parameter column 'LifeForm' should not contain any missing value.

    ## ! Strict parameter column 'LeafShape' should not contain any missing value.

    ## ! Strict parameter column 'LeafSize' should not contain any missing value.

    ## ! Strict parameter column 'PhenologyType' should not contain any missing value.

    ## ! Strict parameter column 'DispersalType' should not contain any missing value.

    ## ! Strict parameter column 'Hmed' should not contain any missing value.

    ## ! Strict parameter column 'Hmax' should not contain any missing value.

    ## ! Strict parameter column 'Z95' should not contain any missing value.

Normally, the information of plant trait data bases will not contain
information for all species and strict parameters, even if most of them
are qualitative traits. This problem is raised by
[`check_medfate_params()`](https://emf-creaf.github.io/traits4models/reference/check_medfate_params.md)
and must be addressed.

## Completing strict parameters

Package **trait4models** includes function
[`complete_medfate_strict()`](https://emf-creaf.github.io/traits4models/reference/complete_medfate_strict.md)
to perform imputation of strict parameters, so that the result table can
be used for simulations. Imputation is performed on the basis of
available information for similar taxa. Taxonomically closer entities
are given priority over higher order taxa (i.e. species of the same
genus are priorized as sources before searching for species in the same
family):

``` r

SpParams <- traits4models::complete_medfate_strict(SpParams, progress = FALSE)
```

After completion of strict paratemers, the species parameter table
should pass the previous check:

``` r

check_medfate_params(SpParams)
```

    ## ✔ The data frame is acceptable as species parameter table for medfate.

And we can store the table for its use in model simulations.

## References

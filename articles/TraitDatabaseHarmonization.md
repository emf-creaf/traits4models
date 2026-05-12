# Trait database harmonization

## Introduction

Species parameter values cannot be drawn from a single data source.
Moreover, the availability of plant trait data continuously increases,
as additional efforts are made in observational or experimental studies
(Kattge et al. 2020). This means that multiple data sources need to be
harmonized before species parameter tables are build, in terms of:

    a) Nomenclature of measured variables.
    b) Measurement units.
    c) Taxonomy of the measured biological entities. 

The result of this harmonization needs to be stored in a harmonized
format for subsequent use when creating species parameter tables. This
vignette illustrates the harmonization procedures for an example data
set using package **trait4models** and the usual **tidyverse** packages.
Harmonization of allometric equations is explained in a companion
vignette.

> **IMPORTANT**: This vignette is not self-contained, in the sense that
> it cannot be reproduced without access to data sets that are not
> included. Nevertheless, it is intended to serve as example of trait
> database harmonization.

### Required packages

Assuming we have **traits4models** installed, we load it and other
common packages that we will employ in this vignette:

``` r

library(traits4models)
library(tidyverse)
library(readr)
```

### Example dataset

As example for the harmonization process, here we will use data from a
Bartlett et al. (2016). Much larger data sets can (and should) be
processed, but they take much more time. Bartlett et al. provide traits
that describe the leaf/stem/root hydraulic vulnerability curves of
several species as well as water potential corresponding to stomatal
aperture. We start by loading the dataset:

``` r

DB_path <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/traits_and_models/"
db <- readr::read_csv(paste0(DB_path, "data-raw/raw_trait_data/Bartlett_et_al_2016/pnas.1604088113.sd01.csv"))
```

The data looks as follows:

``` r

db
```

    ## # A tibble: 310 × 26
    ##    Group      Name          Biome `Evergreen/Decid` `Leaf P50 (MPa)` `TLP (MPa)`
    ##    <chr>      <chr>         <chr> <chr>                        <dbl>       <dbl>
    ##  1 Angiosperm Acacia gregg… Semi… E                            NA          -4.25
    ##  2 Angiosperm Acer campest… Temp… D                            -1.32       -1.9 
    ##  3 Angiosperm Acer grandid… Temp… D                            NA          -2.45
    ##  4 Angiosperm Acer monspes… Med.… D                            -1.89       -2.2 
    ##  5 Angiosperm Acer negundo  Temp… D                            NA          -1.59
    ##  6 Angiosperm Acer pseudop… Temp… D                            -1.19       -1.4 
    ##  7 Angiosperm Acer rubrum   Temp… D                            -1.7        -1.59
    ##  8 Angiosperm Acer sacchar… Temp… D                            NA          -2.78
    ##  9 Angiosperm Adansonia ru… Trop… D                            NA          -1.12
    ## 10 Angiosperm Adansonia za  Trop… D                            NA          -1.26
    ## # ℹ 300 more rows
    ## # ℹ 20 more variables: `Stem P50 (MPa)` <dbl>, `Stem P88 (MPa)` <dbl>,
    ## #   `Stem P12 (MPa)` <dbl>, `Root P50 (MPa)` <dbl>, `Gs P50 (MPa)` <dbl>,
    ## #   `Gs 95 (MPa)` <dbl>, `plant Psi_lethal (MPa)` <dbl>,
    ## #   `Psimin_predawn (MPa)` <dbl>, `Psimin_midday (MPa)` <dbl>,
    ## #   `Psimin_midday and/or predawn Method` <chr>,
    ## #   `Reference (for Leaf P50)` <chr>, `Reference (for TLP)` <chr>, …

## Harmonizing notation and measurement units

The first steps to be done are to harmonize trait notation, i.e. how
plant traits are referred to, and if necessary, change their units.
Package **traits4models** includes a data table called
`HarmonizedTraitDefinition` that presents plant trait definitions and
their required notation and units:

| Definition | Notation | Type | Units | EquivalentUnits | AcceptedValues | MinimumValue | MaximumValue |
|:---|:---|:---|:---|:---|:---|---:|---:|
| Life form | LifeForm | String | NA | NA | chamaephyte,cryptophyte,epiphyte,hemicryptophyte,phanerophyte,therophyte,hydrophyte | NA | NA |
| Growth form | GrowthForm | String | NA | NA | tree,shrub,herb,shrub/herb,tree/herb,tree/shrub,tree/shrub/herb,fern,grass,other | NA | NA |
| Leaf shape | LeafShape | String | NA | NA | broad,linear,needle,scale,spines,succulent | NA | NA |
| Leaf area | LeafArea | Numeric | mm2 | NA | NA | 0 | NA |
| Leaf size (category) | LeafSize | String | NA | NA | lepto,macro,meso,micro,nano,noto,pico | NA | NA |
| Leaf angle (inclination, orientation) | LeafAngle | Numeric | degree | NA | NA | 0 | 90 |
| Dispersal syndrome | DispersalMode | String | NA | NA | insect,auto,ballistic,vertebrate,water,wind,vehicles | NA | NA |
| Leaf phenology type | PhenologyType | String | NA | NA | drought-semideciduous,oneflush-evergreen,winter-deciduous,winter-semideciduous | NA | NA |
| Shade tolerance type | ShadeToleranceType | String | NA | NA | light-demanding,shade-tolerant | NA | NA |
| Duration of leaves (leaf lifespan) | LeafDuration | Numeric | year | NA | NA | NA | NA |
| Maximum plant height | Hmax | Numeric | cm | NA | NA | 0 | NA |
| Maximum (tree) diameter | Dmax | Numeric | cm | NA | NA | 0 | NA |
| Actual plant height | Hact | Numeric | cm | NA | NA | 0 | NA |
| Rooting depth | Z95 | Numeric | mm | NA | NA | 0 | NA |
| Crown ratio (crown length divided over total height) | CrownRatio | Numeric | NA | NA | NA | NA | NA |
| Leaf area per leaf dry mass (specific leaf area, SLA), 1/ Leaf mass per area (LMA) | SLA | Numeric | m2 kg-1 | mm2 mg-1 | NA | 0 | NA |
| Leaf area to sapwood area ratio (Al2As), 1 / Huber Value (Hv) | Al2As | Numeric | m2 m-2 | mm2 mm-2 | NA | 0 | NA |
| Proportion of sapwood corresponding to conducive elements (vessels or tracheids) as opposed to parenchymatic tissue. | conduit2sapwood | Numeric | NA | NA | NA | NA | NA |
| Specific root length | SRL | Numeric | cm g-1 | NA | NA | 0 | NA |
| Proportion of total fine fuels that are dead | pDead | Numeric | NA | NA | NA | 0 | 1 |
| Stem carbon (C) content per stem dry mass | WoodC | Numeric | g g-1 | NA | NA | NA | NA |
| Density of leaf tissue (dry weight over volume) | LeafDensity | Numeric | g cm-3 | mg mm-3 | NA | 0 | NA |
| Wood tissue density (at 0% humidity!) | WoodDensity | Numeric | g cm-3 | mg mm-3 | NA | 0 | NA |
| Density of fine root tissue (dry weight over volume). | FineRootDensity | Numeric | g cm-3 | mg mm-3 | NA | 0 | NA |
| Leaf width | LeafWidth | Numeric | cm | NA | NA | 0 | NA |
| Maximum stomatal conductance to water vapor | Gswmax | Numeric | mol s-1 m-2 | NA | NA | 0 | NA |
| Minimum stomatal conductance to water vapor | Gswmin | Numeric | mol s-1 m-2 | NA | NA | 0 | NA |
| Stem conductance to water vapor | Gbark | Numeric | mol s-1 m-2 | NA | NA | 0 | NA |
| Osmotic potential at full turgor of leaves | LeafPI0 | Numeric | MPa | NA | NA | NA | NA |
| Modulus of elasticity (capacity of the cell wall to resist changes in volume in response to changes in turgor) of leaves | LeafEPS | Numeric | MPa | NA | NA | NA | NA |
| Leaf apoplastic fraction | LeafAF | Numeric | \[0-1\] | NA | NA | NA | NA |
| Modulus of elasticity (capacity of the cell wall to resist changes in volume in response to changes in turgor) of sapwood | StemEPS | Numeric | MPa | NA | NA | NA | NA |
| Leaf water potential at turgor loss point | Ptlp | Numeric | MPa | NA | NA | NA | NA |
| Slope coefficient of the Medlyn stomatal conductance model | g1_Medlyn | Numeric | NA | NA | NA | NA | NA |
| Parameters of the stomatal response to leaf water potential | Gs_P20 | Numeric | MPa | NA | NA | NA | 0 |
| Parameters of the stomatal response to leaf water potential | Gs_P50 | Numeric | MPa | NA | NA | NA | 0 |
| Parameters of the stomatal response to leaf water potential | Gs_P80 | Numeric | MPa | NA | NA | NA | 0 |
| Parameters of the stomatal response to leaf water potential | Gs_P90 | Numeric | MPa | NA | NA | NA | 0 |
| Parameters of the stomatal response to leaf water potential | Gs_P95 | Numeric | MPa | NA | NA | NA | 0 |
| Leaf photosynthesis carboxylation capacity (Vcmax) per leaf area (Farquhar model) | Vmax | Numeric | umol m-2 s-1 | NA | NA | 0 | NA |
| Leaf photosynthesis electron transport capacity (Jmax) per leaf area (Farquhar model) | Jmax | Numeric | umol m-2 s-1 | NA | NA | 0 | NA |
| Leaf nitrogen (N) content per leaf dry mass | Nleaf | Numeric | mg g-1 | NA | NA | 0 | NA |
| Wood nitrogen (N) content per wood dry mass | Nsapwood | Numeric | mg g-1 | NA | NA | 0 | NA |
| Fine root nitrogen (N) content per fine root dry mass | Nfineroot | Numeric | mg g-1 | NA | NA | 0 | NA |
| Maximum stem-specific hydraulic conductivity | Ks | Numeric | kg m-1 MPa-1 s-1 | NA | NA | 0 | NA |
| Maximum leaf-specific hydraulic conductivity (Ks\*Hv) | Kl | Numeric | 10-4 kg m-1 MPa-1 s-1 | NA | NA | 0 | NA |
| Maximum leaf hydraulic conductance | kleaf | Numeric | mmol m-2 s-1 MPa-1 | NA | NA | 0 | NA |
| Maximum whole-plant hydraulic conductance | kplant | Numeric | mmol m-2 s-1 MPa-1 | NA | NA | 0 | NA |
| Parameters of the stem vulnerability curve | VCstem_P12 | Numeric | MPa | NA | NA | NA | 0 |
| Parameters of the stem vulnerability curve | VCstem_P50 | Numeric | MPa | NA | NA | NA | 0 |
| Parameters of the stem vulnerability curve | VCstem_P88 | Numeric | MPa | NA | NA | NA | 0 |
| Parameters of the stem vulnerability curve | VCstem_slope | Numeric | NA | NA | NA | 0 | NA |
| Parameters of the leaf vulnerability curve | VCleaf_P12 | Numeric | MPa | NA | NA | NA | 0 |
| Parameters of the leaf vulnerability curve | VCleaf_P50 | Numeric | MPa | NA | NA | NA | 0 |
| Parameters of the leaf vulnerability curve | VCleaf_P88 | Numeric | MPa | NA | NA | NA | 0 |
| Parameters of the leaf vulnerability curve | VCleaf_slope | Numeric | NA | NA | NA | 0 | NA |
| Parameters of the root vulnerability curve | VCroot_P12 | Numeric | MPa | NA | NA | NA | 0 |
| Parameters of the root vulnerability curve | VCroot_P50 | Numeric | MPa | NA | NA | NA | 0 |
| Parameters of the root vulnerability curve | VCroot_P88 | Numeric | MPa | NA | NA | NA | 0 |
| Parameters of the root vulnerability curve | VCroot_slope | Numeric | NA | NA | NA | 0 | NA |
| Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC) | LDMC | Numeric | mg g-1 | NA | NA | 0 | NA |
| Leaf fuel moisture content (% of dry weight) | LFMC | Numeric | % | NA | NA | 0 | NA |
| Ratio of foliar (photosynthetic) + small branches (\<6.35 mm) dry biomass to foliar (photosynthetic) dry biomass | r635 | Numeric | NA | NA | NA | 1 | NA |
| High fuel heat content | HeatContent | Numeric | kJ kg-1 | NA | NA | 0 | NA |
| Surface-area-to-volume ratio | SAV | Numeric | m2 m-3 | NA | NA | 0 | NA |
| Percent of lignin+cutin over dry weight in leaves | LigninPercent | Numeric | % | NA | NA | 0 | 100 |
| Bark thickness | BarkThickness | Numeric | mm | NA | NA | 0 | NA |
| Seedbank average longevity | SeedLongevity | Numeric | year | NA | NA | 0 | NA |
| Maturation height | Hmat | Numeric | cm | NA | NA | 0 | NA |
| Maturation diameter | Dmat | Numeric | cm | NA | NA | 0 | NA |
| Seed dry mass | SeedMass | Numeric | mg | NA | NA | 0 | NA |
| Succulence (g of water /m2 of projected leaf) | LeafSucculence | Numeric | g m-2 | NA | NA | 0 | NA |
| Leaf projected to half developed area (m2/m2) | LeafProjectedToHalfDevelopedArea | Numeric | m2 m-2 | NA | NA | 0 | NA |
| Minimum conductance of the leaf to water vapor on developed area basis (including cuticule and stomatal leakiness) | GminLeaf | Numeric | mmol m-2 s-1 | NA | NA | 0 | NA |
| Q10 of the initial gmin response to temperature (before Tp) | Q10gminPhase1 | Numeric | NA | NA | NA | NA | NA |
| Q10 of the initial gmin response to temperature (after Tp) | Q10gminPhase2 | Numeric | NA | NA | NA | NA | NA |
| Transition phase for gmin dependence to temperature | GminTransitionPhase | Numeric | Celsius | NA | NA | NA | NA |
| Maintenance respiration rates for leaves | RERleaf | Numeric | g g-1 day-1 | NA | NA | 0 | NA |
| Maintenance respiration rates for living cells of sapwood | RERsapwood | Numeric | g g-1 day-1 | NA | NA | 0 | NA |
| Maintenance respiration rates for fine roots | RERfineroot | Numeric | g g-1 day-1 | NA | NA | 0 | NA |
| Leaf construction costs | CCleaf | Numeric | g g-1 | NA | NA | 0 | NA |
| Sapwood construction costs | CCsapwood | Numeric | g g-1 | NA | NA | 0 | NA |
| Fine root construction costs | CCfineroot | Numeric | g g-1 | NA | NA | 0 | NA |
| Date to start the accumulation of degree days | t0gdd | Numeric | day | NA | NA | NA | NA |
| Degree days for leaf budburst | Sgdd | Numeric | Celsius | NA | NA | NA | NA |
| Base temperature for the calculation of degree days to leaf budburst | Tbgdd | Numeric | Celsius | NA | NA | NA | NA |
| Degree days corresponding to senescence | Ssen | Numeric | Celsius | NA | NA | NA | NA |
| Photoperiod corresponding to start counting senescence | Phsen | Numeric | hour | NA | NA | NA | NA |
| Base temperature for the calculation of degree days to senescence | Tbsen | Numeric | Celsius | NA | NA | NA | NA |
| Discrete values, to allow for any absent/proportional/more than proportional effects of temperature on senescence | xsen | Integer | NA | NA | 0,1,2 | 0 | 2 |
| Discrete values, to allow for any absent/proportional/more than proportional effects of photoperiod on senescence | ysen | Integer | NA | NA | 0,1,2 | 0 | 2 |
| Shade tolerance index according to Valladares and Niinemets | ShadeTolerance | Numeric | NA | NA | NA | 0 | 5 |

In the case of Bartlett’s dataset we are interested in points of the
vulnerability curve and stomatal behavior. We can check their
corresponding names in `HarmonizedTraitDefinition`. For example,
`Leaf P50 (MPa)` is the water potential corresponding to the 50%
conductance loss in leaves, and should be named `VCleaf_P50` according
to `HarmonizedTraitDefinition`, and so on. We can use **dplyr** function
[`rename()`](https://dplyr.tidyverse.org/reference/rename.html) to
harmonize trait notation. In this case, all plant traits we are
interested in are given in units of MPa, and `HarmonizedTraitDefinition`
reports the same units for these traits, so there is no need to
harmonize measurement units. The code for notation harmonization could
be as follows:

``` r

db_var <- db |>
  dplyr::select(Name, "Leaf P50 (MPa)", "Stem P50 (MPa)", "Stem P88 (MPa)", "Stem P12 (MPa)", 
                "Root P50 (MPa)", "Gs P50 (MPa)", "Gs 95 (MPa)") |>
  dplyr::rename(originalName = Name,
                VCleaf_P50 = "Leaf P50 (MPa)",
                VCstem_P50 = "Stem P50 (MPa)",
                VCstem_P12 = "Stem P12 (MPa)",
                VCstem_P88 = "Stem P88 (MPa)",
                VCroot_P50 = "Root P50 (MPa)",
                Gs_P50 = "Gs P50 (MPa)",
                Gs_P95 = "Gs 95 (MPa)") 
```

Note that we also renamed the column containing the plant species into
`originalName`. The original name represents the taxon name that is used
by the data set owner/provider and is key for taxonomic harmonization.
The result of this step should contain `originalName`, plus one column
for each harmonized trait, and (preferably) three columns called
`Reference`, `DOI` and `Priority`. We can add those columns manually
using:

``` r

db_var <- db_var |>
  dplyr::mutate(Reference = "Bartlett et al. (2016). The correlations and sequence of plant stomatal, hydraulic, and wilting responses to drought. PNAS 113: 13098-13103",
                DOI = "10.1073/pnas.1604088113",
                Priority = 3)
```

Columns `Reference` and `DOI` indicates the bibliographic source of the
data, whereas `Priority` allows defining an order in which trait data
sources will be processed. Those with highest priority order (lowest
value of `Priority`) will be given preference.

``` r

db_var
```

    ## # A tibble: 310 × 11
    ##    originalName    VCleaf_P50 VCstem_P50 VCstem_P88 VCstem_P12 VCroot_P50 Gs_P50
    ##    <chr>                <dbl>      <dbl>      <dbl>      <dbl>      <dbl>  <dbl>
    ##  1 Acacia greggii       NA         NA         NA        NA          NA     NA   
    ##  2 Acer campestre       -1.32      -3.87      -4.60     -3.19       NA     NA   
    ##  3 Acer grandiden…      NA         -3.66      -7.14     -0.92       -0.86  NA   
    ##  4 Acer monspessu…      -1.89      -3.31      -4.61     -2.02       -1.6   NA   
    ##  5 Acer negundo         NA         -1.34      -2.74     -0.451      -0.3   NA   
    ##  6 Acer pseudopla…      -1.19      -2.37      -2.71     -1.95       NA     NA   
    ##  7 Acer rubrum          -1.7       -3.9       -6        -2.5        -1.69  NA   
    ##  8 Acer saccharum       NA         -3.97      NA        NA          -1.5   -1.56
    ##  9 Adansonia rubr…      NA         -1.1       -2.82     -0.293      NA     NA   
    ## 10 Adansonia za         NA         -1.7       -3.49     -0.59       NA     NA   
    ## # ℹ 300 more rows
    ## # ℹ 4 more variables: Gs_P95 <dbl>, Reference <chr>, DOI <chr>, Priority <dbl>

Note that the kind of trait mapping conducted here, with one column per
trait, allows different traits in different columns (i.e. wide format)
but it does not store the trait units and therefore they cannot be
checked. Another (long) format is recommended where trait names are
stored in column `Trait`, trait values are stored in column `Value` and
units are stored in column `Units` (see
[`?check_harmonized_trait`](https://emf-creaf.github.io/traits4models/reference/check_harmonized_trait.md)).

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

WFO_file <- paste0(DB_path, "data-raw/wfo_backbone/classification.csv")
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

    ## # A tibble: 6 × 17
    ##   originalName  acceptedName acceptedNameAuthorship family genus specificEpithet
    ##   <chr>         <chr>        <chr>                  <chr>  <chr> <chr>          
    ## 1 Acacia gregg… Senegalia g… (A.Gray) Britton & Ro… Fabac… Sene… greggii        
    ## 2 Acer campest… Acer campes… L.                     Sapin… Acer  campestre      
    ## 3 Acer grandid… Acer saccha… (Nutt.) Desmarais      Sapin… Acer  saccharum      
    ## 4 Acer monspes… Acer monspe… L.                     Sapin… Acer  monspessulanum 
    ## 5 Acer negundo  Acer negundo L.                     Sapin… Acer  negundo        
    ## 6 Acer pseudop… Acer pseudo… L.                     Sapin… Acer  pseudoplatanus 
    ## # ℹ 11 more variables: taxonRank <chr>, VCleaf_P50 <dbl>, VCstem_P50 <dbl>,
    ## #   VCstem_P88 <dbl>, VCstem_P12 <dbl>, VCroot_P50 <dbl>, Gs_P50 <dbl>,
    ## #   Gs_P95 <dbl>, Reference <chr>, DOI <chr>, Priority <dbl>

## Checking harmonized trait data

The packages includes function
[`check_harmonized_trait()`](https://emf-creaf.github.io/traits4models/reference/check_harmonized_trait.md)
to check whether a given data frame conforms in structure and content to
what is later required for parameter table filling:

``` r

check_harmonized_trait(db_post)
```

    ## ℹ Column 'checkVersion' should preferably be defined.

    ## ✔ The data frame (wide format) is acceptable as harmonized trait data source.

The data set is acceptable, but the package recommends storing the
package version used for harmonization checking, which we can do using:

``` r

db_post <- db_post |>
   dplyr::mutate(checkVersion = as.character(packageVersion("traits4models")))
check_harmonized_trait(db_post)
```

    ## ✔ The data frame (wide format) is acceptable as harmonized trait data source.

Now the data set is ready to be used in parameter estimation. As an
example of checking, we can also run the same checking function with the
data base before taxonomic harmonization:

``` r

check_harmonized_trait(db_var)
```

    ## ! Taxonomy columns missing: acceptedName acceptedNameAuthorship family genus specificEpithet taxonRank

    ## ℹ Column 'checkVersion' should preferably be defined.

    ## ! The data frame is not acceptable as harmonized trait data source.

## Storing harmonized dataset

Harmonized data tables can be stored in **.csv** text format or
compressed **.rds** format. Moreover, all tables should be stored in the
same directory, here in the `Products/harmonized/` path.

``` r

file_out <- paste0(DB_path, "data/harmonized_trait_sources/Bartlett_et_al_2016.rds")
saveRDS(db_post, file_out)
```

## Accessing harmonized trait data

### List of harmonized trait databases

As mentioned in the introduction, here it is assumed that a set of plant
trait databases have been harmonized. In our case, harmonized data files
have been stored in the following path:

``` r

harmonized_trait_path <- paste0(DB_path, "data/harmonized_trait_sources")
```

We can list the set of harmonized sources using:

``` r

trait_files <- list.files(path = harmonized_trait_path, full.names = FALSE)
head(trait_files)
```

    ## [1] "00_compilation_CCfineroot.rds"              
    ## [2] "00_compilation_CCleaf.rds"                  
    ## [3] "00_compilation_CCsapwood.rds"               
    ## [4] "00_compilation_FineFuelRatio.rds"           
    ## [5] "00_compilation_Flammability_HeatContent.rds"
    ## [6] "00_compilation_Flammability_SAV.rds"

### Querying data for particular traits or species

Before filling any species parameter table, it may be useful to inspect
the amount of information available for particular traits or species.
Package **trait4models** provides a couple of utility functions for
this. For example, we can load all values for Gswmin, the minimum
stomatal conductance, using function
[`get_trait_data()`](https://emf-creaf.github.io/traits4models/reference/get_trait_data.md):

``` r

gsmin_data <- get_trait_data(harmonized_trait_path, "Gswmin",
                             progress = FALSE)
head(gsmin_data)
```

    ##       originalName     acceptedName acceptedNameAuthorship   family  genus
    ## 1       Abies alba       Abies alba                  Mill. Pinaceae  Abies
    ## 2 Abies lasiocarpa Abies lasiocarpa          (Hook.) Nutt. Pinaceae  Abies
    ## 3   Abies sibirica   Abies sibirica                 Ledeb. Pinaceae  Abies
    ## 4       Acacia koa       Acacia koa                 A.Gray Fabaceae Acacia
    ## 5  Acacia maidenii  Acacia maidenii               F.Muell. Fabaceae Acacia
    ## 6   Acacia mangium   Acacia mangium                 Willd. Fabaceae Acacia
    ##   specificEpithet taxonRank  Trait       Value       Units
    ## 1            alba   species Gswmin 0.001500000 mol s-1 m-2
    ## 2      lasiocarpa   species Gswmin 0.004779429 mol s-1 m-2
    ## 3        sibirica   species Gswmin 0.003044600 mol s-1 m-2
    ## 4             koa   species Gswmin 0.003800000 mol s-1 m-2
    ## 5        maidenii   species Gswmin 0.004200000 mol s-1 m-2
    ## 6         mangium   species Gswmin 0.004750000 mol s-1 m-2
    ##                                                                                                                                                                                                          Reference
    ## 1 Wang et al. (2024) Water loss after stomatal closure: quantifying leaf  minimum conductance and minimal water use in nine  temperate European tree species during a severe drought. Tree Physiology, 44, tpae027
    ## 2                                            Duursma et al. (2018) On the minimum leaf conductance: its role in models of plant water use, and ecological and environmental controls. New Phytologist 221, 693-705
    ## 3                                            Duursma et al. (2018) On the minimum leaf conductance: its role in models of plant water use, and ecological and environmental controls. New Phytologist 221, 693-705
    ## 4                                            Duursma et al. (2018) On the minimum leaf conductance: its role in models of plant water use, and ecological and environmental controls. New Phytologist 221, 693-705
    ## 5                                            Duursma et al. (2018) On the minimum leaf conductance: its role in models of plant water use, and ecological and environmental controls. New Phytologist 221, 693-705
    ## 6                                            Duursma et al. (2018) On the minimum leaf conductance: its role in models of plant water use, and ecological and environmental controls. New Phytologist 221, 693-705
    ##                        DOI                 OriginalReference Priority
    ## 1 10.1093/treephys/tpae027                              <NA>        1
    ## 2        10.1111/nph.15395         Boyce and Saunders (2000)        1
    ## 3        10.1111/nph.15395    Brodribb McAdam, et al. (2014)        1
    ## 4        10.1111/nph.15395 Pasquet-Kok Creese, et al. (2010)        1
    ## 5        10.1111/nph.15395      Warren Aranda, et al. (2011)        1
    ## 6        10.1111/nph.15395      Warren Aranda, et al. (2011)        1
    ##   checkVersion
    ## 1        0.2.3
    ## 2        0.2.3
    ## 3        0.2.3
    ## 4        0.2.3
    ## 5        0.2.3
    ## 6        0.2.3

Analogously, if we are interested in querying trait information for a
particular taxa, we can use function
[`get_taxon_data()`](https://emf-creaf.github.io/traits4models/reference/get_trait_data.md):

``` r

ph_data <- get_taxon_data(harmonized_trait_path, "Pinus halepensis",
                          progress = FALSE)
head(ph_data)
```

    ##       originalName     acceptedName acceptedNameAuthorship   family genus
    ## 1 Pinus halepensis Pinus halepensis                  Mill. Pinaceae Pinus
    ## 2 Pinus halepensis Pinus halepensis                  Mill. Pinaceae Pinus
    ## 3 Pinus halepensis Pinus halepensis                  Mill. Pinaceae Pinus
    ## 4 Pinus halepensis Pinus halepensis                  Mill. Pinaceae Pinus
    ## 5 Pinus halepensis Pinus halepensis                  Mill. Pinaceae Pinus
    ## 6 Pinus halepensis Pinus halepensis                  Mill. Pinaceae Pinus
    ##   specificEpithet taxonRank       Trait            Value   Units
    ## 1      halepensis   species        r635 1.96422645197988    <NA>
    ## 2      halepensis   species HeatContent            22150 kJ kg-1
    ## 3      halepensis   species         SAV             6050  m2 m-3
    ## 4      halepensis   species      Gs_P50            -1.75    <NA>
    ## 5      halepensis   species      Gs_P95            -2.37    <NA>
    ## 6      halepensis   species  VCroot_P50            -0.88    <NA>
    ##                                                                                                                                                                                                                                                                                                                                 Reference
    ## 1                                                                                                                                                                                                                                                                                                                                   Field
    ## 2 Cohen, M., Cuiñas, P., Diez, C., Fernandes, P., Guijarro, M., Moro, C., 2001. FIRE STAR: a decision support system for fuel management and fire hazard reduction in Mediterranean wildland - urban interfaces Deliverable D4-02 Fire Star Impacts Model of Wildland Fire Compilation of People Tenability and Material Properties Data.
    ## 3 Cohen, M., Cuiñas, P., Diez, C., Fernandes, P., Guijarro, M., Moro, C., 2001. FIRE STAR: a decision support system for fuel management and fire hazard reduction in Mediterranean wildland - urban interfaces Deliverable D4-02 Fire Star Impacts Model of Wildland Fire Compilation of People Tenability and Material Properties Data.
    ## 4                                                                                                                                                                                               Bartlett et al. (2016) The correlations and sequence of plant stomatal, hydraulic, and wilting responses to drought. PNAS 113 13098-13103
    ## 5                                                                                                                                                                                               Bartlett et al. (2016) The correlations and sequence of plant stomatal, hydraulic, and wilting responses to drought. PNAS 113 13098-13103
    ## 6                                                                                                                                                                                               Bartlett et al. (2016) The correlations and sequence of plant stomatal, hydraulic, and wilting responses to drought. PNAS 113 13098-13103
    ##                       DOI Priority checkVersion    OriginalReference
    ## 1                    <NA>        1        0.2.3                 <NA>
    ## 2                    <NA>        1        0.2.3                 <NA>
    ## 3                    <NA>        1        0.2.3                 <NA>
    ## 4 10.1073/pnas.1604088113        3        0.2.3    Klein et al. 2011
    ## 5 10.1073/pnas.1604088113        3        0.2.3    Klein et al. 2011
    ## 6 10.1073/pnas.1604088113        3        0.2.3 Oliveras et al. 2003

## References

Bartlett, Megan K, Tamir Klein, Steven Jansen, Brendan Choat, and Lawren
Sack. 2016. “The Correlations and Sequence of Plant Stomatal, Hydraulic,
and Wilting Responses to Drought.” *Proceedings of the National Academy
of Sciences of the United States of America* 113 (46): 13098–103.
<https://doi.org/10.1073/pnas.1604088113>.

Kattge, Jens, Gerhard Bönisch, Sandra Díaz, et al. 2020. “TRY Plant
Trait Database – Enhanced Coverage and Open Access.” *Global Change
Biology* 26 (1): 119–88. <https://doi.org/10.1111/gcb.14904>.

Kindt, Roeland. 2020. “WorldFlora: An R Package for Exact and Fuzzy
Matching of Plant Names Against the World Flora Online Taxonomic
Backbone Data.” *Applications in Plant Sciences* 8 (9): e11388.
<https://doi.org/10.1002/aps3.11388>.

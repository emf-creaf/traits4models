traits4models - from plant traits to model parameters
================

<!-- badges: start -->
[![R-CMD-check](https://github.com/emf-creaf/traits4models/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/emf-creaf/traits4models/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Introduction

Package **traits4models** is designed to facilitate creating species
parameter tables for process-based models such as those of package
[medfate](https://github.com/emf-creaf/medfate). Specifically utility
functions are provided to harmonize plant trait data bases and defining
tables from them.

## Package installation

Since both packages evolve together, installing **traits4models**
normally requires an up-to-date version of **medfate**.

The latest stable versions GitHub as follows (required package
\`remotes\`\` should be installed/updated first):

``` r
remotes::install_github("emf-creaf/traits4models")
```

## Usage

The package is still under active development.

## Documentation

A number of *vignettes* illustrate how to initialize inputs and run
simulation models in **traits4models**. These can be found at the
package [website](https://emf-creaf.github.io/traits4models/).

## Companion R packages

The development of **traits4models** is intended to complement other
packages of the medfate modelling framework:

- Package [**medfateland**](https://emf-creaf.github.io/medfateland)
  extends **medfate** by allowing simulations to be performed in a
  spatially explicit context.
- Package [**meteoland**](https://emf-creaf.github.io/meteoland) allows
  generating daily weather input for simulation models in **medfate**
  and **medfateland**.

<img src="man/figures/packages.png" width="60%" style="display: block; margin: auto;" />

## Authorship

The set of R packages are developed and maintained by the [*Ecosystem
Modelling Facility*](https://emf.creaf.cat) unit at
[*CREAF*](https://www.creaf.cat/) (in Spain), in close collaboration
with researchers from
[*URFM-INRAE*](https://www6.paca.inrae.fr/ecologie_des_forets_mediterraneennes/)
(in France) and [*CTFC*](https://www.ctfc.cat/) (in Spain).

<img src="man/figures/logos.png" width="60%" style="display: block; margin: auto;" />

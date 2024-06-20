traits4models - from plant traits to model parameters
================

<!-- badges: start -->
[![R-CMD-check](https://github.com/emf-creaf/traits4models/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/emf-creaf/traits4models/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Introduction

Package **traits4models** is designed to facilitate creating plant
species parameter tables for process-based models of forest function
and/or dynamics. Specifically utility functions are provided to
harmonize plant trait data bases and defining tables from them. At
present, species parameter estimation is tailored to be used for the
models included in packages
[medfate](https://emf-creaf.github.io/medfate) and
[medfateland](https://emf-creaf.github.io/medfateland), but it other
functions could be added to become useful for other process-based
models.

## Package installation

Since both packages evolve together, installing **traits4models**
normally requires an up-to-date version of package **medfate**, which is
available at [CRAN](https://cran.r-project.org/package=medfate).

The latest stable versions GitHub as follows (required package
\`remotes\`\` should be installed/updated first):

``` r
remotes::install_github("emf-creaf/traits4models")
```

## Usage

IMPORTANT: The package is still under active development.

## Documentation

A number of *vignettes* illustrate how to initialize inputs and run
simulation models in **traits4models**. These can be found at the
package [website](https://emf-creaf.github.io/traits4models/).

## Companion R packages

The development of **traits4models** is intended to complement packages
**medfate** and **medfateland**. Another package of the same modelling
framework is:

- Package [**meteoland**](https://emf-creaf.github.io/meteoland) allows
  generating daily weather input for simulation models in **medfate**
  and **medfateland**.

The relationships between the four packages are illustrated in the
figure below, where black arrows indicate package dependencies and gray
arrows indicate model parameter provision.

<img src="man/figures/packages.png" width="60%" style="display: block; margin: auto;" />

## Authorship

The set of R packages are developed and maintained by the [*Ecosystem
Modelling Facility*](https://emf.creaf.cat) unit at
[*CREAF*](https://www.creaf.cat/) (in Spain), in close collaboration
with researchers from
[*URFM-INRAE*](https://www6.paca.inrae.fr/ecologie_des_forets_mediterraneennes/)
(in France) and [*CTFC*](https://www.ctfc.cat/) (in Spain).

<img src="man/figures/logos.png" width="60%" style="display: block; margin: auto;" />

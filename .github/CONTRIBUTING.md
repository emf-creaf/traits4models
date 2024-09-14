# An ongoing project

The **traits4models** R package aims at becoming the result of collaborative work between process-based modellers interested in compiling plant trait data for model parameter estimation. Therefore, we are open to further expanding the set of people contributing to the project. 

# Contributing to traits4models

Contributions to the development of **traits4models** can be done in different aspects:

 1. **Harmonized trait definition**: The data set `HarmonizedTraitDefinition` sets the nomenclature, definition and units for plant traits in the package. If you want to use the package, but the current trait definition does not include a given trait you need, you could contribute to the package by suggesting new definitions.
 2. **Model parameter building**: At present, **traits4models** provides functions to populate species parameter tables for models in **medfate** package. If you would like to use **traits4models** for parameter estimation of another process-based model, you could develop your own functions and contribute with them to the package.
 3. **Sharing trait/allometry sources**: Discovering and harmonizing trait data bases or allometry databases is a tedious task. Sharing sources and harmonized data sets among users of **traits4models** can be beneficial to many people.

## Reporting bugs and suggesting enchancements

If you want to report a bug or suggest an enhancement, it's a good idea to file an issue to the traits4models repository at GitHub. If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex).

### Code contributions

Before making contributions to the package R code, make sure someone from the **traits4models** team agrees that the change you suggest is needed. 

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("emf-creaf/traits4models", fork = TRUE)`.

*   Install all development dependences with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.

*   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

*  For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). Follow the style described in <https://style.tidyverse.org/news.html>.

# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(tidyr)

# Set target options:
tar_option_set(
  packages = c("traits4models", "readxl", "dplyr", "cli", "readr", "sf"),
  controller = crew::crew_controller_local(workers = 3)
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("data-raw/1_spp_params_es.R")
tar_source("data-raw/2_spp_params_fr.R")
tar_source("data-raw/3_spp_params_us.R")
tar_source("data-raw/4_spp_params_au.R")
tar_source("data-raw/5_spp_params_zm.R")

WFO_path <- "~/OneDrive/EMF_datasets/TaxonomyDatabases/WFO/"
DB_path <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/traits_and_models/"
harmonized_trait_path <- paste0(DB_path,"data/harmonized_trait_sources")
harmonized_allometry_path <- "~/OneDrive/EMF_datasets/AllometryDatabases/Products/harmonized"

rebuild_species_list <- FALSE

values <- tibble(
  method_function = rlang::syms(c("spp_params_es", "spp_params_fr")),
  data_source = c("SpParamsES", "SpParamsFR")
)

# Replace the target list below with your own:
list(
  tar_target(
    name = trait_database_list,
    command = list.files(harmonized_trait_path, full.names = TRUE),
    format = "file"
  ),
  tar_target(
    name = allometry_database_list,
    command = list.files(harmonized_allometry_path, full.names = TRUE),
    format = "file"
  ),
  tar_target(
    name = SpParamsES,
    command = spp_params_es(trait_database_list,
                            allometry_database_list,
                            rebuild_species_list,
                            WFO_path,
                            harmonized_trait_path,
                            harmonized_allometry_path),
    format = "file"
  ),
  tar_target(
    name = SpParamsFR,
    command = spp_params_fr(trait_database_list,
                            allometry_database_list,
                            rebuild_species_list,
                            WFO_path,
                            harmonized_trait_path,
                            harmonized_allometry_path),
    format = "file"
  ),
  tar_target(
    name = SpParamsUS,
    command = spp_params_us(trait_database_list,
                            allometry_database_list,
                            rebuild_species_list,
                            WFO_path,
                            harmonized_trait_path,
                            harmonized_allometry_path),
    format = "file"
  ),
  tar_target(
    name = SpParamsAU,
    command = spp_params_au(trait_database_list,
                            allometry_database_list,
                            rebuild_species_list,
                            WFO_path,
                            harmonized_trait_path,
                            harmonized_allometry_path),
    format = "file"
  ),
  tar_target(
    name = SpParamsZM,
    command = spp_params_zm(trait_database_list,
                            allometry_database_list,
                            rebuild_species_list,
                            WFO_path,
                            harmonized_trait_path,
                            harmonized_allometry_path),
    format = "file"
  )
)

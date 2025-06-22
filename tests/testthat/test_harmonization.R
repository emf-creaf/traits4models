DB_path <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/traits_and_models/"
harmonized_trait_path <- paste0(DB_path,"data/harmonized_trait_sources")
harmonized_allometry_path = "~/OneDrive/EMF_datasets/AllometryDatabases/Products/harmonized"
WFO_file <- paste0(DB_path, "data-raw/wfo_backbone/classification.csv")

test_that("trait taxonomic harmonization can be done", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  db <- readr::read_csv(paste0(DB_path, "data-raw/raw_trait_data/Bartlett_et_al_2016/pnas.1604088113.sd01.csv"),
                        progress = FALSE,
                        show_col_types = FALSE)
  db_var <- db |>
    dplyr::select(Name, "Leaf P50 (MPa)") |>
    dplyr::rename(originalName = Name,
                  VCleaf_P50 = "Leaf P50 (MPa)") |>
    dplyr::mutate(Reference = "Bartlett et al. (2016)",
                  DOI ="xxx",
                  Priority = 3) |>
    dplyr::filter(!is.na(VCleaf_P50))
  db_post <- traits4models::harmonize_taxonomy_WFO(db_var[1:5,], WFO_file, progress = FALSE)
  expect_s3_class(db_post, "data.frame")
  expect_true(check_harmonized_trait(db_post))
})

test_that("harmonization checks are ok",{
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  expect_type(check_harmonized_trait_dir(harmonized_trait_path, verbose = FALSE), "logical")
  expect_type(check_harmonized_allometry_dir(harmonized_allometry_path, verbose = FALSE), "logical")
})

test_that("harmonized data can be loaded",{
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  expect_s3_class(get_trait_data(harmonized_trait_path, "Al2As"), "data.frame")
  expect_s3_class(get_taxon_data(harmonized_trait_path, "Pinus halepensis"), "data.frame")
  expect_s3_class(get_allometry_data(harmonized_allometry_path, "FoliarBiomass"), "data.frame")
})

DB_path <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/traits_and_models/"
harmonized_trait_path <- paste0(DB_path,"data/harmonized_trait_sources")
harmonized_allometry_path <- paste0(DB_path,"data/harmonized_allometry_sources")
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
                  Priority = 3,
                  checkVersion = as.character(packageVersion("traits4models"))) |>
    dplyr::filter(!is.na(VCleaf_P50))
  db_post <- traits4models::harmonize_taxonomy_WFO(db_var[1:5,], WFO_file, progress = FALSE)
  expect_s3_class(db_post, "data.frame")
  expect_true(traits4models::check_harmonized_trait(db_post, verbose = FALSE))

  db_var_long <- db |>
    dplyr::select(Name, "Leaf P50 (MPa)") |>
    dplyr::rename(originalName = Name,
                  Value = "Leaf P50 (MPa)") |>
    dplyr::mutate(Trait = "VCleaf_P50",
                  Units = "MPa",
                  Method = "AE",
                  Reference = "Bartlett et al. (2016)",
                  DOI ="xxx",
                  Priority = 3,
                  checkVersion = as.character(packageVersion("traits4models"))) |>
    dplyr::filter(!is.na(Value))
  db_post <- traits4models::harmonize_taxonomy_WFO(db_var_long[1:5,], WFO_file, progress = FALSE)
  expect_s3_class(db_post, "data.frame")
  expect_false(traits4models::check_harmonized_trait(db_post, verbose = FALSE))
  db_post2 <- db_post |>
    dplyr::mutate(Level = "species",
                  Method = NA)
  expect_false(traits4models::check_harmonized_trait(db_post2, verbose = FALSE))
  db_post3 <- db_post2 |>
    dplyr::mutate(Level = "taxon")
  expect_true(traits4models::check_harmonized_trait(db_post3, verbose = FALSE))
})

test_that("harmonization checks are ok",{
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  expect_type(traits4models::check_harmonized_trait_dir(harmonized_trait_path, verbose = FALSE), "logical")
  expect_type(traits4models::check_harmonized_allometry_dir(harmonized_allometry_path, verbose = FALSE), "logical")
})

test_that("harmonized data can be loaded",{
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  expect_s3_class(traits4models::get_trait_data(harmonized_trait_path, "Al2As", progress = FALSE), "data.frame")
  expect_s3_class(traits4models::get_taxon_data(harmonized_trait_path, "Pinus halepensis", progress = FALSE), "data.frame")
  expect_s3_class(get_allometry_data(harmonized_allometry_path, "FoliarBiomass", progress = FALSE), "data.frame")
})

test_that("harmonized data can be summarized",{
  expect_s3_class(traits4models::taxon_trait_summary(harmonized_trait_path, c("SLA", "Gswmax"), taxonomic_level = "family", progress = FALSE), "data.frame")
  expect_s3_class(traits4models::taxon_trait_summary(harmonized_trait_path, c("SLA", "Gswmax"),
                                                     summary_function = "var",
                                                     taxonomic_level = "family", progress = FALSE), "data.frame")
  expect_s3_class(traits4models::taxon_trait_summary(harmonized_trait_path, c("LifeForm"),
                                                     summary_function = "weightedmode",
                                                     taxonomic_level = "family", progress = FALSE), "data.frame")
})

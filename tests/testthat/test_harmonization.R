DB_path <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/traits_and_models/"

test_that("trait harmonization can be done", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  db <- readr::read_csv(paste0(DB_path, "data-raw/raw_trait_data/Bartlett_et_al_2016/pnas.1604088113.sd01.csv"),
                        progress = FALSE,
                        show_col_types = FALSE)
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
  db_var <- db_var[1:5,] |>
    dplyr::mutate(Reference = "Bartlett et al. (2016)",
                  DOI ="xxx",
                  Priority = 3)
  WFO_file <- paste0(DB_path, "data-raw/wfo_backbone/classification.csv")
  db_post <- traits4models::harmonize_taxonomy_WFO(db_var, WFO_file, progress = FALSE)
})

test_that("harmonization checks are ok",{
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  expect_type(check_harmonized_trait_dir(paste0(DB_path, "data/harmonized_trait_sources"), verbose = FALSE), "logical")
})

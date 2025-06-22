DB_path <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/traits_and_models/"
harmonized_trait_path <- paste0(DB_path,"data/harmonized_trait_sources")
harmonized_allometry_path = "~/OneDrive/EMF_datasets/AllometryDatabases/Products/harmonized"
WFO_file <- paste0(DB_path, "data-raw/wfo_backbone/classification.csv")

test_that("parameter initalisation and harmonized trait filling works",{
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  sp_names_1 <- c("Rosmarinus officinalis", "Pinus contorta")
  df <- data.frame(originalName = sp_names_1)
  df_harm <- traits4models::harmonize_taxonomy_WFO(df, WFO_file, progress = FALSE)
  expect_s3_class(df_harm, "data.frame")
  df_init <- init_medfate_params(df_harm, verbose = FALSE)
  expect_s3_class(df_init, "data.frame")
  df_fill <- fill_medfate_traits(df_init, harmonized_trait_path, parameters = "SLA", progress = FALSE, verbose = FALSE)
  expect_s3_class(df_fill, "data.frame")
})

test_that("check_medfate_params works", {
  expect_s3_class(check_medfate_params(SpParamsES, verbose = FALSE), "data.frame")
  expect_s3_class(check_medfate_params(SpParamsFR, verbose = FALSE), "data.frame")
  expect_s3_class(check_medfate_params(SpParamsUS, verbose = FALSE), "data.frame")
  expect_s3_class(check_medfate_params(SpParamsAU, verbose = FALSE), "data.frame")
})
test_that("complete_medfate_strict works", {
  expect_s3_class(complete_medfate_strict(SpParamsES, verbose = FALSE), "data.frame")
  expect_s3_class(complete_medfate_strict(SpParamsFR, verbose = FALSE), "data.frame")
  expect_s3_class(complete_medfate_strict(SpParamsUS, verbose = FALSE), "data.frame")
  expect_s3_class(complete_medfate_strict(SpParamsAU, verbose = FALSE), "data.frame")
})

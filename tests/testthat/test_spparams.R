DB_path <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/traits_and_models/"
harmonized_trait_path <- paste0(DB_path,"data/harmonized_trait_sources")
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
  expect_type(check_medfate_params(SpParamsES, check_consistency = FALSE, verbose = FALSE), "list")
  expect_type(check_medfate_params(SpParamsFR, check_consistency = FALSE, verbose = FALSE), "list")
  expect_type(check_medfate_params(SpParamsUS, check_consistency = FALSE, verbose = FALSE), "list")
  expect_type(check_medfate_params(SpParamsAU, check_consistency = FALSE, verbose = FALSE), "list")
  expect_type(check_medfate_params(SpParamsZM, check_consistency = FALSE, verbose = FALSE), "list")
})
test_that("complete_medfate_strict works", {
  expect_s3_class(complete_medfate_strict(SpParamsES, progress = FALSE, verbose = FALSE), "data.frame")
  expect_s3_class(complete_medfate_strict(SpParamsFR, progress = FALSE, verbose = FALSE), "data.frame")
  expect_s3_class(complete_medfate_strict(SpParamsUS, progress = FALSE, verbose = FALSE), "data.frame")
  expect_s3_class(complete_medfate_strict(SpParamsAU, progress = FALSE, verbose = FALSE), "data.frame")
  expect_s3_class(complete_medfate_strict(SpParamsZM, progress = FALSE, verbose = FALSE), "data.frame")
})

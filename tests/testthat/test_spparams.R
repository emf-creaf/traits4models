library(traits4models)

sp_names_1 <- c("Salvia rosmarinus", "Pinus contorta")
sp_names_2 <- c("Rosmarinus officinalis", "Pinus contorta", "Quercus ilex subsp. ilex")
accepted_names_2 <- c("Salvia rosmarinus", "Pinus contorta", "Quercus ilex subsp. ilex")
test_that("init_medfate_params returns a data frame",{
  expect_s3_class(init_medfate_params(sp_names_1, verbose = FALSE), "data.frame")
  expect_s3_class(init_medfate_params(sp_names_2, accepted_names_2, verbose = FALSE), "data.frame")
})

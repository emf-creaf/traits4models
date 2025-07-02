
# HarmonizedDefinition ----------------------------------------------------------------
HarmonizedTraitDefinition <-as.data.frame(readxl::read_xlsx("data-raw/HarmonizedTraitDefinition.xlsx",
                                                     sheet=1, na = "NA"), stringsAsFactors=FALSE)
HarmonizedTraitDefinition$Definition <- stringi::stri_enc_toascii(HarmonizedTraitDefinition$Definition)
HarmonizedTraitDefinition$Notation <- stringi::stri_enc_toascii(HarmonizedTraitDefinition$Notation)
HarmonizedTraitDefinition$Type <- stringi::stri_enc_toascii(HarmonizedTraitDefinition$Type)
HarmonizedTraitDefinition$Units <- stringi::stri_enc_toascii(HarmonizedTraitDefinition$Units)
usethis::use_data(HarmonizedTraitDefinition, overwrite = TRUE)

#
# # Family data -------------------------------------------------------------
# DB_path <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/traits_and_models/data-raw/"
# WFO_file <- paste0(DB_path, "wfo_backbone/classification.csv")
# classification  <- readr::read_delim(file = WFO_file,
#                                      delim = "\t", escape_double = FALSE,
#                                      trim_ws = TRUE)|> tibble::tibble()
# families <- classification$scientificName[classification$taxonRank=="family"]
# fam_data <- data.frame(Family = families, Order = NA, Group = NA)
# id_df<-taxize::get_gbifid_(families, messages = TRUE)
# for(i in 1:nrow(fam_data)) {
#   if(nrow(id_df[[i]])>0 && "order" %in% names(id_df[[i]])) {
#     ord <- id_df[[i]]$order[1]
#     if(!is.na(ord)) {
#       fam_data$Order[i] <- ord
#       if(fam_data$Order[i] %in% c("Ginkgoales", "Pinales", "Welwitschiales", "Ephedrales")) {
#         fam_data$Group[i] = "Gymnosperm"
#       } else {
#         fam_data$Group[i] = "Angiosperm"
#       }
#     }
#   }
# }
# fam_data <- fam_data |> dplyr::distinct()
# usethis::use_data(fam_data, internal = TRUE, overwrite = TRUE)



MFWdir <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/"
NFIparamDir <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/NFIs_parametrization/"


# Generate SpParams -------------------------------------------------------
targets::tar_make()

# SpParamsES, SpParamsUS, SpParamsFR, SpParamsAU --------------------------------------
SpParamsES <- readRDS(paste0(NFIparamDir, "data/es/SpParams_final_es.rds"))
# Results of meta-modelling exercise
metamodellingParamsSpecies = readRDS(paste0(MFWdir, "Metamodelling_TR_WUE/data/SpParamsES/metamodelling_params.rds"))
SpParamsES = medfate::modifySpParams(SpParamsES, metamodellingParamsSpecies, subsetSpecies = FALSE)
# Load growth calibration results TO BE CHANGED
# RGRcambiummaxTrees = readRDS(paste0(MFWdir,"GrowthCalibration/Rdata/RGRcambiummax_trees.rds"))
# SpParamsES = medfate::modifySpParams(SpParamsES, RGRcambiummaxTrees, subsetSpecies = FALSE)
# Load ingrowth calibration results
## SHOULD BE RECALIBRATED: THEY REFER TO INGROWTH (~7.5 cm)
recruitmentParamsSpecies = readRDS(paste0(MFWdir,"MortalityRegenerationCalibration/data/final_recruitment_params.rds"))
recruitmentParamsSpecies$RecrTreeHeight <- recruitmentParamsSpecies$RecrTreeHeight/10
recruitmentParamsSpecies$IngrowthTreeDensity <- recruitmentParamsSpecies$RecrTreeDensity
recruitmentParamsSpecies$RecrTreeDensity <- NULL
SpParamsES = medfate::modifySpParams(SpParamsES, recruitmentParamsSpecies, subsetSpecies = FALSE)
# Load Baseline mortality calibration results
mortalityParamsSpecies = readRDS(paste0(MFWdir,"MortalityRegenerationCalibration/data/mort_rates.rds"))
SpParamsES = medfate::modifySpParams(SpParamsES, mortalityParamsSpecies, subsetSpecies = FALSE)
# Load SurvivalModel calibration results
survivalParamsSpecies = readRDS(paste0(MFWdir,"MortalityRegenerationCalibration/data/survival_models.rds"))
SpParamsES = medfate::modifySpParams(SpParamsES, survivalParamsSpecies, subsetSpecies = FALSE)
# Load SurvivalModel calibration results
resproutingParamsSpecies = readxl::read_xlsx(paste0(MFWdir,"MortalityRegenerationCalibration/data-raw/ResproutingMED.xlsx"))
names(resproutingParamsSpecies)[1] = "Species"
SpParamsES = medfate::modifySpParams(SpParamsES, resproutingParamsSpecies, subsetSpecies = FALSE)
usethis::use_data(SpParamsES, overwrite = T)

SpParamsFR <- readRDS(paste0(NFIparamDir, "data/fr/SpParams_final_fr.rds"))
usethis::use_data(SpParamsFR, overwrite = T)
SpParamsUS <- readRDS(paste0(NFIparamDir, "data/us/SpParams_final_us.rds"))
usethis::use_data(SpParamsUS, overwrite = T)
SpParamsAU <- readRDS(paste0(NFIparamDir, "data/au/SpParams_final_au.rds"))
usethis::use_data(SpParamsAU, overwrite = T)


# HarmonizedDefinition ----------------------------------------------------------------
HarmonizedTraitDefinition <-as.data.frame(readxl::read_xlsx("data-raw/HarmonizedTraitDefinition.xlsx",
                                                     sheet=1, na = "NA"), stringsAsFactors=FALSE)
HarmonizedTraitDefinition$Definition <- stringi::stri_enc_toascii(HarmonizedTraitDefinition$Definition)
HarmonizedTraitDefinition$Notation <- stringi::stri_enc_toascii(HarmonizedTraitDefinition$Notation)
HarmonizedTraitDefinition$Type <- stringi::stri_enc_toascii(HarmonizedTraitDefinition$Type)
HarmonizedTraitDefinition$Units <- stringi::stri_enc_toascii(HarmonizedTraitDefinition$Units)
usethis::use_data(HarmonizedTraitDefinition, overwrite = T)


MFWdir <- "~/OneDrive/mcaceres_work/model_development/medfate_development/"
NFIparamDir <- "~/OneDrive/mcaceres_work/model_development/medfate_development/MedfateSpeciesParametrization/NFIs_parametrization/"

# SpParamsES, SpParamsUS, SpParamsFR, SpParamsAU --------------------------------------
SpParamsES <- readRDS(paste0(NFIparamDir, "Rdata/sp/SpParams_filled_strict_allom_sp.rds"))
# Results of meta-modelling exercise
metamodellingParamsSpecies = readRDS(paste0(MFWdir, "Metamodelling_TR_WUE/Rdata/metamodelling_params.rds"))
SpParamsES = medfate::modifySpParams(SpParamsES, metamodellingParamsSpecies, subsetSpecies = FALSE)
# Load growth calibration results
RGRcambiummaxTrees = readRDS(paste0(MFWdir,"GrowthCalibration/Rdata/RGRcambiummax_trees.rds"))
SpParamsES = medfate::modifySpParams(SpParamsES, RGRcambiummaxTrees, subsetSpecies = FALSE)
# Load ingrowth calibration results
## SHOULD BE RECALIBRATED: THEY REFER TO INGROWTH (~7.5 cm)
recruitmentParamsSpecies = readRDS(paste0(MFWdir,"MortalityRegenerationCalibration/Rdata/final_recruitment_params.rds"))
recruitmentParamsSpecies$RecrTreeHeight <- recruitmentParamsSpecies$RecrTreeHeight/10
recruitmentParamsSpecies$IngrowthTreeDensity <- recruitmentParamsSpecies$RecrTreeDensity
recruitmentParamsSpecies$RecrTreeDensity <- NULL
SpParamsES = medfate::modifySpParams(SpParamsES, recruitmentParamsSpecies, subsetSpecies = FALSE)
# Load Baseline mortality calibration results
mortalityParamsSpecies = readRDS(paste0(MFWdir,"MortalityRegenerationCalibration/Rdata/mort_rates.rds"))
SpParamsES = medfate::modifySpParams(SpParamsES, mortalityParamsSpecies, subsetSpecies = FALSE)
# Load SurvivalModel calibration results
survivalParamsSpecies = readRDS(paste0(MFWdir,"MortalityRegenerationCalibration/Rdata/survival_models.rds"))
SpParamsES = medfate::modifySpParams(SpParamsES, survivalParamsSpecies, subsetSpecies = FALSE)
# Load SurvivalModel calibration results
resproutingParamsSpecies = readxl::read_xlsx(paste0(MFWdir,"MortalityRegenerationCalibration/Data/ResproutingMED.xlsx"))
names(resproutingParamsSpecies)[1] = "Species"
SpParamsES = medfate::modifySpParams(SpParamsES, resproutingParamsSpecies, subsetSpecies = FALSE)
usethis::use_data(SpParamsES, overwrite = T)

SpParamsFR <- readRDS(paste0(NFIparamDir, "Rdata/fr/SpParams_filled_strict_allom_fr.rds"))
usethis::use_data(SpParamsFR, overwrite = T)
SpParamsUS <- readRDS(paste0(NFIparamDir, "Rdata/us/SpParams_filled_strict_allom_us.rds"))
usethis::use_data(SpParamsUS, overwrite = T)
SpParamsAU <- readRDS(paste0(NFIparamDir, "Rdata/au/SpParams_filled_strict_allom_au.rds"))
usethis::use_data(SpParamsAU, overwrite = T)

# Species mapping table
# IFN_species_mapping <- read.table("data-raw/IFN_species_mapping.csv", sep="\t", header=TRUE)
# # IFN_species_mapping$Name[IFN_species_mapping$Name == "Arbutus unedo "] <- "Arbutus unedo"
# usethis::use_data(IFN_species_mapping, overwrite = T)

NFI_SP_mapping <- read.table("data-raw/NFI_SP_mapping.csv", sep=";", header=TRUE, na.strings = "")
NFI_SP_mapping$NFICode <- as.character(NFI_SP_mapping$NFICode)
usethis::use_data(NFI_SP_mapping, overwrite = T)

NFI_FR_mapping <- read.table("data-raw/NFI_FR_mapping.csv", sep=";", header=TRUE, na.strings = "")
NFI_FR_mapping$NFICode <- as.character(NFI_FR_mapping$NFICode)
usethis::use_data(NFI_FR_mapping, overwrite = T)

NFI_US_mapping <- read.table("data-raw/NFI_US_mapping.csv", sep=";", header=TRUE, na.strings = "")
NFI_US_mapping$NFICode <- as.character(NFI_US_mapping$NFICode)
usethis::use_data(NFI_US_mapping, overwrite = T)


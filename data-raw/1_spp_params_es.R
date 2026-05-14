spp_params_es <- function(trait_database_list,
                          allometry_database_list,
                          rebuild_species_list,
                          WFO_path,
                          harmonized_trait_path,
                          harmonized_allometry_path) {
  country_code <- "es"
  IFN_path <- "~/OneDrive/mcaceres_datasets/ForestInventories/IFN_old/"
  MFWdir <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/"
  NFIparamDir <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/NFIs_parametrization/"

  cli::cli_h1("SpParamsES")

  if(rebuild_species_list) {
    cli::cli_h2("Building species list")

    # Read taxonomic reference
    lista_patron<-readxl::read_excel(paste0(IFN_path,"data-raw/lista_patron_especies_silvestres_con_sinonimos_tcm30-540560.xlsx"))
    lista_patron_vascular<-lista_patron |>
      dplyr::filter(
        kingdom== "Plantae",
        phylum == "Tracheophyta")|>
      dplyr::filter(IdTaxonomicStatus =="Aceptado/Válido") |>
      dplyr::rename(WithoutAuthorship = WithoutAutorship) |>
      dplyr::select(WithoutAuthorship, ScientificNameAuthorship)

    # Read species codes from IFN2, IFN3 and IFN4
    # SpeciesCodesIFN23 <- readr::read_delim(
    #   paste0(IFN_path,"data-raw/SpeciesCodesIFN23.csv"),
    #   delim = ";",
    #   escape_double = FALSE,
    #   trim_ws = TRUE, locale = readr::locale(encoding = "ISO-8859-1"))|>
    #   dplyr::mutate(
    #     IFNNAME = stringr::str_replace(IFNNAME, " ssp\\.", ""),
    #     IFNNAME = stringr::str_replace(IFNNAME, " spp\\.", ""),
    #     IFNNAME = stringr::str_replace(IFNNAME, " subsp\\.", ""))|>
    #   dplyr::mutate(IFNNAME = iconv(IFNNAME, from = "UTF-8", to = "UTF-8", sub = ""))|>
    #   dplyr::arrange(IFNCODE)|>
    #   tibble::as_tibble()
    #
    # shrub_codes_ifn4 <- readr::read_delim(
    #   paste0(IFN_path,"data-raw/shrub_codes_ifn4.csv"),
    #   delim = ";",
    #   escape_double = FALSE,
    #   trim_ws = TRUE, locale = readr::locale(encoding = "ISO-8859-1"))|>
    #   dplyr::mutate(
    #     IFNNAME = stringr::str_replace(IFNNAME, " ssp\\.", ""),
    #     IFNNAME = stringr::str_replace(IFNNAME, " spp\\.", ""),
    #     IFNNAME = stringr::str_replace(IFNNAME, " subsp\\.", ""))|>
    #   dplyr::mutate(IFNNAME = iconv(IFNNAME, from = "UTF-8", to = "UTF-8", sub = ""))|>
    #   dplyr::arrange(IFNCODE)|>
    #   tibble::as_tibble()
    #
    # tree_codes_ifn4 <- readr::read_delim(
    #   paste0(IFN_path,"data-raw/tree_codes_ifn4.csv"),
    #   delim = ";",
    #   escape_double = FALSE,
    #   trim_ws = TRUE, locale = readr::locale(encoding = "ISO-8859-1")) |>
    #   dplyr::select(-COMMONNAME) |>
    #   dplyr::mutate(
    #     IFNNAME = stringr::str_replace(IFNNAME, " ssp\\.", ""),
    #     IFNNAME = stringr::str_replace(IFNNAME, " spp\\.", ""),
    #     IFNNAME = stringr::str_replace(IFNNAME, " subsp\\.", ""),
    #     IFNNAME = stringr::str_replace(IFNNAME, "Quercus ilex ballota", "Quercus ilex"),
    #     IFNNAME = stringr::str_replace(IFNNAME, " \\(Q\\. humilis\\)", ""),
    #     IFNNAME = stringr::str_replace(IFNNAME, " \\(Q\\. fruticosa\\)", ""))|>
    #   dplyr::mutate(IFNNAME = iconv(IFNNAME, from = "UTF-8", to = "UTF-8", sub = ""))|>
    #   dplyr::arrange(IFNCODE)|>
    #   tibble::as_tibble()
    # Merge IFN codes into one data frame
    # spp_es_df <- SpeciesCodesIFN23 |>
    #   dplyr::full_join(shrub_codes_ifn4, by =c("IFNCODE", "IFNNAME"))|>
    #   dplyr::full_join(tree_codes_ifn4, by =c("IFNCODE", "IFNNAME")) |>
    #   dplyr::rename(NFICode = "IFNCODE",
    #                 NFIName = "IFNNAME") |>
    #   dplyr::mutate(originalName = NFIName)|>
    #   dplyr::arrange(NFICode)|>
    #   tibble::as_tibble()
    ### GET SPECIES LIST FROM FORESTABLES
    spp_es_df <- forestables:::species_ifn_internal|>
      dplyr::rename(NFICode = "SP_CODE",
                    NFIName = "SP_NAME") |>
      dplyr::mutate(originalName = NFIName) |>
      dplyr::mutate(originalName = stringr::str_replace(originalName, " spp\\.", ""),
                    originalName = stringr::str_replace(originalName, " \\(Q\\. humilis\\)", ""),
                    originalName = stringr::str_replace(originalName, " \\(Q\\. fruticosa\\)", ""))


    spp_es_df$originalName[spp_es_df$originalName == "Pinos"] <- "Pinus"
    spp_es_df$originalName[spp_es_df$originalName == "Otros pinos"] <- "Pinus"
    spp_es_df$originalName[spp_es_df$originalName == "Otros quercus"] <- "Quercus"
    spp_es_df$originalName[spp_es_df$originalName == "Otros eucaliptos"] <- "Eucalyptus"
    spp_es_df$originalName[spp_es_df$originalName == "Mezcla de eucaliptos"] <- "Eucalyptus"
    spp_es_df$originalName[spp_es_df$originalName == "Sin asignar"] <- NA
    spp_es_df$originalName[spp_es_df$originalName == "Cultivo en mosaico"] <- NA
    spp_es_df$originalName[stringr::str_detect(spp_es_df$originalName, "^(Mezcla|Otras|Otros)")] <- NA
    spp_es_df$originalName[stringr::str_detect(spp_es_df$originalName, "^(Prados|Pastizal|Herbazal|Matorral|Mosaico|Cultivo)")] <- NA

    spp_es_df <- spp_es_df |>
      dplyr::left_join(lista_patron_vascular, by=c("originalName" = "WithoutAuthorship")) |>
      dplyr::rename(originalNameAuthorship = ScientificNameAuthorship) |>
      dplyr::arrange(NFIName)


    # Perform harmonization with World Flora Online
    spp_es_df_complete <- traits4models::harmonize_taxonomy_WFO(spp_es_df, fs::path(WFO_path, "WFO_Backbone/classification.csv"))
    saveRDS(spp_es_df_complete, file = "data-raw/spp_es_df_complete.rds")
    write.table(spp_es_df_complete, "data-raw/NFI_ES_mapping.csv", sep=";", na = "",
                row.names = FALSE)
  }

  cli::cli_h2("SpParamsES initialisation")
  spp_es_df_complete <- readRDS("data-raw/spp_es_df_complete.rds")
  spp_filt <- spp_es_df_complete |>
    dplyr::select(-originalName, -originalNameAuthorship, -acceptedNameAuthorship, -NFICode) |>
    dplyr::rename(originalName = NFIName)|>
    dplyr::distinct()
  SpParams <- traits4models::init_medfate_params(spp_filt,
                                                 complete_rows = TRUE,
                                                 verbose = FALSE)

  # Filling structural parameters from inventory data -----------------------
  cli::cli_h2("SpParamsES filling parameters from IFN")
  sf_IFN3 <- readRDS("~/OneDrive/mcaceres_work/model_initialisation/medfate_initialisation/IFN2medfate/data/SpParamsES/IFN3/soilmod/IFN3_spain_soilmod_WGS84.rds")
  SpParams<- traits4models::fill_medfate_inventory_traits(SpParams, sf_IFN3,
                                                          progress = TRUE)

  # Fill allometries from databases -----------------------------------------
  cli::cli_h2("SpParamsES filling parameters from harmonized allometries")
  SpParams<- traits4models::fill_medfate_allometries(SpParams, harmonized_allometry_path, verbose = FALSE, replace_previous = FALSE)

  # Fill params from traits -------------------------------------------------
  cli::cli_h2("SpParamsES filling parameters from harmonized traits")
  SpParams<- traits4models::fill_medfate_traits(SpParams, harmonized_trait_path, verbose = FALSE, replace_previous = FALSE, erase_previous = FALSE)

  # Fixing growth forms
  shrubs <- c("Ampelodesmos", "Anthyllis", "Anagyris spp.", "Artemisia campestris", "Asparagus acutifolius",
              "Calicotome spp.", "Cistus laxus", "Cistus psilosepalus", "Cytisus fontanesii subsp. fontanesii", "Erica erigena", "Genista baetica", "Globularia",
              "Halimium atriplicifolium", "Halimium commutatum", "Helichrysum spp.", "Kleinia",
              "Launaea", "Lithodora spp.", "Matorral en mosaico", "Medicago", "Pastizal-Matorral en mosaico",
              "Periploca laevigata", "Rosmarinus", "Rosmarinus tomentosus", "Ruscus", "Rumex", "Ruscus hypophyllum",
              "Sideritis spp.", "Ulex canescens", "Vaccinium spp.", "Vitis vinifera", "Withania frutescens")
  tree <- c("Amelanchier", "Cedrus spp.", "Cupressus spp.", "Dracaena", "Fraxinus spp.",
            "Larix x eurolepis", "Laurisilva", "Mezcla de árboles de ribera", "Mezcla de coníferas",
            "Mezcla de frondosas de gran porte", "Mezcla de pequeñas frondosas", "Myrica rivas-martinezii",
            "Otras coníferas", "Otros árboles ripícolas")
  tree_shrubs <- c("Chamaerops", "Chamaerops humilis", "Crataegus monogyna", "Crataegus spp.", "Corylus avellana", "Erica spp.", "Flueggea", "Flueggea tinctoria",
                   "Juniperus communis ssp. alpina", "Juniperus communis", "Juniperus oxycedrus", "Juniperus cedrus",
                   "Juniperus phoenicea", "Juniperus spp.", "Lonicera xylosteum", "Maytenus canariensis", "Maytenus senegalensis",
                   "Mirtus", "Mirtus communis", "Olea europaea",  "Pinus halepensis", "Pinus nigra", "Paliurus", "Paliurus spina-christi", "Phillyrea latifolia",
                   "Pistacia lentiscus", "Pistacia terebinthus", "Prunus mahaleb", "Prunus spinosa", "Prunus spp.", "Quercus coccifera", "Quercus ilex", "Quercus ilex spp. ilex",
                   "Quercus ilex spp. ballota","Quercus faginea", "Rhamnus alaternus", "Rhamnus spp.", "Sambucus nigra", "Sambucus spp.", "Sambucus racemosa",
                   "Ziziphus", "Ziziphus lotus", "Buxus", "Buxus sempervirens", "Buxus balearica", "Salix atrocinerea")
  SpParams$GrowthForm[SpParams$Name %in% tree] <- "Tree"
  SpParams$GrowthForm[SpParams$Name %in% shrubs] <- "Shrub"
  SpParams$GrowthForm[SpParams$Name %in% tree_shrubs] <- "Tree/Shrub"
  View(SpParams[is.na(SpParams$GrowthForm),c("Name", "GrowthForm")])

  # Complete strict (for taxa) -------------------------------------------------------
  cli::cli_h2("SpParamsES completing strict")
  SpParams <- traits4models::complete_medfate_strict(SpParams)

  # Complete strict for non-taxa or delete them -------------------------------------------------------
  cli::cli_h2("Cleaning and checking")
  mis_strict <- traits4models::check_medfate_params(SpParams)
  SpParams$Name[mis_strict$Genus]
  SpParams <- SpParams|>
    dplyr::filter(!(Name %in% c("Cultivo en mosaico", "Herbazal en mosaico", "Pastizal-Matorral en mosaico",
                                "Matorral en mosaico", "Sin asignar")))
  SpParams[SpParams$Name == "Populus x canadensis",-c(1:4)] <- SpParams[SpParams$Name == "Populus",-c(1:4)]
  SpParams[SpParams$Name == "Larix x eurolepis",-c(1:4)] <- SpParams[SpParams$Name == "Larix spp.",-c(1:4)]
  SpParams[SpParams$Name == "Otras coníferas",-c(1:4)] <- SpParams[SpParams$Name == "Cupressus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Otras coniferas",-c(1:4)] <- SpParams[SpParams$Name == "Cupressus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Mezcla de coniferas",-c(1:4)] <- SpParams[SpParams$Name == "Cupressus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Mezcla de coníferas",-c(1:4)] <- SpParams[SpParams$Name == "Cupressus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Otras frondosas",-c(1:4)] <- SpParams[SpParams$Name == "Prunus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Mezcla de pequeñas frondosas",-c(1:4)] <- SpParams[SpParams$Name == "Prunus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Mezcla de pequenas frondosas",-c(1:4)] <- SpParams[SpParams$Name == "Prunus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Mezcla de frondosas de gran porte",-c(1:4)] <- SpParams[SpParams$Name == "Fraxinus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Otras laurisilvas",-c(1:4)] <- SpParams[SpParams$Name == "Ocotea phoetens",-c(1:4)]
  SpParams[SpParams$Name == "Otros árboles ripícolas",-c(1:4)] <- SpParams[SpParams$Name == "Fraxinus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Otros arboles ripicolas",-c(1:4)] <- SpParams[SpParams$Name == "Fraxinus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Otras papilionoideas altas",-c(1:4)] <- SpParams[SpParams$Name == "Cytisus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Otras papilionoideas bajas",-c(1:4)] <- SpParams[SpParams$Name == "Thymus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Mezcla de árboles de ribera",-c(1:4)] <- SpParams[SpParams$Name == "Fraxinus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Mezcla de arboles de ribera",-c(1:4)] <- SpParams[SpParams$Name == "Fraxinus spp.",-c(1:4)]


  # Modifications from metamodelling/calibration exercises ------------------
  cli::cli_h2("Calibration/metamodelling parameters")
  # Results of meta-modelling exercise
  metamodellingParamsSpecies = readRDS(paste0(MFWdir, "Metamodelling_TR_WUE/data/SpParamsES/metamodelling_params.rds"))
  SpParams = medfate::modifySpParams(SpParams, metamodellingParamsSpecies, subsetSpecies = FALSE)
  # Load growth calibration results TO BE CHANGED
  # RGRcambiummaxTrees = readRDS(paste0(MFWdir,"GrowthCalibration/Rdata/RGRcambiummax_trees.rds"))
  # SpParamsES = medfate::modifySpParams(SpParamsES, RGRcambiummaxTrees, subsetSpecies = FALSE)
  # Load ingrowth calibration results
  ## SHOULD BE RECALIBRATED: THEY REFER TO INGROWTH (~7.5 cm)
  recruitmentParamsSpecies = readRDS(paste0(MFWdir,"MortalityRegenerationCalibration/data/final_recruitment_params.rds"))
  recruitmentParamsSpecies$RecrTreeHeight <- recruitmentParamsSpecies$RecrTreeHeight/10
  recruitmentParamsSpecies$IngrowthTreeDensity <- recruitmentParamsSpecies$RecrTreeDensity
  recruitmentParamsSpecies$RecrTreeDensity <- NULL
  SpParams = medfate::modifySpParams(SpParams, recruitmentParamsSpecies, subsetSpecies = FALSE)
  # Load Baseline mortality calibration results
  mortalityParamsSpecies = readRDS(paste0(MFWdir,"MortalityRegenerationCalibration/data/mort_rates.rds"))
  SpParams = medfate::modifySpParams(SpParams, mortalityParamsSpecies, subsetSpecies = FALSE)
  # Load SurvivalModel calibration results
  survivalParamsSpecies = readRDS(paste0(MFWdir,"MortalityRegenerationCalibration/data/survival_models.rds"))
  SpParams = medfate::modifySpParams(SpParams, survivalParamsSpecies, subsetSpecies = FALSE)
  # Load SurvivalModel calibration results
  resproutingParamsSpecies = readxl::read_xlsx(paste0(MFWdir,"MortalityRegenerationCalibration/data-raw/ResproutingMED.xlsx"))
  names(resproutingParamsSpecies)[1] = "Species"
  SpParams = medfate::modifySpParams(SpParams, resproutingParamsSpecies, subsetSpecies = FALSE)


  mis_strict<-traits4models::check_medfate_params(SpParams)
  out_file <- NULL
  if(sum(as.matrix(mis_strict))==0) {
    out_file <- paste0("data/SpParamsES.rda")
    SpParamsES <- SpParams
    usethis::use_data(SpParamsES, overwrite = TRUE)
  } else {
    cli::cli_abort("Not acceptable!")
  }
  return(out_file)
}



spp_params_es <- function(trait_database_list,
                          allometry_database_list,
                          rebuild_species_list,
                          WFO_path,
                          harmonized_trait_path,
                          harmonized_allometry_path) {
  country_code <- "es"
  IFN_path <- "~/OneDrive/EMF_datasets/ForestInventories/IFN/"
  
  cli::cli_h1("SpParamsES")
  
  if(rebuild_species_list) {
    cli::cli_h2("Building species list")
    
    # Read taxonomic reference 
    lista_patron<-readxl::read_excel(paste0(IFN_path,"Sources/lista_patron_especies_silvestres_con_sinonimos_tcm30-540560.xlsx"))
    lista_patron_vascular<-lista_patron |>
      dplyr::filter(
        kingdom== "Plantae",
        phylum == "Tracheophyta")|>
      dplyr::filter(IdTaxonomicStatus =="Aceptado/Válido") |>
      dplyr::rename(WithoutAuthorship = WithoutAutorship) |>
      dplyr::select(WithoutAuthorship, ScientificNameAuthorship)
    
    # Read species codes from IFN2, IFN3 and IFN4 
    SpeciesCodesIFN23 <- readr::read_delim(
      paste0(IFN_path,"Sources/SpeciesCodesIFN23.csv"), 
      delim = ";", 
      escape_double = FALSE,
      trim_ws = TRUE)
    
    shrub_codes_ifn4 <- readr::read_delim(
      paste0(IFN_path,"Sources/shrub_codes_ifn4.csv"),
      delim = ";", 
      escape_double = FALSE,
      trim_ws = TRUE)
    
    tree_codes_ifn4 <- readr::read_delim(
      paste0(IFN_path,"Sources/tree_codes_ifn4.csv"), 
      delim = ";", 
      escape_double = FALSE,
      trim_ws = TRUE, locale = readr::locale(encoding = "ISO-8859-1")) |>
      dplyr::select(-COMMONNAME)
    
    
    # Merge IFN codes into one data frame 
    spp_es_df <- SpeciesCodesIFN23 |>
      dplyr::full_join(shrub_codes_ifn4, by =c("IFNCODE", "IFNNAME"))|>
      dplyr::full_join(tree_codes_ifn4, by =c("IFNCODE", "IFNNAME")) |> 
      dplyr::rename(NFICode = "IFNCODE",
                    NFIName = "IFNNAME") |>
      dplyr::mutate(
        originalName = NFIName,
        originalName = stringr::str_replace(originalName, " ssp\\.", ""),
        originalName = stringr::str_replace(originalName, " spp\\.", ""),
        originalName = stringr::str_replace(originalName, " subsp\\.", ""),
        originalName = stringr::str_replace(originalName, " \\(Q\\. humilis\\)", ""),
        originalName = stringr::str_replace(originalName, " \\(Q\\. fruticosa\\)", ""))|>
      dplyr::mutate(originalName = iconv(originalName, from = "UTF-8", to = "UTF-8", sub = ""))|>
      dplyr::arrange(NFICode)|>
      tibble::as_tibble()
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
    
    saveRDS(spp_es_df, file = "data/es/spp_es_df.rds")
    
    
    # Perform harmonization with World Flora Online 
    spp_es_df_complete <- traits4models::harmonize_taxonomy_WFO(spp_es_df, fs::path(WFO_path, "WFO_Backbone/classification.csv"))
    saveRDS(spp_es_df_complete, file = "data/es/spp_es_df_complete.rds")
    write.table(spp_es_df_complete, "data/es/NFI_ES_mapping.csv", sep=";", na = "",
                row.names = FALSE)
  }
  
  cli::cli_h2("SpParamsES initialisation")
  spp_es_df_complete <- readRDS("data/es/spp_es_df_complete.rds")
  spp_filt <- spp_es_df_complete |>
    dplyr::select(-originalName, -originalNameAuthorship, -acceptedNameAuthorship, -NFICode) |>
    dplyr::rename(originalName = NFIName)|>
    dplyr::distinct() 
  SpParams <- traits4models::init_medfate_params(spp_filt,
                                                 complete_rows = TRUE, 
                                                 verbose = FALSE)
  saveRDS(SpParams, file = "data/es/SpParams_init_es.rds")
  
  # Filling structural parameters from inventory data -----------------------
  cli::cli_h2("SpParamsES filling parameters from IFN")
  sf_IFN3 <- readRDS("~/OneDrive/mcaceres_work/model_initialisation/medfate_initialisation/IFN2medfate/data/SpParamsES/IFN3/soilmod/IFN3_spain_soilmod_WGS84.rds")
  SpParams <- readRDS(file = paste0("data/", country_code,"/SpParams_init_",country_code,".rds"))
  SpParams<- traits4models::fill_medfate_inventory_traits(SpParams, sf_IFN3, 
                                                          progress = TRUE)
  saveRDS(SpParams, file = paste0("data/", country_code,"/SpParams_struct_", country_code,".rds"))
  
  # Fill allometries from databases -----------------------------------------
  cli::cli_h2("SpParamsES filling parameters from harmonized allometries")
  SpParams <- readRDS(file = paste0("data/", country_code,"/SpParams_struct_",country_code,".rds"))
  SpParams<- traits4models::fill_medfate_allometries(SpParams, harmonized_allometry_path, verbose = FALSE, replace_previous = FALSE)
  saveRDS(SpParams, file = paste0("data/", country_code,"/SpParams_allom_", country_code,".rds"))
  
  
  # Fill params from traits -------------------------------------------------
  cli::cli_h2("SpParamsES filling parameters from harmonized traits")
  SpParams <- readRDS(file = paste0("data/", country_code,"/SpParams_allom_",country_code,".rds"))
  SpParams<- traits4models::fill_medfate_traits(SpParams, harmonized_trait_path, verbose = FALSE, replace_previous = FALSE, erase_previous = FALSE)
  saveRDS(SpParams, file = paste0("data/", country_code,"/SpParams_filled_", country_code,".rds"))
  
  
  # Complete strict (for taxa) -------------------------------------------------------
  cli::cli_h2("SpParamsES completing strict")
  SpParams <- readRDS(file = paste0("data/", country_code,"/SpParams_filled_",country_code,".rds"))
  SpParams <- traits4models::complete_medfate_strict(SpParams)
  saveRDS(SpParams, file = paste0("data/", country_code,"/SpParams_strict_", country_code,".rds"))
  
  
  # Complete strict for non-taxa or delete them -------------------------------------------------------
  cli::cli_h2("Cleaning and checking")
  SpParams <- readRDS(file = paste0("data/", country_code,"/SpParams_strict_",country_code,".rds"))
  mis_strict <- traits4models::check_medfate_params(SpParams)
  SpParams$Name[mis_strict$Genus]
  SpParams <- SpParams|>
    dplyr::filter(!(Name %in% c("Cultivo en mosaico", "Herbazal en mosaico", "Pastizal-Matorral en mosaico",
                                "Matorral en mosaico", "Sin asignar")))
  SpParams[SpParams$Name == "Populus x canadensis",-c(1:4)] <- SpParams[SpParams$Name == "Populus",-c(1:4)]
  SpParams[SpParams$Name == "Larix x eurolepis",-c(1:4)] <- SpParams[SpParams$Name == "Larix spp.",-c(1:4)]
  SpParams[SpParams$Name == "Otras coníferas",-c(1:4)] <- SpParams[SpParams$Name == "Cupressus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Mezcla de coníferas",-c(1:4)] <- SpParams[SpParams$Name == "Cupressus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Otras frondosas",-c(1:4)] <- SpParams[SpParams$Name == "Prunus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Mezcla de pequeñas frondosas",-c(1:4)] <- SpParams[SpParams$Name == "Prunus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Mezcla de frondosas de gran porte",-c(1:4)] <- SpParams[SpParams$Name == "Fraxinus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Otras laurisilvas",-c(1:4)] <- SpParams[SpParams$Name == "Ocotea phoetens",-c(1:4)]
  SpParams[SpParams$Name == "Otros árboles ripícolas",-c(1:4)] <- SpParams[SpParams$Name == "Fraxinus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Otras papilionoideas altas",-c(1:4)] <- SpParams[SpParams$Name == "Cytisus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Otras papilionoideas bajas",-c(1:4)] <- SpParams[SpParams$Name == "Thymus spp.",-c(1:4)]
  SpParams[SpParams$Name == "Mezcla de árboles de ribera",-c(1:4)] <- SpParams[SpParams$Name == "Fraxinus spp.",-c(1:4)]
  
  mis_strict<-traits4models::check_medfate_params(SpParams)
  out_file <- NULL
  if(sum(as.matrix(mis_strict))==0) {
    out_file <- paste0("data/", country_code,"/SpParams_final_",country_code,".rds")
    saveRDS(SpParams, file = out_file)
  } else {
    cli::cli_abort("Not acceptable!")
  }
  return(out_file)
}



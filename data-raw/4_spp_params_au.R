spp_params_au<-function(trait_database_list,
                        allometry_database_list,
                        rebuild_species_list,
                        WFO_path,
                        harmonized_trait_path,
                        harmonized_allometry_path) {
  NFI_path <- "~/OneDrive/EMF_datasets/ForestInventories/AustraliaNFI/"
  country_code <- "au"
  
  cli::cli_h1("SpParamsAU")
  
  if(rebuild_species_list) {
    cli::cli_h2("Building species list")
    flora_list <- readxl::read_xlsx(paste0(NFI_path,"forest_species_and_communities_SOFR2018.xlsx"), sheet = "Flora list")
    sp_list <- data.frame(originalName = flora_list$Species)
    
    # Perform harmonization with World Flora Online 
    spp_au_df_complete <- traits4models::harmonize_taxonomy_WFO(sp_list, fs::path(WFO_path, "WFO_Backbone/classification.csv"))
    saveRDS(spp_au_df_complete, file = "data/au/spp_au_df_complete.rds")
    write.table(spp_au_df_complete, "data/au/NFI_AU_mapping.csv", sep=";", na = "",
                row.names = FALSE)
  }
  
  # SpParams initialization -------------------------------------------------
  cli::cli_h2("SpParamsAU initialisation")
  spp_au_df_complete <- readRDS("data/au/spp_au_df_complete.rds")
  spp_filt <- spp_au_df_complete |>
    dplyr::select(-acceptedNameAuthorship) |>
    dplyr::distinct() 
  SpParams <- traits4models::init_medfate_params(spp_filt,
                                                 complete_rows = TRUE, 
                                                 verbose = FALSE)
  saveRDS(SpParams, file = "data/au/SpParams_init_au.rds")
  
  # Filling structural parameters from inventory data -----------------------
  # files <- list.files("~/OneDrive/mcaceres_work/model_initialisation/medfate_initialisation/IFN/Products/SpParamsES/IFN3/soilmod/", full.names = TRUE)
  # sf_list <- vector("list", length(files))
  # for(i in 1:length(files)) {
  #   sf_list[[i]] <- readRDS(files[i])
  # }
  # sf_IFN3 <- dplyr::bind_rows(sf_list)
  # SpParams <- readRDS(file = paste0("data/", country_code,"/SpParams_init_",country_code,".rds"))
  # SpParams<- traits4models::fill_medfate_inventory_traits(SpParams, sf_IFN3, 
  #                                                         progress = TRUE)
  saveRDS(SpParams, file = paste0("data/", country_code,"/SpParams_struct_", country_code,".rds"))
  
  # Fill allometries from databases -----------------------------------------
  cli::cli_h2("SpParamsAU filling parameters from harmonized allometries")
  SpParams <- readRDS(file = paste0("data/", country_code,"/SpParams_struct_",country_code,".rds"))
  SpParams<- traits4models::fill_medfate_allometries(SpParams, harmonized_allometry_path, verbose = FALSE, replace_previous = FALSE)
  saveRDS(SpParams, file = paste0("data/", country_code,"/SpParams_allom_", country_code,".rds"))
  
  # Fill params from traits -------------------------------------------------
  cli::cli_h2("SpParamsAU filling parameters from harmonized traits")
  SpParams <- readRDS(file = paste0("data/", country_code,"/SpParams_allom_",country_code,".rds"))
  SpParams<- traits4models::fill_medfate_traits(SpParams, harmonized_trait_path, verbose = FALSE, replace_previous = FALSE, erase_previous = FALSE)
  saveRDS(SpParams, file = paste0("data/", country_code,"/SpParams_filled_", country_code,".rds"))
  
  # Complete strict (for taxa) -------------------------------------------------------
  cli::cli_h2("SpParamsAU completing strict")
  SpParams <- readRDS(file = paste0("data/", country_code,"/SpParams_filled_",country_code,".rds"))
  SpParams<- SpParams[!is.na(SpParams$Genus),] # TO BE FIXED
  SpParams <- traits4models::complete_medfate_strict(SpParams)
  saveRDS(SpParams, file = paste0("data/", country_code,"/SpParams_strict_", country_code,".rds"))
  
  # Complete strict for non-taxa or delete them -------------------------------------------------------
  cli::cli_h2("Cleaning and checking")
  SpParams <- readRDS(file = paste0("data/", country_code,"/SpParams_strict_",country_code,".rds"))
  SpParams <- SpParams|>
    dplyr::filter(!is.na(Name))
  mis_strict <- traits4models::check_medfate_params(SpParams)
  SpParams$Name[mis_strict$Family]
  SpParams[SpParams$Name == "Acacia sp. Graveside Gorge (V.J.Levitzke 806) NT Herbarium",-c(1:4)] <- SpParams[SpParams$Name == "Acacia",-c(1:4)]
  SpParams[SpParams$Name == "Burmannia sp.",-c(1:4)] <- SpParams[SpParams$Name == "Burmannia",-c(1:4)]
  SpParams[SpParams$Name == "Chamelaucium sp. Gingin",-c(1:4)] <- SpParams[SpParams$Name == "Chamelaucium",-c(1:4)]
  SpParams[SpParams$Name == "Gaillardia x grandiflora",-c(1:4)] <- SpParams[SpParams$Name == "Gaillardia",-c(1:4)]
  SpParams[SpParams$Name == "Melichrus sp. Gibberagee",-c(1:4)] <- SpParams[SpParams$Name == "Melichrus",-c(1:4)]
  SpParams[SpParams$Name == "Pterostylis sp.",-c(1:4)] <- SpParams[SpParams$Name == "Pterostylis",-c(1:4)]
  SpParams[SpParams$Name == "Rulingia sp. Trigwell Bridge",-c(1:4)] <- SpParams[SpParams$Name == "Rulingia",-c(1:4)]
  SpParams[SpParams$Name == "Spyridium sp. Little Desert",-c(1:4)] <- SpParams[SpParams$Name == "Spyridium",-c(1:4)]
  SpParams[SpParams$Name == "Synaphea sp.",-c(1:4)] <- SpParams[SpParams$Name == "Synaphea",-c(1:4)]
  SpParams[SpParams$Name == "Toechima sp. East Alligator",-c(1:4)] <- SpParams[SpParams$Name == "Toechima",-c(1:4)]
  SpParams <- SpParams|>
    dplyr::filter(!(Name %in% c("Candolaria xanthostimoides",
                                "Helicteres sp. Glenluckie Creek (N.Byrnes 1280) Cowie",
                                "Lychnothamnus barbatus",
                                "Samadera sp. Moonee Creek",
                                "Tephrastrum tenue",
                                "Thomasia sp. Green Hill",
                                "Hydatella sessilis")))
  SpParams<- SpParams[!mis_strict$Family,]
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



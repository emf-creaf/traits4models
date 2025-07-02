spp_params_zm<-function(trait_database_list,
                        allometry_database_list,
                        rebuild_species_list,
                        WFO_path,
                        harmonized_trait_path,
                        harmonized_allometry_path) {
  country_code <- "zm"
  
  cli::cli_h1("SpParamsZM")
  
  if(rebuild_species_list) {
    cli::cli_h2("Building species list")
    
    # Read taxonomic reference 
    sp_list <- read.table("~/OneDrive/Professional/Recerca/Colaboracions/MariaGonzalezSanchis/Zambia/Especies_nuevas_Zambia.txt", row.names=NULL, sep="\t", skip = 1) 
    spp_zm_df <- data.frame(originalName = sp_list$V1) |>
      dplyr::arrange(originalName)
    # sp_list <- readxl::read_excel("~/OneDrive/Professional/Recerca/Colaboracions/MariaGonzalezSanchis/Zambia/Species_list.xlsx")
    # spp_zm_df <- data.frame(originalName = c(sp_list$`Species name`[sp_list$Process=="Yes"],
    #                                          "Coffea arabica")) |>
    #   dplyr::arrange(originalName)
    
    # Perform harmonization with World Flora Online 
    spp_zm_df_complete <- traits4models::harmonize_taxonomy_WFO(spp_zm_df, fs::path(WFO_path, "WFO_Backbone/classification.csv"))
    saveRDS(spp_zm_df_complete, file = "data/zm/spp_zm_df_complete.rds")
    write.table(spp_zm_df_complete, "data/zm/NFI_ZM_mapping.csv", sep=";", na = "",
                row.names = FALSE)
  }
  
  
  # SpParams initialization -------------------------------------------------
  cli::cli_h2("SpParamsZM initialisation")
  spp_zm_df_complete <- readRDS("data/zm/spp_zm_df_complete.rds")
  spp_filt <- spp_zm_df_complete |>
    dplyr::distinct() 
  SpParams <- traits4models::init_medfate_params(spp_filt,
                                                 complete_rows = TRUE, 
                                                 verbose = FALSE)
  saveRDS(SpParams, file = "data/zm/SpParams_init_zm.rds")
  
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
  cli::cli_h2("SpParamsZM filling parameters from harmonized allometries")
  SpParams <- readRDS(file = paste0("data/", country_code,"/SpParams_struct_",country_code,".rds"))
  SpParams<- traits4models::fill_medfate_allometries(SpParams, harmonized_allometry_path, verbose = FALSE, replace_previous = FALSE)
  saveRDS(SpParams, file = paste0("data/", country_code,"/SpParams_allom_", country_code,".rds"))
  
  # Fill params from traits -------------------------------------------------
  cli::cli_h2("SpParamsZM filling parameters from harmonized traits")
  SpParams <- readRDS(file = paste0("data/", country_code,"/SpParams_allom_",country_code,".rds"))
  SpParams<- traits4models::fill_medfate_traits(SpParams, harmonized_trait_path, verbose = FALSE, replace_previous = FALSE, erase_previous = FALSE)
  saveRDS(SpParams, file = paste0("data/", country_code,"/SpParams_filled_", country_code,".rds"))
  
  # Complete strict (for taxa) -------------------------------------------------------
  cli::cli_h2("SpParamsZM completing strict")
  SpParams <- readRDS(file = paste0("data/", country_code,"/SpParams_filled_",country_code,".rds"))
  SpParams <- traits4models::complete_medfate_strict(SpParams)
  saveRDS(SpParams, file = paste0("data/", country_code,"/SpParams_strict_", country_code,".rds"))
  
  # Complete strict for non-taxa or delete them -------------------------------------------------------
  cli::cli_h2("Cleaning and checking")
  SpParams <- readRDS(file = paste0("data/", country_code,"/SpParams_strict_",country_code,".rds"))
  SpParams <- SpParams|>
    dplyr::filter(!is.na(Name))
  mis_strict <- traits4models::check_medfate_params(SpParams)
  SpParams$Name[mis_strict$Genus]
  SpParams <- SpParams|>
    dplyr::filter(!(Name %in% c("Lonokadia absynika", "Unknown", "Unlisted")))
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



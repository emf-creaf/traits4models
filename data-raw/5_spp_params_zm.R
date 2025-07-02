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
    saveRDS(spp_zm_df_complete, file = "data-raw/spp_zm_df_complete.rds")
  }


  # SpParams initialization -------------------------------------------------
  cli::cli_h2("SpParamsZM initialisation")
  spp_zm_df_complete <- readRDS("data-raw/spp_zm_df_complete.rds")
  spp_filt <- spp_zm_df_complete |>
    dplyr::distinct()
  SpParams <- traits4models::init_medfate_params(spp_filt,
                                                 complete_rows = TRUE,
                                                 verbose = FALSE)

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

  # Fill allometries from databases -----------------------------------------
  cli::cli_h2("SpParamsZM filling parameters from harmonized allometries")
  SpParams<- traits4models::fill_medfate_allometries(SpParams, harmonized_allometry_path, verbose = FALSE, replace_previous = FALSE)

  # Fill params from traits -------------------------------------------------
  cli::cli_h2("SpParamsZM filling parameters from harmonized traits")
  SpParams<- traits4models::fill_medfate_traits(SpParams, harmonized_trait_path, verbose = FALSE, replace_previous = FALSE, erase_previous = FALSE)

  # Complete strict (for taxa) -------------------------------------------------------
  cli::cli_h2("SpParamsZM completing strict")
  SpParams <- traits4models::complete_medfate_strict(SpParams)

  # Complete strict for non-taxa or delete them -------------------------------------------------------
  cli::cli_h2("Cleaning and checking")
  SpParams <- SpParams|>
    dplyr::filter(!is.na(Name))
  mis_strict <- traits4models::check_medfate_params(SpParams)
  SpParams$Name[mis_strict$Genus]
  SpParams <- SpParams|>
    dplyr::filter(!(Name %in% c("Lonokadia absynika", "Unknown", "Unlisted")))
  mis_strict<-traits4models::check_medfate_params(SpParams)
  out_file <- NULL
  if(sum(as.matrix(mis_strict))==0) {
    out_file <- paste0("data/SpParamsZM.rda")
    SpParamsZM <- SpParams
    usethis::use_data(SpParamsZM, overwrite = TRUE)
  } else {
    cli::cli_abort("Not acceptable!")
  }
  return(out_file)
}



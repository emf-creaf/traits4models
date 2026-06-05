spp_params_us<-function(trait_database_list,
                        allometry_database_list,
                        rebuild_species_list,
                        WFO_path,
                        harmonized_trait_path,
                        harmonized_allometry_path) {
  country_code <- "us"
  FIA_path <- "~/OneDrive/EMF_datasets/ForestInventories/FIA_forestables/Sources/"

  if(rebuild_species_list) {
    cli::cli_h2("Building species list")
    # Read taxonomic reference
    REF_PLANT_DICTIONARY<-readr::read_csv(paste0(FIA_path,"REF_PLANT_DICTIONARY.csv"))

    # Read species codes
    fia_ref_species<-readr::read_csv(paste0(FIA_path,"REF_SPECIES.csv"))

    # Use taxonomic reference to fill data
    spp_us_df<-fia_ref_species |>
      dplyr::mutate(
        SP_NAME = paste0(
          ifelse(!is.na(GENUS) & GENUS != 0, GENUS, ""),
          ifelse(!is.na(SPECIES) & SPECIES != 0, paste0(" ",SPECIES), ""),
          ifelse(!is.na(SUBSPECIES ) & SUBSPECIES  != 0, paste0(" ssp. ", SUBSPECIES ), ""),
          ifelse(!is.na(VARIETY ) & VARIETY  != 0, paste0(" var. ", VARIETY), ""), "")
      ) |>
      dplyr::select(SPCD, SPECIES_SYMBOL, SP_NAME) |>
      dplyr::left_join(REF_PLANT_DICTIONARY[,c("SYMBOL", "SCIENTIFIC_NAME", "GENERA_BINOMIAL_AUTHOR")], by = c("SPECIES_SYMBOL" = "SYMBOL")) |>
      dplyr::select(-SPECIES_SYMBOL) |>
      dplyr::rename(NFICode = SPCD,
                    NFIName = SP_NAME,
                    originalName = SCIENTIFIC_NAME,
                    originalNameAuthorship = GENERA_BINOMIAL_AUTHOR)|>
      dplyr::mutate(originalName = stringr::str_replace(originalName, " x ", " ")) |>
      dplyr::mutate(originalName = stringr::str_replace(originalName, " spp.", "")) |>
      dplyr::filter(!is.na(originalName)) |>
      dplyr::arrange(NFIName)
    spp_us_df$originalName[spp_us_df$originalName == "Tree broadleaf"] <- NA
    spp_us_df$originalName[spp_us_df$originalName == "Tree evergreen"] <- NA
    spp_us_df$originalName[spp_us_df$originalName == "Tree unknown"] <- NA

    # Perform harmonization with World Flora Online
    spp_us_df_complete <- traits4models::harmonize_taxonomy_WFO(spp_us_df, fs::path(WFO_path, "WFO_Backbone/classification.csv"))
    saveRDS(spp_us_df_complete, file = "data-raw/spp_us_df_complete.rds")
  }

  # SpParams initialization -------------------------------------------------
  cli::cli_h2("SpParamsUS initialisation")
  spp_us_df_complete <- readRDS("data-raw/spp_us_df_complete.rds")
  spp_filt <- spp_us_df_complete |>
    dplyr::select(-originalNameAuthorship, -acceptedNameAuthorship, -NFICode, -NFIName) |>
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
  cli::cli_h2("SpParamsUS filling parameters from harmonized allometries")
  SpParams<- traits4models::fill_medfate_allometries(SpParams, harmonized_allometry_path, verbose = FALSE, replace_previous = FALSE)

  # Fill params from traits -------------------------------------------------
  cli::cli_h2("SpParamsUS filling parameters from harmonized traits")
  SpParams<- traits4models::fill_medfate_traits(SpParams, harmonized_trait_path, verbose = FALSE, replace_previous = FALSE, erase_previous = FALSE)

  # Complete strict (for taxa) -------------------------------------------------------
  cli::cli_h2("SpParamsUS completing strict")
  SpParams <- traits4models::complete_medfate_strict(SpParams)

  # Complete strict for non-taxa or delete them -------------------------------------------------------
  cli::cli_h2("Cleaning and checking")
  SpParams <- SpParams|>
    dplyr::filter(!is.na(Name)) |>
    dplyr::filter(!is.na(Family))
  check <- traits4models::check_medfate_params(SpParams, check_consistency = FALSE)
  SpParams$Name[check$mis_strict$Name]

  check<-traits4models::check_medfate_params(SpParams, check_consistency = FALSE)
  out_file <- NULL
  if(sum(as.matrix(check$mis_strict))==0) {
    out_file <- paste0("data/SpParamsUS.rda")
    SpParamsUS <- SpParams
    usethis::use_data(SpParamsUS, overwrite = TRUE)
  } else {
    cli::cli_abort("Not acceptable!")
  }
  return(out_file)
}



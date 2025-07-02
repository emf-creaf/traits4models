spp_params_fr<-function(trait_database_list,
                        allometry_database_list,
                        rebuild_species_list,
                        WFO_path,
                        harmonized_trait_path,
                        harmonized_allometry_path) {
  country_code <- "fr"
  FFI_path <- "~/OneDrive/EMF_datasets/ForestInventories/FFI_forestables/Sources/"

  cli::cli_h1("SpParamsFR")

  if(rebuild_species_list) {
    # Read taxonomic reference ------------------------------------------------
    TAXREFv13 <- read.delim(fs::path(FFI_path,"TAXREFv13.txt")) |> data.frame() |>
      dplyr::filter(
        REGNE == "Plantae",
        GROUP1_INPN == "Trachéophytes" ,
        RANG %in% c("ES","SSES","VAR","FO")
      ) |>
      dplyr::mutate(CD_NOM = as.character(CD_NOM))|>
      unique()

    # Read species codes ----------------------------
    metadonnees <- readr::read_delim(file = fs::path(FFI_path, "metadonnees.csv"), skip = 419) |>
      dplyr::as_tibble() |>
      dplyr::rename(
        UNITE = "// Unité",
        DEFINITION = "Définition"
      )

    # Take original name and author from TAXREF -------------------------------
    spp_fr_df <- metadonnees |>
      dplyr::filter(UNITE == "CDREF13") |>
      dplyr::rename(NFICode = "Code",
                    NFIName = "Libellé") |>
      dplyr::select(-c(UNITE, DEFINITION)) |>
      dplyr::left_join(TAXREFv13[,c("CD_NOM", "REGNE", "LB_NOM", "LB_AUTEUR")], by=c("NFICode" = "CD_NOM")) |>
      dplyr::filter(REGNE =="Plantae") |>
      dplyr::rename(originalName = "LB_NOM",
                    originalNameAuthorship = "LB_AUTEUR") |>
      dplyr::mutate(originalNameAuthorship = unlist(lapply(strsplit(originalNameAuthorship, ","), function(x) x[[1]]))) |>
      dplyr::select(-REGNE)

    rm(TAXREFv13)
    rm(metadonnees)
    gc()

    # Perform harmonization with World Flora Online ----------------------------------------------------
    spp_fr_df_complete <- traits4models::harmonize_taxonomy_WFO(spp_fr_df, fs::path(WFO_path, "WFO_Backbone/classification.csv"))
    saveRDS(spp_fr_df_complete, file = "data-raw/spp_fr_df_complete.rds")
  }

  # SpParams initialization -------------------------------------------------
  cli::cli_h2("SpParamsFR initialisation")
  spp_fr_df_complete <- readRDS("data-raw/spp_fr_df_complete.rds")
  spp_filt <- spp_fr_df_complete |>
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
  # saveRDS(SpParams, file = paste0("data/", country_code,"/SpParams_struct_", country_code,".rds"))

  # Fill allometries from databases -----------------------------------------
  cli::cli_h2("SpParamsFR filling parameters from harmonized allometries")
  SpParams<- traits4models::fill_medfate_allometries(SpParams, harmonized_allometry_path, verbose = FALSE, replace_previous = FALSE)

  # Fill params from traits -------------------------------------------------
  cli::cli_h2("SpParamsFR filling parameters from harmonized traits")
  SpParams<- traits4models::fill_medfate_traits(SpParams, harmonized_trait_path, verbose = FALSE, replace_previous = FALSE, erase_previous = FALSE)

  # Complete strict (for taxa) -------------------------------------------------------
  cli::cli_h2("SpParamsFR completing strict")
  SpParams <- traits4models::complete_medfate_strict(SpParams)

  # Complete strict for non-taxa or delete them -------------------------------------------------------
  cli::cli_h2("Cleaning and checking")
  mis_strict <- traits4models::check_medfate_params(SpParams)
  SpParams$Name[mis_strict$Genus]
  SpParams[SpParams$Name == "Circaea x intermedia",-c(1:4)] <- SpParams[SpParams$Name == "Circaea",-c(1:4)]
  SpParams[SpParams$Name == "Cotoneaster x intermedius",-c(1:4)] <- SpParams[SpParams$Name == "Cotoneaster",-c(1:4)]
  SpParams[SpParams$Name == "Crataegus x subsphaerica",-c(1:4)] <- SpParams[SpParams$Name == "Crataegus",-c(1:4)]
  SpParams[SpParams$Name == "Hypericum x desetangsii",-c(1:4)] <- SpParams[SpParams$Name == "Hypericum",-c(1:4)]
  SpParams[SpParams$Name == "Larix x marschlinsii",-c(1:4)] <- SpParams[SpParams$Name == "Larix",-c(1:4)]
  SpParams[SpParams$Name == "Narcissus x medioluteus",-c(1:4)] <- SpParams[SpParams$Name == "Narcissus",-c(1:4)]
  SpParams[SpParams$Name == "Platanus x hispanica",-c(1:4)] <- SpParams[SpParams$Name == "Platanus",-c(1:4)]
  SpParams[SpParams$Name == "Polypodium x font-queri",-c(1:4)] <- SpParams[SpParams$Name == "Polypodium",-c(1:4)]
  SpParams[SpParams$Name == "Polystichum x bicknellii",-c(1:4)] <- SpParams[SpParams$Name == "Polystichum",-c(1:4)]
  SpParams[SpParams$Name == "Populus x canadensis",-c(1:4)] <- SpParams[SpParams$Name == "Populus",-c(1:4)]
  SpParams[SpParams$Name == "Populus x generosa",-c(1:4)] <- SpParams[SpParams$Name == "Populus",-c(1:4)]
  SpParams[SpParams$Name == "Populus x canescens",-c(1:4)] <- SpParams[SpParams$Name == "Populus",-c(1:4)]
  SpParams[SpParams$Name == "Rosa x pervirens",-c(1:4)] <- SpParams[SpParams$Name == "Rosa",-c(1:4)]
  SpParams[SpParams$Name == "Salix x rubens",-c(1:4)] <- SpParams[SpParams$Name == "Salix",-c(1:4)]
  SpParams[SpParams$Name == "Salvia x sylvestris",-c(1:4)] <- SpParams[SpParams$Name == "Salvia",-c(1:4)]
  SpParams[SpParams$Name == "Symphyotrichum x salignum",-c(1:4)] <- SpParams[SpParams$Name == "Symphyotrichum",-c(1:4)]
  SpParams[SpParams$Name == "Tilia x europaea",-c(1:4)] <- SpParams[SpParams$Name == "Tilia",-c(1:4)]
  SpParams <- SpParams|>
    dplyr::filter(!(Name %in% c("Potamogeton x zizii", "Rheum x hybridum")))
  mis_strict<-traits4models::check_medfate_params(SpParams)

  out_file <- NULL
  if(sum(as.matrix(mis_strict))==0) {
    out_file <- paste0("data/SpParamsFR.rda")
    SpParamsFR <- SpParams
    usethis::use_data(SpParamsFR, overwrite = TRUE)
  } else {
    cli::cli_abort("Not acceptable!")
  }
  return(out_file)
}



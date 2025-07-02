
# HarmonizedDefinition ----------------------------------------------------------------
HarmonizedTraitDefinition <-as.data.frame(readxl::read_xlsx("data-raw/HarmonizedTraitDefinition.xlsx",
                                                     sheet=1, na = "NA"), stringsAsFactors=FALSE)
HarmonizedTraitDefinition$Definition <- stringi::stri_enc_toascii(HarmonizedTraitDefinition$Definition)
HarmonizedTraitDefinition$Notation <- stringi::stri_enc_toascii(HarmonizedTraitDefinition$Notation)
HarmonizedTraitDefinition$Type <- stringi::stri_enc_toascii(HarmonizedTraitDefinition$Type)
HarmonizedTraitDefinition$Units <- stringi::stri_enc_toascii(HarmonizedTraitDefinition$Units)
usethis::use_data(HarmonizedTraitDefinition, overwrite = TRUE)

# Family data -------------------------------------------------------------
DB_path <- "~/OneDrive/mcaceres_work/model_development/medfate_parameterization/traits_and_models/data-raw/"
WFO_file <- paste0(DB_path, "wfo_backbone/classification.csv")
classification  <- readr::read_delim(file = WFO_file,
                                     delim = "\t", escape_double = FALSE,
                                     trim_ws = TRUE)|> tibble::tibble()
families <- classification$scientificName[classification$taxonRank=="family"]
fam_data <- data.frame(Family = families, Order = NA, Group = NA)
id_df<-taxize::get_gbifid_(families, messages = TRUE)
for(i in 1:nrow(fam_data)) {
  if(nrow(id_df[[i]])>0 && "order" %in% names(id_df[[i]])) {
    ord <- id_df[[i]]$order[1]
    if(!is.na(ord)) {
      fam_data$Order[i] <- ord
      if(fam_data$Order[i] %in% c("Ginkgoales", "Pinales", "Welwitschiales", "Ephedrales")) {
        fam_data$Group[i] = "Gymnosperm"
      } else {
        fam_data$Group[i] = "Angiosperm"
      }
    }
  }
}
fam_data <- fam_data |> dplyr::distinct()
usethis::use_data(fam_data, internal = TRUE, overwrite = TRUE)


# SpParamsES, SpParamsUS, SpParamsFR, SpParamsAU, SpParamsZM ------------------------------------
targets::tar_make()


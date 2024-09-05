# zzz.R
.onLoad <- function(libname, pkgname) {
  load_harmonized_trait_tables <<- memoise::memoise(load_harmonized_trait_tables)
  load_harmonized_allometry_tables <<- memoise::memoise(load_harmonized_allometry_tables)
  read_WFO_backbone <<- memoise::memoise(read_WFO_backbone)
}

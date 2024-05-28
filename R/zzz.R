# zzz.R
.onLoad <- function(libname, pkgname) {
  load_harmonized_tables <<- memoise::memoise(load_harmonized_tables)
  read_WFO_backbone <<- memoise::memoise(read_WFO_backbone)
}

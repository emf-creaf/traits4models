# zzz.R
.onLoad <- function(libname, pkgname) {
  load_harmonized_tables <<- memoise::memoise(load_harmonized_tables)
}

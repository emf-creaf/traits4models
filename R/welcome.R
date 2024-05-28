.onAttach <- function(lib, pkg)  {
  packageStartupMessage("Package 'traits4models' [ver. ",
                        utils::packageDescription("traits4models",
                                                  fields="Version"),"]",
                        appendLF = TRUE)
}

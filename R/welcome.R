.onAttach <- function(lib, pkg)  {
  packageStartupMessage("Package 'medfatetraits' [ver. ",
                        utils::packageDescription("medfatetraits",
                                                  fields="Version"),"]",
                        appendLF = TRUE)
}

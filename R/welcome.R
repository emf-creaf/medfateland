.onAttach <- function(lib, pkg)  {
  packageStartupMessage("Package 'medfateland' [ver. ",
                        utils::packageDescription("medfateland",
                                                  fields="Version"),"]",
                        appendLF = TRUE)
}
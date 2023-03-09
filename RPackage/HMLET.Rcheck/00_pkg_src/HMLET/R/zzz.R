.onLoad <- function(libname, pkgname) {
  options(dplyr.summarise.inform = FALSE)
}

.onAttach <- function(libname, pkgname) {
  options(dplyr.summarise.inform = FALSE)
}

.onDetach <- function(libpath) {
  options(dplyr.summarise.inform = TRUE)
}

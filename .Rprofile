# renv place la bibliothèque hors du projet quand celui-ci est un paquet R
# (présence de DESCRIPTION). On la garde dans renv/library/ (ignoré par git et
# par R CMD build via .Rbuildignore). Le lanceur inst/bin/metabo fait de même.
if (!nzchar(Sys.getenv("RENV_PATHS_LIBRARY")))
  Sys.setenv(RENV_PATHS_LIBRARY = file.path(getwd(), "renv", "library"))
source("renv/activate.R")

# install.R — bootstrap reproductible de l'environnement metabo-cli
#
# Usage (depuis la racine du repo) :
#   Rscript install.R
#
# Idempotent : on peut le relancer sans risque. Il
#   1. vérifie les prérequis système (R >= 4.0, compilateurs C/C++/Fortran) ;
#   2. initialise renv (bibliothèque de paquets propre au projet, comme node_modules)
#      avec le support Bioconductor, ou restaure renv.lock s'il existe ;
#   3. installe uniquement les paquets manquants (CRAN, Bioconductor, GitHub) ;
#   4. vérifie que library(MetaboAnalystR) se charge ;
#   5. termine par renv::snapshot() (l'équivalent d'un package-lock.json).
#
# Note R : avec le R de Homebrew, CRAN et Bioconductor ne fournissent pas de
# binaires compatibles ; tout est compilé depuis les sources (compter 30 à 60 min
# la première fois).

local({

  msg  <- function(...) message(">> ", ...)
  fail <- function(...) stop(paste0(...), call. = FALSE)

  # ---------------------------------------------------------------- 0. contexte
  if (!file.exists("install.R"))
    fail("Lancez ce script depuis la racine du repo metabo-cli.")

  cran <- "https://cloud.r-project.org"
  options(repos = c(CRAN = cran), Ncpus = max(1L, parallel::detectCores() - 1L))
  Sys.setenv(
    MAKEFLAGS = paste0("-j", getOption("Ncpus")),
    # renv ne lance pas `R CMD build` par défaut, donc ne construit jamais les
    # vignettes (l'équivalent de build_vignettes = FALSE) : on le rend explicite.
    RENV_CONFIG_INSTALL_BUILD = "FALSE",
    RENV_CONFIG_PAK_ENABLED   = "FALSE"
  )

  # ------------------------------------------------------ 1. prérequis système
  if (getRversion() < "4.0.0")
    fail("R >= 4.0 requis (trouvé ", getRversion(), ").")
  msg("R ", getRversion(), " (", R.version$platform, ")")

  rcmd <- file.path(R.home("bin"), "R")
  for (tool in c("CC", "CXX", "FC")) {
    cmd <- strsplit(trimws(system2(rcmd, c("CMD", "config", tool), stdout = TRUE)), " ")[[1]][1]
    if (!nzchar(Sys.which(cmd)) && !file.exists(cmd))
      fail("Compilateur ", tool, " introuvable (", cmd, "). ",
           if (tool == "FC") "Installez gfortran." else "Installez les Xcode Command Line Tools (xcode-select --install).")
    msg(tool, " : ", cmd)
  }

  # ------------------------------------------------------------- 2. renv
  if (!requireNamespace("renv", quietly = TRUE)) {
    msg("Installation de renv")
    install.packages("renv")
  }

  if (file.exists("renv/activate.R")) {
    source("renv/activate.R")
  } else {
    msg("Initialisation de renv (bioconductor = TRUE)")
    renv::init(bare = TRUE, bioconductor = TRUE, restart = FALSE, load = TRUE)
  }
  # Le lockfile n'est pas restreint aux paquets « utilisés » par le code :
  # on veut figer toute la bibliothèque du projet.
  renv::settings$snapshot.type("all")

  installed <- function() rownames(utils::installed.packages(lib.loc = renv::paths$library()))

  # Clone neuf : on restaure d'abord les versions exactes du lockfile.
  if (file.exists("renv.lock") && !"MetaboAnalystR" %in% installed()) {
    msg("renv.lock trouvé : restauration des versions figées")
    # MetaboAnalystR est exclu : renv ne sait pas l'installer (voir étape 3).
    renv::restore(prompt = FALSE, exclude = "MetaboAnalystR")
  }

  # ------------------------------------------------------------- 3. paquets
  install_missing <- function(pkgs, label) {
    names <- sub("^.*[:/]", "", pkgs)
    todo  <- pkgs[!names %in% installed()]
    if (length(todo) == 0L) { msg(label, " : déjà installés"); return(invisible()) }
    msg(label, " : ", paste(names[!names %in% installed()], collapse = ", "))
    renv::install(todo, prompt = FALSE)
  }

  # Bioconductor : Imports de MetaboAnalystR + Suggests utilisés par les modules
  # Statistical Analysis [one factor] et Pathway Analysis (voir README).
  bioc <- c(
    # Imports
    "BiocParallel", "RBGL", "edgeR", "fgsea", "impute", "pcaMethods", "siggenes",
    # Suggests
    "graph", "globaltest", "GlobalAncova", "Rgraphviz", "preprocessCore",
    "genefilter", "sva", "limma", "KEGGgraph"
    # MSnbase volontairement exclu : il ne sert qu'au traitement des spectres
    # bruts / MS-MS (spectra_processing.R, tandem_ms.R), et sa dépendance
    # mzR -> ncdf4 exige la librairie système netcdf (brew install netcdf).
  )

  # CRAN : outils du CLI + Suggests (déclarés ou non) appelés par les deux modules.
  cran_pkgs <- c(
    "optparse", "jsonlite", "testthat", "roxygen2", "BiocManager", "remotes",
    "crmn",   # Import de MetaboAnalystR publié sur CRAN
    "pls", "ellipse", "pheatmap", "ggrepel", "patchwork", "qs2", "memoise",
    "viridis", "gridExtra", "reshape", "stringr", "xtable", "rjson", "RJSONIO",
    "httr", "progress", "vegan", "scatterplot3d"
  )

  install_missing(paste0("bioc::", bioc), "Bioconductor")
  install_missing(cran_pkgs, "CRAN")
  # MetaboAnalystR est installé avec remotes::install_github() et non renv::install() :
  #  - le dépôt GitHub contient par erreur un dossier `Meta/` (artefact d'un paquet
  #    installé). renv 1.3 en déduit que l'archive est un binaire, la décompresse
  #    sans la compiler, puis échoue sans message ;
  #  - le champ `Remotes: xia-lab/OptiLCMS` n'est pas suivi (dependencies = FALSE) :
  #    OptiLCMS n'est qu'en Suggests et tire MSnbase -> mzR -> ncdf4 (exclus).
  # La version est épinglée sur le commit enregistré dans renv.lock s'il existe.
  install_metaboanalystr <- function() {
    if ("MetaboAnalystR" %in% installed()) { msg("GitHub : déjà installé"); return(invisible()) }
    ref <- "master"
    if (file.exists("renv.lock")) {
      rec <- jsonlite::read_json("renv.lock")$Packages$MetaboAnalystR
      if (!is.null(rec$RemoteSha)) ref <- rec$RemoteSha
    }
    msg("GitHub : xia-lab/MetaboAnalystR@", ref)
    remotes::install_github(
      "xia-lab/MetaboAnalystR", ref = ref,
      build_vignettes = FALSE, dependencies = FALSE, upgrade = "never",
      lib = renv::paths$library()
    )
  }
  install_metaboanalystr()

  # ------------------------------------------------------ 4. vérification
  ok <- suppressPackageStartupMessages(requireNamespace("MetaboAnalystR", quietly = TRUE))
  if (!ok) fail("MetaboAnalystR est installé mais ne se charge pas.")
  suppressPackageStartupMessages(library(MetaboAnalystR))
  msg("MetaboAnalystR ", as.character(packageVersion("MetaboAnalystR")), " chargé")

  # ------------------------------------------------------------- 5. lockfile
  renv::snapshot(prompt = FALSE)
  msg("Environnement prêt.")
})

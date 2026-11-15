#' Créer le dossier de sortie horodaté d'une exécution
#'
#' Crée `<out_dir>/<module>_<AAAAMMJJ-HHMMSS>/`. Si le dossier existe déjà
#' (deux exécutions dans la même seconde), un suffixe `_2`, `_3`... est ajouté.
#'
#' @param out_dir Dossier parent (créé s'il n'existe pas).
#' @param module Nom du module (`"stats"` ou `"pathway"`).
#' @return Chemin absolu du dossier créé.
#' @keywords internal
create_run_dir <- function(out_dir, module) {
  if (!dir.exists(out_dir) && !dir.create(out_dir, recursive = TRUE, showWarnings = FALSE))
    input_error("Impossible de créer le dossier de sortie : ", out_dir)
  base <- file.path(normalizePath(out_dir), paste0(module, "_", format(Sys.time(), "%Y%m%d-%H%M%S")))
  path <- base
  i <- 1L
  while (dir.exists(path)) {
    i <- i + 1L
    path <- paste0(base, "_", i)
  }
  dir.create(path)
  path
}

#' Lister les fichiers produits (chemins relatifs au dossier d'exécution)
#'
#' @param run_dir Dossier d'exécution.
#' @param pattern Expression régulière optionnelle sur le nom de fichier.
#' @keywords internal
list_outputs <- function(run_dir, pattern = "\\.(png|csv)$") {
  files <- list.files(run_dir, pattern = pattern, recursive = TRUE)
  sort(files[files != "summary.json"])
}

#' Écrire summary.json
#'
#' Le format est stable et destiné à être lu par d'autres programmes :
#' `module`, `status` (`"success"` ou `"error"`), `started_at`, `finished_at`,
#' `input`, `parameters`, `files` (chemins relatifs), `results`, `error`.
#'
#' @param run_dir Dossier d'exécution.
#' @param summary Liste R à sérialiser.
#' @return Chemin du fichier écrit (invisible).
#' @keywords internal
write_summary <- function(run_dir, summary) {
  path <- file.path(run_dir, "summary.json")
  jsonlite::write_json(summary, path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null", digits = NA)
  invisible(path)
}

#' Exécuter du code avec `out_dir` comme répertoire courant
#'
#' MetaboAnalystR écrit ses fichiers (PNG, CSV, .qs) dans le répertoire courant :
#' on s'y place le temps de l'analyse et on restaure le répertoire d'origine
#' en sortie, même en cas d'erreur (`on.exit()` est l'équivalent d'un `finally`).
#'
#' @keywords internal
with_dir <- function(dir, code) {
  owd <- setwd(dir)
  on.exit(setwd(owd), add = TRUE)
  force(code)
}

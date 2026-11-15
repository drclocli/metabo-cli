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

#' Horodatage ISO 8601 avec fuseau
#' @keywords internal
timestamp_iso <- function(time = Sys.time()) format(time, "%Y-%m-%dT%H:%M:%S%z")

#' Versions des composants (pour la traçabilité dans summary.json)
#' @keywords internal
versions_info <- function() {
  v <- function(pkg) tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NA_character_)
  list(metabocli = v("metabocli"), MetaboAnalystR = v("MetaboAnalystR"),
       R = paste(R.version$major, R.version$minor, sep = "."))
}

#' Finaliser une exécution : nettoyage, liste des fichiers, summary.json
#'
#' En cas d'erreur, summary.json est tout de même écrit (status = "error")
#' puis l'erreur est relancée pour que le CLI choisisse le code de sortie.
#'
#' @param run_dir Dossier d'exécution.
#' @param summary Résumé partiel (module, paramètres, entrée...).
#' @param outcome Résultat du pipeline (`list(results, warnings)`) ou condition d'erreur.
#' @param keep_intermediate Garder les fichiers intermédiaires de MetaboAnalystR.
#' @keywords internal
finish_run <- function(run_dir, summary, outcome, keep_intermediate = FALSE) {
  if (!isTRUE(keep_intermediate)) remove_intermediate_files(run_dir)
  summary$finished_at <- timestamp_iso()
  summary$files <- as.list(list_outputs(run_dir))

  if (inherits(outcome, "error")) {
    if (!inherits(outcome, "metabo_error"))
      outcome <- metabo_condition(paste0("Erreur inattendue : ", conditionMessage(outcome)),
                                  "metabo_analysis_error", parent = outcome)
    summary$status <- "error"
    summary$error <- list(
      type = if (inherits(outcome, "metabo_input_error")) "input"
             else if (inherits(outcome, "metabo_network_error")) "network" else "analysis",
      message = conditionMessage(outcome)
    )
    write_summary(run_dir, summary)
    stop(outcome)
  }

  summary$status <- "success"
  summary$results <- outcome$results
  summary$warnings <- c(summary$warnings, as.list(outcome$warnings))
  write_summary(run_dir, summary)
  invisible(list(run_dir = run_dir, summary = summary))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

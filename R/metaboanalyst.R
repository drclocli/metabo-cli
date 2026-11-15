# Adaptateurs autour de MetaboAnalystR.
#
# L'API de MetaboAnalystR est pensée pour le serveur web et n'est pas homogène
# en usage local. Ce fichier isole les contournements nécessaires :
#
# - Valeur de retour : la plupart des fonctions renvoient l'objet mSet mis à
#   jour, mais certaines renvoient un code numérique (FilterVariable renvoie
#   toujours 1 ou 2 et écrit son résultat sur disque) ; en cas d'échec, elles
#   renvoient 0 (ou -1, -2) et stockent le message dans la variable globale
#   `err.vec`. mar_step() normalise tout cela en erreurs R typées.
# - État global : InitDataObjects() crée des variables dans .GlobalEnv
#   (anal.type, primary.user, default.dpi...) que les autres fonctions lisent.
#   Les appels qualifiés `MetaboAnalystR::f()` fonctionnent sans attacher le
#   paquet (vérifié), car .GlobalEnv reste dans la chaîne de recherche des
#   fonctions du paquet ; on n'utilise donc pas library().
# - Rserve : InitDataObjects("stat") démarre un démon Rserve qui n'est plus
#   utilisé (RSclient a été retiré du paquet) et qui survit au processus R.
#   On le neutralise (disable_rserve()).
# - Fichiers : MetaboAnalystR écrit dans le répertoire courant ; les run_*()
#   s'y placent avec setwd() et restaurent le répertoire d'origine (on.exit).

#' Récupérer une fonction interne de MetaboAnalystR
#' @keywords internal
mar_internal <- function(name) utils::getFromNamespace(name, "MetaboAnalystR")

#' Empêcher MetaboAnalystR de démarrer un démon Rserve orphelin
#' @keywords internal
disable_rserve <- function() {
  current <- mar_internal("load_Rserve")
  if (!isTRUE(attr(current, "metabocli_noop"))) {
    noop <- structure(function() invisible(NULL), metabocli_noop = TRUE)
    utils::assignInNamespace("load_Rserve", noop, ns = "MetaboAnalystR")
  }
  invisible(TRUE)
}

#' Messages d'erreur accumulés par MetaboAnalystR (sans balises HTML)
#' @keywords internal
mar_errors <- function() {
  msgs <- get0("err.vec", envir = globalenv(), ifnotfound = character())
  msgs <- gsub("<[^>]+>", "", msgs)
  unique(trimws(msgs[nzchar(trimws(msgs))]))
}

#' Exécuter une étape MetaboAnalystR et normaliser son résultat
#'
#' @param label Nom lisible de l'étape (pour les messages d'erreur).
#' @param call Appel à MetaboAnalystR (évalué paresseusement ici).
#' @param mset Objet mSet courant, renvoyé si l'étape signale un succès par un
#'   code numérique positif au lieu de renvoyer l'objet.
#' @return L'objet mSet à utiliser pour la suite.
#' @keywords internal
mar_step <- function(label, call, mset = NULL) {
  assign("err.vec", character(0), envir = globalenv())
  res <- tryCatch(call, error = function(e) {
    if (inherits(e, "metabo_error")) stop(e)
    analysis_error("Échec de l'étape « ", label, " » : ", conditionMessage(e), parent = e)
  })
  if (is.list(res)) return(res)
  if (is.numeric(res) && length(res) == 1L && !is.na(res) && res > 0) return(mset)
  msgs <- mar_errors()
  analysis_error("Échec de l'étape « ", label, " »",
                 if (length(msgs)) paste0(" : ", paste(msgs, collapse = " ")) else ".")
}

#' Exécuter du code MetaboAnalystR en capturant sa sortie dans un journal
#'
#' Tout ce que MetaboAnalystR imprime (stdout, messages, avertissements) est
#' écrit dans `log_file`. Avec `verbose = TRUE`, c'est aussi affiché.
#' Les périphériques graphiques ouverts pendant l'exécution sont refermés,
#' même en cas d'erreur.
#'
#' @keywords internal
with_mar_log <- function(log_file, verbose, code) {
  con <- file(log_file, open = "wt", encoding = "UTF-8")
  sink_level <- sink.number()
  devices <- grDevices::dev.list()
  sink(con, split = isTRUE(verbose))
  on.exit({
    while (sink.number() > sink_level) sink()
    close(con)
    for (d in setdiff(grDevices::dev.list(), devices)) try(grDevices::dev.off(d), silent = TRUE)
  }, add = TRUE)
  withCallingHandlers(
    force(code),
    message = function(m) {
      cat(conditionMessage(m), file = con)
      if (!isTRUE(verbose)) invokeRestart("muffleMessage")
    },
    warning = function(w) {
      cat("Avertissement : ", conditionMessage(w), "\n", sep = "", file = con)
      if (!isTRUE(verbose)) invokeRestart("muffleWarning")
    }
  )
}

#' Supprimer les fichiers intermédiaires de MetaboAnalystR
#'
#' MetaboAnalystR conserve ses états intermédiaires (.qs, .mat, .rds...) dans
#' le répertoire courant ; ils ne sont utiles qu'au débogage.
#' @keywords internal
remove_intermediate_files <- function(run_dir) {
  junk <- list.files(run_dir, full.names = TRUE,
                     pattern = "\\.(qs|qs2|mat|rds|rda|RData|arrow|json)$|^NUL$")
  junk <- junk[basename(junk) != "summary.json"]
  unlink(junk)
  invisible(basename(junk))
}

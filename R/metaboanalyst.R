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
  # AddErrMsg() ajoute à `current.msg` et `err.vec`, que seul le serveur web
  # initialise ; sans elles, le message d'erreur d'origine est perdu.
  assign("err.vec", character(0), envir = globalenv())
  assign("current.msg", character(0), envir = globalenv())
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
                     pattern = "\\.(qs|qs2|mat|rds|rda|RData|arrow|json)$|^NUL$|^datalist\\.csv$")
  junk <- junk[basename(junk) != "summary.json"]
  unlink(junk)
  invisible(basename(junk))
}

# ---------------------------------------------------------------- réseau

# Serveurs de Xia Lab utilisés par le module pathway en usage local :
# - bibliothèques de composés et de voies (téléchargées, mises en cache) ;
# - API qui calcule l'ORA et la topologie KEGG (reçoit la liste des composés).
XIALAB_ENDPOINTS <- c(
  libs = "https://www.metaboanalyst.ca/resources/libs/compound_db.qs",
  api  = "https://www.xialab.ca/api"
)

#' Vérifier que les serveurs de Xia Lab sont joignables et l'espèce disponible
#'
#' Requêtes HEAD avec délai court. Pour les serveurs, on teste la connexion ;
#' pour l'espèce, l'existence de sa bibliothèque KEGG (404 = espèce non prise
#' en charge par MetaboAnalyst, erreur d'entrée).
#' @param organism Code KEGG de l'espèce, ou NULL pour ne pas le vérifier.
#' @keywords internal
check_xialab_network <- function(organism = NULL, timeout = 15) {
  head_status <- function(url) {
    h <- curl::new_handle(nobody = TRUE, connecttimeout = timeout, timeout = timeout)
    tryCatch(curl::curl_fetch_memory(url, handle = h)$status_code, error = function(e) e)
  }
  for (url in XIALAB_ENDPOINTS) {
    st <- head_status(url)
    if (inherits(st, "error"))
      network_error("Serveur de Xia Lab injoignable (", sub("^https://([^/]+).*", "\\1", url), ") : ",
                    gsub("\\s*\n\\s*", " ", conditionMessage(st)), ". Le module pathway nécessite un accès Internet ",
                    "(bibliothèques de composés et calcul ORA via l'API de MetaboAnalyst).")
  }
  if (!is.null(organism)) {
    st <- head_status(paste0("https://www.metaboanalyst.ca/resources/libs/kegg/metpa/", organism, ".qs"))
    if (identical(st, 404L))
      input_error("Espèce '", organism, "' non disponible dans les bibliothèques KEGG de MetaboAnalyst ",
                  "(exemples : hsa, mmu, rno, bta, gga, dre, dme, cel, sce, eco, ath, osa).")
  }
  invisible(TRUE)
}

# ------------------------------------------------- cache des bibliothèques

# MetaboAnalystR télécharge ses bibliothèques (.qs) dans le répertoire courant
# et les retélécharge si elles ont plus de 30 jours. On les conserve entre deux
# exécutions dans un cache utilisateur (dates de modification préservées).

#' Dossier de cache des bibliothèques MetaboAnalystR
#' @keywords internal
lib_cache_dir <- function() {
  dir <- Sys.getenv("METABO_CACHE_DIR", tools::R_user_dir("metabocli", "cache"))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

#' Copier les bibliothèques en cache dans le dossier d'exécution
#' @keywords internal
restore_lib_cache <- function(run_dir) {
  files <- list.files(lib_cache_dir(), pattern = "\\.qs$", full.names = TRUE)
  if (length(files)) file.copy(files, run_dir, overwrite = TRUE, copy.date = TRUE)
  invisible(basename(files))
}

#' Enregistrer dans le cache les bibliothèques téléchargées
#' @keywords internal
save_lib_cache <- function(run_dir, libs) {
  files <- file.path(run_dir, libs)
  files <- files[file.exists(files) & file.size(files) > 0]
  if (length(files)) file.copy(files, lib_cache_dir(), overwrite = TRUE, copy.date = TRUE)
  invisible(basename(files))
}

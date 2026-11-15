#' Paramètres par défaut du module Pathway Analysis
#'
#' Valeurs identiques à celles proposées par défaut sur metaboanalyst.ca.
#' @export
pathway_defaults <- function() {
  list(
    id_type     = "name",     # noms de composés (alternatives : hmdb, kegg)
    organism    = "hsa",      # bibliothèque KEGG : Homo sapiens
    lib_version = "current",
    ora_method  = "hyperg",   # test hypergéométrique
    topology    = "rbc",      # centralité d'intermédiarité relative
    ref_metabolome = FALSE,   # pas de métabolome de référence
    dpi         = 72,
    keep_intermediate = FALSE
  )
}

# Bibliothèques MetaboAnalystR mises en cache entre deux exécutions
PATHWAY_LIBS <- c("compound_db.qs", "syn_nms.qs")

#' Lancer l'analyse de voies métaboliques (métabolomique ciblée)
#'
#' Reproduit le pipeline de la vignette « Pathway Analysis » : correspondance
#' des composés, bibliothèque KEGG de l'espèce, analyse de sur-représentation
#' (ORA) et de topologie, graphique de synthèse.
#'
#' Nécessite un accès réseau : MetaboAnalystR télécharge les bibliothèques de
#' composés depuis metaboanalyst.ca et délègue le calcul ORA/topologie à l'API
#' de Xia Lab (xialab.ca), à qui la liste des composés est envoyée.
#'
#' @param input Chemin d'un fichier texte, un composé par ligne.
#' @param out_dir Dossier parent ; un sous-dossier `pathway_<horodatage>` y est créé.
#' @param params Liste de paramètres (voir `pathway_defaults()`).
#' @return Liste invisible : `run_dir` et `summary` (contenu de summary.json).
#' @export
run_pathway <- function(input, out_dir, params = list()) {
  p <- utils::modifyList(pathway_defaults(), params)
  verbose <- isTRUE(p$verbose)
  p$verbose <- NULL

  info <- validate_pathway_input(input, p$id_type)
  check_xialab_network(p$organism)
  run_dir <- create_run_dir(out_dir, "pathway")

  summary <- list(
    module = "pathway",
    status = "running",
    versions = versions_info(),
    started_at = timestamp_iso(),
    finished_at = NULL,
    input = list(path = info$path, id_type = p$id_type,
                 n_compounds = length(info$compounds), n_duplicates = info$n_duplicates),
    parameters = p,
    results = NULL,
    files = list(),
    log = "metaboanalyst.log",
    warnings = as.list(info$notes),
    error = NULL
  )

  # MetaboAnalystR écrit dans le répertoire courant : on s'y place et on
  # restaure le répertoire d'origine en sortie, même en cas d'erreur.
  owd <- setwd(run_dir)
  on.exit(setwd(owd), add = TRUE)

  restore_lib_cache(run_dir)
  outcome <- tryCatch(
    with_mar_log(file.path(run_dir, "metaboanalyst.log"), verbose,
                 pathway_pipeline(info$compounds, p)),
    error = function(e) e
  )
  save_lib_cache(run_dir, c(PATHWAY_LIBS, paste0(p$organism, ".qs")))
  finish_run(run_dir, summary, outcome, p$keep_intermediate)
}

# Pipeline proprement dit ; s'exécute dans le dossier de sortie.
pathway_pipeline <- function(compounds, p) {
  M <- asNamespace("MetaboAnalystR")
  warnings <- character()
  mset <- NULL
  step <- function(label, call) mset <<- mar_step(label, call, mset)

  # MetaboAnalystR télécharge ses bibliothèques avec le binaire curl, dont la
  # barre de progression s'écrit directement sur le terminal.
  old_opt <- options(download.file.extra = "--silent --show-error")
  on.exit(options(old_opt), add = TRUE)

  disable_rserve()
  step("Initialisation", M$InitDataObjects("conc", "pathora", FALSE))
  step("Liste de composés", M$Setup.MapData(mset, compounds))

  # Correspondance des identifiants. Si aucun composé n'est reconnu, c'est
  # une erreur d'entrée (mauvaise liste ou mauvais --id-type).
  mapped <- tryCatch(step("Correspondance des composés", M$CrossReferencing(mset, p$id_type)),
                     metabo_analysis_error = function(e) e)
  if (inherits(mapped, "error")) {
    if (any(grepl("matching failed", mar_errors(), ignore.case = TRUE)))
      input_error("Aucun composé reconnu (", length(compounds), " fournis, --id-type ", p$id_type,
                  "). Vérifiez l'orthographe des noms ou le type d'identifiants.")
    stop(mapped)
  }
  step("Table de correspondance", M$CreateMappingResultTable(mset))
  mapping <- compound_mapping(mset, compounds, p$id_type)

  step("Bibliothèque KEGG", M$SetKEGG.PathLib(mset, p$organism, p$lib_version))
  step("Bibliothèque KEGG", M$SetMetabolomeFilter(mset, isTRUE(p$ref_metabolome)))

  # ORA + topologie : calculées par l'API de Xia Lab en usage local.
  ora <- tryCatch(step("Analyse ORA / topologie", M$CalculateOraScore(mset, p$topology, p$ora_method)),
                  metabo_analysis_error = function(e) e)
  if (inherits(ora, "error")) {
    errs <- mar_errors()
    if (any(grepl("connect to the API", errs, ignore.case = TRUE)))
      network_error("L'API de Xia Lab n'a pas répondu (", XIALAB_ENDPOINTS[["api"]], ").")
    if (any(grepl("No valid KEGG compounds", errs)))
      input_error("Aucun des composés reconnus n'a d'identifiant KEGG : analyse de voies impossible.")
    stop(ora)
  }
  ora_mat <- mset$analSet$ora.mat
  if (is.null(ora_mat) || nrow(ora_mat) == 0L)
    analysis_error("L'analyse n'a renvoyé aucune voie pour l'espèce '", p$organism, "'.")

  # Graphique de synthèse : PlotPathSummary() lit les noms de voies dans la
  # variable globale current.kegglib, que SetKEGG.PathLib() ne crée qu'en mode
  # web ; on charge la bibliothèque KEGG de l'espèce nous-mêmes.
  kegglib <- M$.get.my.lib(paste0(p$organism, ".qs"), "kegg/metpa")
  if (is.null(kegglib) || is.null(kegglib$path.ids))
    network_error("Bibliothèque KEGG '", p$organism, "' introuvable sur metaboanalyst.ca.")
  assign("current.kegglib", kegglib, envir = globalenv())
  step("Graphique de synthèse", M$PlotPathSummary(mset, TRUE, "path_view_0_", "png", p$dpi, width = NA))

  # Tables de résultats
  results_df <- pathway_table(ora_mat, kegglib$path.ids, mset$analSet$ora.hits)
  utils::write.csv(results_df, "pathway_results.csv", row.names = FALSE, na = "")
  utils::write.csv(mapping, "compound_mapping.csv", row.names = FALSE, na = "")

  top <- utils::head(results_df, 10L)
  results <- list(
    organism = p$organism,
    library = paste0("KEGG (", p$lib_version, ")"),
    ora_method = p$ora_method,
    topology = p$topology,
    mapping = list(
      n_input = length(compounds),
      n_matched = sum(mapping$status != "unmatched"),
      n_with_kegg = sum(mapping$used_in_pathway_analysis),
      unmatched = as.list(mapping$query[mapping$status == "unmatched"])
    ),
    n_pathways = nrow(results_df),
    n_significant_fdr = sum(results_df$fdr < 0.05, na.rm = TRUE),
    top_pathways = lapply(seq_len(nrow(top)), function(i) list(
      pathway = top$pathway[i], kegg_id = top$kegg_id[i],
      hits = top$hits[i], total = top$total[i],
      p_value = signif(top$raw_p[i], 5), fdr = signif(top$fdr[i], 5),
      impact = signif(top$impact[i], 5)
    ))
  )
  if (results$mapping$n_matched < length(compounds))
    warnings <- c(warnings, paste0(length(compounds) - results$mapping$n_matched,
                                   " composé(s) non reconnu(s) : voir compound_mapping.csv."))
  list(results = results, warnings = warnings)
}

# Table de correspondance des composés, non reconnus inclus, avec des
# suggestions pour les noms non reconnus (recherche approchée de MetaboAnalystR).
compound_mapping <- function(mset, compounds, id_type) {
  M <- asNamespace("MetaboAnalystR")
  tab <- as.data.frame(mset$dataSet$map.table, stringsAsFactors = FALSE)
  na_str <- function(x) { x <- as.character(x); x[x %in% c("NA", "")] <- NA; x }
  df <- data.frame(
    query = tab$Query,
    matched_name = na_str(tab$Match),
    hmdb = na_str(tab$HMDB),
    pubchem = na_str(tab$PubChem),
    kegg = na_str(tab$KEGG),
    stringsAsFactors = FALSE
  )
  df$status <- ifelse(is.na(df$matched_name), "unmatched",
                      ifelse(is.na(df$kegg), "matched_no_kegg", "matched"))
  df$used_in_pathway_analysis <- !is.na(df$kegg) & !duplicated(df$kegg)
  df$suggestions <- NA_character_

  if (id_type == "name") {
    for (i in which(df$status == "unmatched")) {
      cand <- tryCatch({
        m <- M$PerformDetailMatch(mset, df$query[i])
        if (!is.list(m)) m <- mset
        m <- M$GetCandidateList(m, FALSE)
        cl <- if (is.list(m)) m$name.map$hits.candidate.list else NULL
        if (is.null(cl) || NROW(cl) == 0L) NA_character_
        else paste(utils::head(unique(cl[, 1]), 3), collapse = " | ")
      }, error = function(e) NA_character_)
      df$suggestions[i] <- cand
    }
  }
  df
}

# Table des voies : noms, identifiants KEGG, statistiques et composés touchés.
pathway_table <- function(ora_mat, path_ids, ora_hits) {
  ids <- rownames(ora_mat)
  names_by_id <- stats::setNames(names(path_ids), unname(path_ids))
  hits_txt <- vapply(ids, function(id) {
    h <- ora_hits[[id]]
    if (is.null(h) || length(h) == 0L) "" else paste(names(h), collapse = "; ")
  }, "")
  data.frame(
    pathway = unname(ifelse(is.na(names_by_id[ids]), ids, names_by_id[ids])),
    kegg_id = ids,
    total = ora_mat[, "Total"],
    expected = signif(ora_mat[, "Expected"], 5),
    hits = ora_mat[, "Hits"],
    raw_p = ora_mat[, "Raw p"],
    neg_log10_p = signif(-log10(ora_mat[, "Raw p"]), 5),  # MetaboAnalystR donne -ln(p)
    holm_p = ora_mat[, "Holm adjust"],
    fdr = ora_mat[, "FDR"],
    impact = ora_mat[, "Impact"],
    hit_compounds = unname(hits_txt),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

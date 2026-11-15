#' Paramètres par défaut du module Statistical Analysis \[one factor\]
#'
#' Valeurs identiques à celles proposées par défaut sur metaboanalyst.ca
#' (voir README pour les points non vérifiables).
#' @export
stats_defaults <- function() {
  list(
    format        = "rowu",     # échantillons en lignes, non appariés
    missing_max   = NA_real_,   # pas de suppression (case décochée sur le site ; 0.5 si cochée)
    missing       = "lod",      # puis remplacer par 1/5 du minimum positif de la variable
    filter        = "iqr",      # filtre de variance : intervalle interquartile
    filter_cutoff = NA_real_,   # NA = % automatique (5 % si < 250 variables, 10 %, 25 %, 40 %)
    norm          = "NULL",     # normalisation par échantillon : aucune
    trans         = "NULL",     # transformation : aucune
    scale         = "NULL",     # mise à l'échelle : aucune
    pval          = 0.05,       # seuil t-test / ANOVA
    pval_type     = "fdr",      # p-value ajustée (FDR) pour le t-test
    fc            = 2.0,        # seuil de fold change
    volcano_pval  = 0.1,        # seuil du volcano plot (p brute)
    nonpar        = FALSE,
    equal_var     = TRUE,
    dpi           = 72,
    keep_intermediate = FALSE   # garder les fichiers .qs de MetaboAnalystR
  )
}

#' Lancer l'analyse statistique à un facteur
#'
#' Reproduit le pipeline de la vignette « Statistical Analysis » de
#' MetaboAnalystR : lecture, contrôle, valeurs manquantes, filtrage,
#' normalisation, puis t-test, fold change et volcano (2 groupes) ou ANOVA
#' (plus de 2 groupes), PCA, PLS-DA et heatmap.
#'
#' Utilisable directement depuis R :
#' `run_stats("examples/human_cachexia.csv", "out", list(trans = "LogNorm"))`.
#'
#' @param input Chemin du fichier CSV (une colonne d'échantillons, une colonne
#'   de groupes, puis une colonne par variable ; transposé pour `colu`/`colp`).
#' @param out_dir Dossier parent ; un sous-dossier `stats_<horodatage>` y est créé.
#' @param params Liste de paramètres (voir `stats_defaults()`) ; les valeurs
#'   absentes prennent la valeur par défaut. `verbose = TRUE` affiche la
#'   sortie de MetaboAnalystR.
#' @return Liste invisible : `run_dir` et `summary` (contenu de summary.json).
#' @export
run_stats <- function(input, out_dir, params = list()) {
  p <- utils::modifyList(stats_defaults(), params)
  verbose <- isTRUE(p$verbose)
  p$verbose <- NULL

  info <- validate_stats_input(input, p$format)
  run_dir <- create_run_dir(out_dir, "stats")

  summary <- list(
    module = "stats",
    status = "running",
    versions = versions_info(),
    started_at = timestamp_iso(),
    finished_at = NULL,
    input = info[c("path", "format", "separator", "n_samples", "n_features", "groups", "n_missing")],
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

  outcome <- tryCatch(
    with_mar_log(file.path(run_dir, "metaboanalyst.log"), verbose,
                 stats_pipeline(info$path, p)),
    error = function(e) e
  )
  finish_run(run_dir, summary, outcome, p$keep_intermediate)
}

# Pipeline proprement dit ; s'exécute dans le dossier de sortie.
stats_pipeline <- function(path, p) {
  M <- asNamespace("MetaboAnalystR")
  dpi <- p$dpi
  paired <- substr(p$format, 4, 4) == "p"
  warnings <- character()

  mset <- NULL
  step <- function(label, call) mset <<- mar_step(label, call, mset)
  # Étapes secondaires : un échec n'interrompt pas l'analyse, il est signalé
  # dans summary.json (ex. validation croisée PLS-DA sur de très petits groupes).
  optional <- function(label, call) {
    r <- tryCatch({ step(label, call); TRUE }, metabo_analysis_error = function(e) {
      warnings <<- c(warnings, conditionMessage(e))
      FALSE
    })
    invisible(r)
  }

  disable_rserve()
  step("Initialisation", M$InitDataObjects("conc", "stat", paired))
  step("Lecture des données", M$Read.TextData(mset, path, p$format, "disc"))
  step("Contrôle des données", M$SanityCheckData(mset))
  step("Contrôle des données", M$PerformSanityClosure(mset))

  # Même ordre que le site (Data filter, puis Missing value) :
  # 1. suppression optionnelle des variables trop incomplètes (désactivée par
  #    défaut sur le site ; seuil proposé 50 % quand on l'active) ;
  # 2. filtre de variance ;
  # 3. imputation des valeurs manquantes restantes.
  if (!is.na(p$missing_max))
    step("Valeurs manquantes", M$RemoveMissingByPercent(mset, p$missing_max, FALSE))

  # "none" = aucun pourcentage retiré (le plafond de variables de
  # MetaboAnalystR s'applique toujours).
  var_filter <- if (p$filter == "none") "iqr" else p$filter
  var_cutoff <- if (p$filter == "none") 0 else if (is.na(p$filter_cutoff)) NULL else p$filter_cutoff
  step("Filtrage", M$FilterVariable(mset, "F", 20, var_filter, var_cutoff, "mean", 0))
  # FilterVariable() ne renvoie pas l'objet mis à jour : on relit la matrice
  # filtrée pour vérifier qu'il reste des variables avant l'imputation.
  latest <- mar_internal(".get.latest.proc.matrix")(mset)
  check_features_left(if (is.null(latest)) 0L else ncol(latest$data))

  step("Imputation", M$ImputeMissingVar(mset, p$missing))

  step("Normalisation", M$PreparePrenormData(mset))
  step("Normalisation", M$Normalization(mset, p$norm, p$trans, p$scale, ratio = FALSE, ratioNum = 20))
  check_features_left(NCOL(mset$dataSet$norm))
  step("Graphique de normalisation", M$PlotNormSummary(mset, "norm_0_", "png", dpi, width = NA))
  step("Graphique de normalisation", M$PlotSampleNormSummary(mset, "snorm_0_", "png", dpi, width = NA))

  cls <- mset$dataSet$cls
  n_groups <- nlevels(cls)
  n_features <- ncol(mset$dataSet$norm)
  results <- list(
    n_samples = nrow(mset$dataSet$norm),
    n_features_analysed = n_features,
    n_groups = n_groups,
    groups = levels(cls)
  )

  if (n_groups == 2L) {
    step("Fold change", M$FC.Anal(mset, p$fc, 0, paired))
    step("Fold change", M$PlotFC(mset, "fc_0_", "png", dpi, width = NA))
    step("t-test", M$Ttests.Anal(mset, p$nonpar, p$pval, paired, p$equal_var, p$pval_type, FALSE))
    step("t-test", M$PlotTT(mset, "tt_0_", "png", dpi, width = NA))
    step("Volcano", M$Volcano.Anal(mset, paired, p$fc, 0, p$nonpar, p$volcano_pval, p$equal_var, "raw"))
    # PlotVolcano() tente de charger un script du serveur web (util_volcano.Rc)
    # absent du paquet ; la fonction qu'il appelle ensuite est déjà dans le paquet.
    step("Volcano", mar_internal("my.plot.volcano")(mset, "volcano_0_", 1, 0, "png", dpi, NA))

    tt <- mset$analSet$tt
    results$test <- if (isTRUE(p$nonpar)) "wilcoxon" else if (isTRUE(p$equal_var)) "t-test" else "welch"
    results$significance <- list(threshold = p$pval, p_type = p$pval_type)
    results$n_significant <- as.integer(tt$sig.num %||% 0L)
    results$top_features <- top_features(tt$sig.mat, "t.stat", log2fc = mset$analSet$fc$fc.log)
    results$fold_change <- list(threshold = p$fc, n_significant = nrow_or_0(mset$analSet$fc$sig.mat))
    results$volcano <- list(fc_threshold = p$fc, p_threshold = p$volcano_pval, p_type = "raw",
                            n_significant = nrow_or_0(mset$analSet$volcano$sig.mat))
  } else {
    step("ANOVA", M$ANOVA.Anal(mset, p$nonpar, p$pval, FALSE))
    step("ANOVA", M$PlotANOVA(mset, "aov_0_", "png", dpi, width = NA))
    aov <- mset$analSet$aov
    results$test <- if (isTRUE(p$nonpar)) "kruskal-wallis" else "anova"
    results$significance <- list(threshold = p$pval, p_type = "raw")
    results$n_significant <- as.integer(aov$sig.num %||% 0L)
    results$top_features <- top_features(aov$sig.mat, "F.stat")
  }

  # PCA
  n_pc <- max(2L, min(5L, n_features, nrow(mset$dataSet$norm) - 1L))
  step("PCA", M$PCA.Anal(mset))
  step("PCA", M$PlotPCAPairSummary(mset, "pca_pair_0_", "png", dpi, width = NA, n_pc))
  step("PCA", M$PlotPCAScree(mset, "pca_scree_0_", "png", dpi, width = NA, n_pc))
  step("PCA", M$PlotPCA2DScore(mset, "pca_score2d_0_", "png", dpi, width = NA, 1, 2, 0.95, 1, 0))
  step("PCA", M$PlotPCALoading(mset, "pca_loading_0_", "png", dpi, width = NA, 1, 2))
  step("PCA", M$PlotPCABiplot(mset, "pca_biplot_0_", "png", dpi, width = NA, 1, 2))
  variance <- mset$analSet$pca$variance
  if (length(variance) >= 2L)
    results$pca <- list(pc1_variance = round(unname(variance[1]), 4),
                        pc2_variance = round(unname(variance[2]), 4))

  # PLS-DA
  step("PLS-DA", M$PLSR.Anal(mset, reg = TRUE))
  step("PLS-DA", M$PlotPLSPairSummary(mset, "pls_pair_0_", "png", dpi, width = NA, n_pc))
  step("PLS-DA", M$PlotPLS2DScore(mset, "pls_score2d_0_", "png", dpi, width = NA, 1, 2, 0.95, 1, 0))
  step("PLS-DA", M$PlotPLSLoading(mset, "pls_loading_0_", "png", dpi, width = NA, 1, 2))
  if (optional("Validation croisée PLS-DA", M$PLSDA.CV(mset, "5", 5, choice = "Q2")))
    optional("Validation croisée PLS-DA", M$PlotPLS.Classification(mset, "pls_cv_0_", "png", dpi, width = NA))
  optional("Importance PLS-DA (VIP)",
           M$PlotPLS.Imp(mset, "pls_imp_0_", "png", dpi, width = NA, "vip", "Comp. 1", 15, FALSE))

  # Heatmap (version statique : PlotHeatMap() exige iheatmapr pour la version
  # interactive du site web ; PlotStaticHeatMap() produit le PNG). Au-delà de
  # 2000 variables, on garde les 2000 de plus grand IQR, comme PlotHeatMap().
  var_inx <- NULL
  norm <- mset$dataSet$norm
  if (ncol(norm) > 2000L) {
    iqr <- apply(norm, 2, stats::IQR, na.rm = TRUE)
    var_inx <- colnames(norm)[rank(-iqr, ties.method = "first") <= 2000L]
  }
  step("Heatmap", M$PlotStaticHeatMap(mset, "heatmap_0_", "png", dpi, NA, "norm", "row",
                                      "euclidean", "ward.D", "bwm", 8, 8, viewOpt = "overview",
                                      var.inx = var_inx))

  list(results = results, warnings = warnings)
}

# Top 10 des variables significatives, triées par p-value.
top_features <- function(sig_mat, stat_col, log2fc = NULL, n = 10L) {
  if (is.null(sig_mat) || NROW(sig_mat) == 0L) return(list())
  m <- as.data.frame(sig_mat, check.names = FALSE)
  m <- utils::head(m[order(m[["p.value"]]), , drop = FALSE], n)
  lapply(seq_len(nrow(m)), function(i) {
    feat <- rownames(m)[i]
    row <- list(
      feature = feat,
      statistic = unname(signif(m[i, stat_col], 5)),
      p_value = unname(signif(m[i, "p.value"], 5)),
      fdr = if ("FDR" %in% names(m)) unname(signif(m[i, "FDR"], 5)) else NULL
    )
    if (!is.null(log2fc) && feat %in% names(log2fc))
      row$log2_fc <- unname(signif(log2fc[[feat]], 5))
    row
  })
}

check_features_left <- function(n_left) {
  if (n_left < 2L)
    analysis_error("Il reste ", n_left, " variable(s) après le contrôle (variables constantes ",
                   "supprimées), les valeurs manquantes et le filtrage ; au moins 2 sont nécessaires. ",
                   "Essayez --filter none, ou un --missing-max plus élevé.")
  invisible(n_left)
}

nrow_or_0 <- function(x) if (is.null(x)) 0L else NROW(x)

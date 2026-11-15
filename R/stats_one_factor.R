#' Paramètres par défaut du module Statistical Analysis \[one factor\]
#'
#' Valeurs identiques à celles proposées par défaut sur metaboanalyst.ca.
#' @export
stats_defaults <- function() {
  list(
    format     = "rowu",   # échantillons en lignes, non appariés
    label_type = "disc",   # groupes discrets (one factor)
    missing    = "lod",    # valeurs manquantes -> 1/5 du min positif de la variable
    filter     = "iqr",    # filtre de variance : intervalle interquartile
    filter_cutoff = NA_real_, # NA = pourcentage automatique selon le nb de variables
    norm       = "NULL",   # normalisation par échantillon : aucune
    trans      = "NULL",   # transformation : aucune
    scale      = "NULL",   # mise à l'échelle : aucune
    pval       = 0.05,
    pval_type  = "fdr",
    fc         = 2.0,
    nonpar     = FALSE,
    equal_var  = TRUE,
    dpi        = 72
  )
}

#' Lancer l'analyse statistique à un facteur
#'
#' @param input Chemin du fichier CSV (concentrations ou intensités, avec une
#'   colonne d'échantillons et une colonne de groupes).
#' @param out_dir Dossier parent ; un sous-dossier `stats_<horodatage>` y est créé.
#' @param params Liste de paramètres (voir `stats_defaults()`) ; les valeurs
#'   absentes prennent la valeur par défaut.
#' @return Liste invisible : `run_dir` et le contenu de `summary.json`.
#' @export
run_stats <- function(input, out_dir, params = list()) {
  input <- validate_input_file(input)
  analysis_error("Le module stats n'est pas encore implémenté.")
}

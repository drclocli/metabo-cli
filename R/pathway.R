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
    dpi         = 72
  )
}

#' Lancer l'analyse de voies métaboliques (métabolomique ciblée)
#'
#' Nécessite un accès réseau : MetaboAnalystR télécharge les bibliothèques de
#' composés et de voies depuis les serveurs de Xia Lab.
#'
#' @param input Chemin d'un fichier texte, un composé par ligne.
#' @param out_dir Dossier parent ; un sous-dossier `pathway_<horodatage>` y est créé.
#' @param params Liste de paramètres (voir `pathway_defaults()`).
#' @return Liste invisible : `run_dir` et le contenu de `summary.json`.
#' @export
run_pathway <- function(input, out_dir, params = list()) {
  input <- validate_input_file(input)
  analysis_error("Le module pathway n'est pas encore implémenté.")
}

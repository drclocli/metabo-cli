# Validation des entrées avant tout appel à MetaboAnalystR.
# Chaque fonction lève input_error() (code de sortie 1) avec un message explicite.

#' Vérifier qu'un fichier d'entrée existe, est lisible et non vide
#'
#' @param path Chemin du fichier.
#' @return Chemin absolu normalisé.
#' @keywords internal
validate_input_file <- function(path) {
  if (is.null(path) || !nzchar(path))
    input_error("Aucun fichier d'entrée : utilisez --input <fichier>.")
  if (dir.exists(path))
    input_error("'", path, "' est un dossier, pas un fichier.")
  if (!file.exists(path))
    input_error("Fichier introuvable : ", path)
  if (file.access(path, 4L) != 0L)
    input_error("Fichier illisible (permissions) : ", path)
  if (file.size(path) == 0L)
    input_error("Fichier vide : ", path)
  normalizePath(path)
}

#' Vérifier qu'une valeur fait partie d'un ensemble autorisé
#'
#' @param value Valeur reçue.
#' @param allowed Valeurs autorisées.
#' @param option Nom de l'option (pour le message).
#' @keywords internal
validate_choice <- function(value, allowed, option) {
  if (length(value) != 1L || !value %in% allowed)
    input_error("Valeur invalide pour ", option, " : '", paste(value, collapse = ","),
                "'. Valeurs possibles : ", paste(allowed, collapse = ", "), ".")
  value
}

#' Vérifier qu'une valeur numérique est dans un intervalle
#' @keywords internal
validate_number <- function(value, option, min = -Inf, max = Inf) {
  if (length(value) != 1L || is.na(value) || !is.numeric(value) || value < min || value > max)
    input_error("Valeur invalide pour ", option, " : '", value, "' (attendu : nombre entre ",
                min, " et ", max, ").")
  value
}

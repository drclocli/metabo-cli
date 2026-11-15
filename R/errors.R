# Conditions typées : permettent à la couche CLI de choisir le code de sortie
# sans connaître la logique métier.
#
# Note R : une « condition » R est l'équivalent d'une exception ; on lui donne
# une classe (vecteur de chaînes) et tryCatch() dispatch dessus, comme un
# `catch (e) { if (e instanceof InputError) ... }`.

metabo_condition <- function(message, class, parent = NULL) {
  structure(
    class = c(class, "metabo_error", "error", "condition"),
    list(message = message, call = NULL, parent = parent)
  )
}

#' Signaler une erreur d'entrée utilisateur (code de sortie 1)
#'
#' @param ... Morceaux du message, concaténés avec `paste0()`.
#' @keywords internal
input_error <- function(...) {
  stop(metabo_condition(paste0(...), "metabo_input_error"))
}

#' Signaler une erreur d'analyse ou d'environnement (code de sortie 2)
#'
#' @param ... Morceaux du message, concaténés avec `paste0()`.
#' @param parent Condition d'origine éventuelle (affichée avec `--verbose`).
#' @keywords internal
analysis_error <- function(..., parent = NULL) {
  stop(metabo_condition(paste0(...), "metabo_analysis_error", parent = parent))
}

#' Erreur réseau (serveurs Xia Lab injoignables) : sous-classe d'erreur d'analyse
#' @keywords internal
network_error <- function(..., parent = NULL) {
  stop(metabo_condition(paste0(...), c("metabo_network_error", "metabo_analysis_error"),
                        parent = parent))
}

#' Code de sortie associé à une condition
#' @keywords internal
exit_code_for <- function(cond) {
  if (inherits(cond, "metabo_input_error")) 1L else 2L
}

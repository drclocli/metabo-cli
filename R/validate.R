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

#' Détecter le séparateur de colonnes d'un fichier texte
#'
#' Compte les virgules, points-virgules et tabulations hors guillemets sur les
#' premières lignes ; renvoie le séparateur le plus fréquent et constant.
#' @keywords internal
detect_separator <- function(path) {
  lines <- readLines(path, n = 20L, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) < 2L)
    input_error("Le fichier doit contenir un en-tête et au moins une ligne de données : ", path)
  unquoted <- gsub('"[^"]*"', "", lines)
  counts <- vapply(c("," = ",", ";" = ";", "\t" = "\t"), function(s)
    lengths(regmatches(unquoted, gregexpr(s, unquoted, fixed = TRUE))), integer(length(lines)))
  counts <- matrix(counts, nrow = length(lines))
  colnames(counts) <- c(",", ";", "\t")
  ok <- apply(counts, 2, function(x) all(x > 0) && length(unique(x)) == 1L)
  if (!any(ok)) {
    best <- colnames(counts)[which.max(colSums(counts))]
    if (sum(counts) == 0L)
      input_error("Aucun séparateur reconnu (virgule, point-virgule ou tabulation) dans ", path)
    input_error("Nombre de colonnes irrégulier selon les lignes (séparateur '",
                if (best == "\t") "tabulation" else best, "') : vérifiez le fichier ", path)
  }
  colnames(counts)[ok][which.max(counts[1, ok])]
}

#' Valider le tableau d'entrée du module stats
#'
#' Contrôles : séparateur, colonne d'échantillons (noms uniques, non vides),
#' colonne de groupes (au moins 2 groupes, au moins 3 échantillons par groupe,
#' hors QC/blank), valeurs numériques, et pour les formats appariés des
#' étiquettes de paires valides (entiers non nuls, -n..-1 et 1..n).
#'
#' @param path Chemin du fichier.
#' @param format `rowu`, `rowp`, `colu` ou `colp`.
#' @return Liste décrivant les données (dimensions, groupes, séparateur,
#'   nombre de valeurs manquantes).
#' @keywords internal
validate_stats_input <- function(path, format = "rowu") {
  path <- validate_input_file(path)
  sep <- detect_separator(path)
  raw <- tryCatch(
    utils::read.table(path, sep = sep, header = FALSE, quote = "\"", comment.char = "",
                      colClasses = "character", na.strings = character(0),
                      check.names = FALSE, strip.white = TRUE, fill = FALSE,
                      encoding = "UTF-8", blank.lines.skip = TRUE),
    error = function(e) input_error("Lecture impossible de ", basename(path), " : ", conditionMessage(e))
  )
  raw <- as.matrix(raw)
  by_row <- substr(format, 1, 3) == "row"
  if (!by_row) raw <- t(raw)
  layout <- if (by_row) "échantillons en lignes" else "échantillons en colonnes"
  unit_sample <- if (by_row) "ligne" else "colonne"

  if (ncol(raw) < 3L)
    input_error("Au moins 3 ", if (by_row) "colonnes" else "lignes",
                " attendues (échantillon, groupe, au moins une variable) avec le format '", format,
                "' (", layout, "). Vérifiez --format.")

  header <- raw[1, ]
  body <- raw[-1, , drop = FALSE]
  filled <- matrix(nzchar(trimws(body)), nrow = nrow(body))
  body <- body[rowSums(filled) > 0, , drop = FALSE]  # lignes vides
  samples <- trimws(body[, 1])
  groups <- trimws(body[, 2])
  values <- body[, -(1:2), drop = FALSE]
  features <- trimws(header[-(1:2)])

  # Échantillons
  if (any(!nzchar(samples)))
    input_error(sum(!nzchar(samples)), " échantillon(s) sans nom (", unit_sample, "s ",
                paste(utils::head(which(!nzchar(samples)) + 1L, 5), collapse = ", "), ").")
  dup <- unique(samples[duplicated(samples)])
  if (length(dup))
    input_error("Noms d'échantillons en double : ", paste(utils::head(dup, 5), collapse = ", "), ".")

  # Variables
  if (any(!nzchar(features)))
    input_error(sum(!nzchar(features)), " variable(s) sans nom dans l'en-tête.")
  dupf <- unique(features[duplicated(features)])
  if (length(dupf))
    input_error("Noms de variables en double : ", paste(utils::head(dupf, 5), collapse = ", "), ".")

  # Groupes
  if (any(!nzchar(groups)))
    input_error(sum(!nzchar(groups)), " échantillon(s) sans groupe (2e ",
                if (by_row) "colonne" else "ligne", " vide) : ",
                paste(utils::head(samples[!nzchar(groups)], 5), collapse = ", "), ".")
  # Appariement : la colonne de groupes contient des étiquettes de paires
  # (1..n et -1..-n) ; MetaboAnalystR en déduit 2 groupes d'après le signe.
  paired <- substr(format, 4, 4) == "p"
  if (paired) {
    lab <- suppressWarnings(as.integer(groups))
    if (anyNA(lab) || any(lab == 0L) || any(as.character(lab) != groups))
      input_error("Format apparié '", format, "' : la colonne de groupes doit contenir des entiers ",
                  "non nuls (1, 2, ... pour un groupe et -1, -2, ... pour leurs paires).")
    n <- length(lab) / 2
    if (n != round(n) || !setequal(lab, c(-seq_len(n), seq_len(n))) || anyDuplicated(lab))
      input_error("Format apparié '", format, "' : étiquettes de paires invalides ; attendu ",
                  "chaque entier de 1 à n et son opposé exactement une fois.")
    analysed <- ifelse(lab > 0L, "positif", "negatif")
  } else {
    analysed <- groups[!tolower(groups) %in% c("qc", "blank")]
  }
  tab <- table(analysed)
  if (!paired && length(tab) > 10L && length(tab) > length(analysed) / 3)
    input_error(length(tab), " groupes différents pour ", length(analysed), " échantillons : ",
                "la 2e ", if (by_row) "colonne" else "ligne",
                " ne semble pas contenir les groupes. Vérifiez --format (actuellement '", format, "').")
  if (length(tab) < 2L)
    input_error("Au moins 2 groupes sont nécessaires ; trouvé : ",
                if (length(tab)) paste0("'", names(tab), "'") else "aucun", ".")
  if (any(tab < 3L))
    input_error("Au moins 3 échantillons par groupe sont nécessaires ; insuffisant pour : ",
                paste0(names(tab)[tab < 3L], " (", tab[tab < 3L], ")", collapse = ", "), ".")

  # Valeurs numériques (vides et NA acceptés comme valeurs manquantes)
  missing_tokens <- c("", "NA", "NaN", "na", "nan", "N/A", "n/a", "NULL", "null")
  is_missing <- matrix(values %in% missing_tokens, nrow = nrow(values))
  num <- suppressWarnings(as.numeric(values))
  bad <- which(is.na(num) & !is_missing)
  if (length(bad)) {
    rc <- arrayInd(bad, dim(values))
    ex <- paste0("'", values[bad], "' (", samples[rc[, 1]], " / ", features[rc[, 2]], ")")
    hint <- if (sep == ";" && any(grepl("^-?[0-9]+,[0-9]+$", values[bad])))
      " Le fichier semble utiliser des virgules décimales : convertissez-les en points." else ""
    input_error(length(bad), " valeur(s) non numérique(s), par ex. ",
                paste(utils::head(ex, 3), collapse = ", "), ".", hint)
  }
  if (all(is.na(num)))
    input_error("Aucune valeur numérique dans le tableau.")
  notes <- character()
  if (any(num < 0, na.rm = TRUE))
    notes <- c(notes, "Le tableau contient des valeurs négatives (les transformations log les traitent comme manquantes).")

  list(
    path = path, separator = sep, format = format,
    n_samples = length(samples), n_features = length(features),
    groups = as.list(stats::setNames(as.integer(tab), names(tab))),
    n_missing = sum(is.na(num)), notes = notes
  )
}

# Couche CLI : parse les arguments, appelle run_stats()/run_pathway(), traduit
# les erreurs en codes de sortie. Aucune logique métier ici.

# Valeurs autorisées. Les noms suivent ceux de MetaboAnalystR ; "none" est un
# alias lisible de la valeur "NULL" attendue par Normalization().
STATS_CHOICES <- list(
  format  = c("rowu", "rowp", "colu", "colp"),
  missing = c("lod", "exclude", "colmin", "mean", "median",
              "knn_var", "knn_smp", "bpca", "ppca", "svdImpute"),
  filter  = c("none", "iqr", "sd", "mad", "rsd", "nrsd", "mean", "median"),
  norm    = c("none", "SumNorm", "MedianNorm", "QuantileNorm"),
  trans   = c("none", "LogNorm", "Log2Norm", "SrNorm", "CrNorm", "VsnNorm"),
  scale   = c("none", "MeanCenter", "AutoNorm", "ParetoNorm", "RangeNorm"),
  pval_type = c("fdr", "raw")
)

PATHWAY_CHOICES <- list(
  id_type    = c("name", "hmdb", "kegg"),
  ora_method = c("hyperg", "fisher"),
  topology   = c("rbc", "dgr")
)

# "NULL" (MetaboAnalystR) <-> "none" (CLI)
as_cli_value <- function(x) if (identical(x, "NULL")) "none" else x
as_mar_value <- function(x) if (identical(x, "none")) "NULL" else x

common_options <- function(dpi) {
  list(
    optparse::make_option("--input", type = "character", metavar = "FICHIER",
                          help = "Fichier d'entrée (obligatoire)."),
    optparse::make_option("--out", type = "character", default = "out", metavar = "DOSSIER",
                          help = "Dossier parent des résultats [défaut : %default]."),
    optparse::make_option("--dpi", type = "integer", default = dpi, metavar = "N",
                          help = "Résolution des PNG [défaut : %default]."),
    optparse::make_option("--verbose", action = "store_true", default = FALSE,
                          help = "Afficher les messages de MetaboAnalystR et la trace R complète en cas d'erreur."),
    optparse::make_option(c("-h", "--help"), action = "store_true", default = FALSE,
                          help = "Afficher cette aide.")
  )
}

stats_parser <- function() {
  d <- stats_defaults()
  ch <- STATS_CHOICES
  opt <- function(...) optparse::make_option(...)
  choices <- function(x) paste(x, collapse = ", ")
  opts <- c(common_options(d$dpi), list(
    opt("--format", type = "character", default = d$format, metavar = "FMT",
        help = paste0("Disposition du tableau : rowu (échantillons en lignes, non appariés), ",
                      "rowp (en lignes, appariés), colu, colp (en colonnes) [défaut : %default].")),
    opt("--missing", type = "character", default = d$missing, metavar = "METHODE",
        help = paste0("Imputation des valeurs manquantes : ", choices(ch$missing),
                      " [défaut : %default = 1/5 du minimum positif de la variable].")),
    opt("--filter", type = "character", default = d$filter, metavar = "FILTRE",
        help = paste0("Filtre de variance : ", choices(ch$filter), " [défaut : %default].")),
    opt("--filter-cutoff", type = "double", default = d$filter_cutoff, metavar = "PCT",
        dest = "filter_cutoff",
        help = "Pourcentage de variables à retirer [défaut : automatique selon le nombre de variables]."),
    opt("--norm", type = "character", default = as_cli_value(d$norm), metavar = "METHODE",
        help = paste0("Normalisation par échantillon : ", choices(ch$norm), " [défaut : %default].")),
    opt("--trans", type = "character", default = as_cli_value(d$trans), metavar = "METHODE",
        help = paste0("Transformation : ", choices(ch$trans), " [défaut : %default].")),
    opt("--scale", type = "character", default = as_cli_value(d$scale), metavar = "METHODE",
        help = paste0("Mise à l'échelle : ", choices(ch$scale), " [défaut : %default].")),
    opt("--pval", type = "double", default = d$pval, metavar = "P",
        help = "Seuil de significativité (t-test / ANOVA) [défaut : %default]."),
    opt("--pval-type", type = "character", default = d$pval_type, metavar = "TYPE",
        dest = "pval_type",
        help = "p-value utilisée pour le seuil du t-test : fdr ou raw [défaut : %default]."),
    opt("--fc", type = "double", default = d$fc, metavar = "X",
        help = "Seuil de fold change (2 groupes) [défaut : %default]."),
    opt("--nonpar", action = "store_true", default = d$nonpar,
        help = "Tests non paramétriques (Wilcoxon / Kruskal-Wallis)."),
    opt("--unequal-var", action = "store_false", default = d$equal_var, dest = "equal_var",
        help = "t-test de Welch (variances inégales)."),
    opt("--missing-max", type = "double", default = d$missing_max, metavar = "F", dest = "missing_max",
        help = "Retirer les variables dont la fraction de valeurs manquantes dépasse F [défaut : %default]."),
    opt("--volcano-pval", type = "double", default = d$volcano_pval, metavar = "P", dest = "volcano_pval",
        help = "Seuil de p-value brute du volcano plot [défaut : %default]."),
    opt("--keep-intermediate", action = "store_true", default = d$keep_intermediate,
        dest = "keep_intermediate",
        help = "Conserver les fichiers intermédiaires de MetaboAnalystR (.qs).")
  ))
  optparse::OptionParser(
    usage = "metabo stats --input data.csv [--out DOSSIER] [options]",
    description = paste0(
      "Statistical Analysis [one factor] : lecture, contrôle, valeurs manquantes, filtrage,\n",
      "normalisation, t-test (2 groupes) ou ANOVA (> 2 groupes), fold change et volcano\n",
      "(2 groupes), PCA, PLS-DA et heatmap."),
    option_list = opts, add_help_option = FALSE
  )
}

pathway_parser <- function() {
  d <- pathway_defaults()
  ch <- PATHWAY_CHOICES
  opt <- function(...) optparse::make_option(...)
  opts <- c(common_options(d$dpi), list(
    opt("--id-type", type = "character", default = d$id_type, metavar = "TYPE", dest = "id_type",
        help = "Type d'identifiants : name, hmdb ou kegg [défaut : %default]."),
    opt("--organism", type = "character", default = d$organism, metavar = "CODE",
        help = "Code KEGG de l'espèce (hsa, mmu, rno, sce, eco...) [défaut : %default]."),
    opt("--ora-method", type = "character", default = d$ora_method, metavar = "TEST", dest = "ora_method",
        help = "Test de sur-représentation : hyperg ou fisher [défaut : %default]."),
    opt("--topology", type = "character", default = d$topology, metavar = "MESURE",
        help = paste0("Mesure de topologie : rbc (centralité d'intermédiarité relative) ",
                      "ou dgr (centralité de degré) [défaut : %default]."))
  ))
  optparse::OptionParser(
    usage = "metabo pathway --input compounds.txt [--out DOSSIER] [options]",
    description = paste0(
      "Pathway Analysis (métabolomique ciblée) : mapping des composés, ORA et topologie\n",
      "sur une bibliothèque KEGG. Nécessite un accès réseau (serveurs de Xia Lab)."),
    option_list = opts, add_help_option = FALSE
  )
}

global_usage <- function() {
  paste(
    "Usage : metabo <commande> [options]",
    "",
    "Commandes :",
    "  stats     Statistical Analysis [one factor]",
    "  pathway   Pathway Analysis (métabolomique ciblée)",
    "",
    "Options :",
    "  -h, --help     Afficher cette aide",
    "  --version      Afficher la version",
    "",
    "Aide d'une commande : metabo <commande> --help",
    "Codes de sortie : 0 succès, 1 erreur d'entrée, 2 erreur d'analyse ou d'environnement.",
    sep = "\n"
  )
}

# Convertit les options parsées en liste de paramètres pour run_*(), et
# valide les valeurs (erreur d'entrée -> code 1).
stats_params_from_opts <- function(o) {
  ch <- STATS_CHOICES
  validate_choice(o$format, ch$format, "--format")
  validate_choice(o$missing, ch$missing, "--missing")
  validate_choice(o$filter, ch$filter, "--filter")
  validate_choice(o$norm, ch$norm, "--norm")
  validate_choice(o$trans, ch$trans, "--trans")
  validate_choice(o$scale, ch$scale, "--scale")
  validate_choice(o$pval_type, ch$pval_type, "--pval-type")
  validate_number(o$pval, "--pval", 0, 1)
  validate_number(o$fc, "--fc", 1)
  validate_number(o$dpi, "--dpi", 36, 1200)
  if (!is.na(o$filter_cutoff)) validate_number(o$filter_cutoff, "--filter-cutoff", 0, 100)
  validate_number(o$missing_max, "--missing-max", 0, 1)
  validate_number(o$volcano_pval, "--volcano-pval", 0, 1)
  list(
    format = o$format, missing_max = o$missing_max, missing = o$missing, filter = o$filter,
    filter_cutoff = o$filter_cutoff,
    norm = as_mar_value(o$norm), trans = as_mar_value(o$trans), scale = as_mar_value(o$scale),
    pval = o$pval, pval_type = o$pval_type, fc = o$fc, volcano_pval = o$volcano_pval,
    nonpar = o$nonpar, equal_var = o$equal_var, dpi = o$dpi,
    keep_intermediate = o$keep_intermediate
  )
}

pathway_params_from_opts <- function(o) {
  ch <- PATHWAY_CHOICES
  validate_choice(o$id_type, ch$id_type, "--id-type")
  validate_choice(o$ora_method, ch$ora_method, "--ora-method")
  validate_choice(o$topology, ch$topology, "--topology")
  validate_number(o$dpi, "--dpi", 36, 1200)
  if (!grepl("^[a-z]{2,5}$", o$organism))
    input_error("Code d'espèce KEGG invalide : '", o$organism, "' (ex. hsa, mmu, rno).")
  list(id_type = o$id_type, organism = o$organism, ora_method = o$ora_method,
       topology = o$topology, dpi = o$dpi)
}

COMMANDS <- list(
  stats   = list(parser = stats_parser, params = stats_params_from_opts, run = function(...) run_stats(...)),
  pathway = list(parser = pathway_parser, params = pathway_params_from_opts, run = function(...) run_pathway(...))
)

#' Point d'entrée du CLI `metabo`
#'
#' Ne quitte jamais R : renvoie le code de sortie, que l'exécutable transmet à
#' `quit(status = )`. Cela permet de tester `main()` depuis testthat.
#'
#' @param args Arguments de la ligne de commande (sans le nom du programme).
#' @return Code de sortie (entier, invisible) : 0 succès, 1 erreur d'entrée,
#'   2 erreur d'analyse ou d'environnement.
#' @export
main <- function(args = commandArgs(trailingOnly = TRUE)) {
  out <- function(...) cat(..., "\n", sep = "")
  err <- function(...) cat(..., "\n", sep = "", file = stderr())

  if (length(args) == 0L) {
    err(global_usage())
    return(invisible(1L))
  }
  if (args[1] %in% c("-h", "--help", "help")) {
    out(global_usage())
    return(invisible(0L))
  }
  if (args[1] == "--version") {
    out("metabo ", as.character(utils::packageVersion("metabocli")))
    return(invisible(0L))
  }

  cmd <- COMMANDS[[args[1]]]
  if (is.null(cmd)) {
    err("Erreur : commande inconnue '", args[1], "'.\n")
    err(global_usage())
    return(invisible(1L))
  }

  parser <- cmd$parser()
  rest <- args[-1]
  if (any(rest %in% c("-h", "--help"))) {
    optparse::print_help(parser)
    return(invisible(0L))
  }

  verbose <- "--verbose" %in% rest
  trace <- NULL

  code <- tryCatch(
    withCallingHandlers({
      opts <- tryCatch(
        optparse::parse_args(parser, args = rest, print_help_and_exit = FALSE),
        error = function(e) input_error(sub("^Error.*?: ", "", conditionMessage(e), perl = TRUE),
                                        "\nVoir : metabo ", args[1], " --help")
      )
      if (is.null(opts$input)) input_error("--input est obligatoire. Voir : metabo ", args[1], " --help")
      params <- cmd$params(opts)
      params$verbose <- opts$verbose
      res <- cmd$run(input = opts$input, out_dir = opts$out, params = params)
      if (!is.null(res$run_dir)) out(res$run_dir)
      0L
    }, error = function(e) {
      # Capture de la pile d'appels au moment de l'erreur (pour --verbose)
      trace <<- sys.calls()
    }),
    interrupt = function(e) {
      err("Interrompu.")
      130L
    },
    error = function(e) {
      kind <- if (inherits(e, "metabo_input_error")) "Erreur d'entrée" else "Erreur d'analyse"
      err(kind, " : ", conditionMessage(e))
      if (!is.null(e$parent) && !grepl(conditionMessage(e$parent), conditionMessage(e), fixed = TRUE))
        err("  Cause : ", conditionMessage(e$parent))
      if (verbose && !is.null(trace)) {
        err("\nTrace R :")
        calls <- vapply(trace, function(cl) paste(deparse(cl, nlines = 1L), collapse = ""), "")
        err(paste0("  ", seq_along(calls), ": ", calls, collapse = "\n"))
      } else if (!inherits(e, "metabo_input_error")) {
        err("(relancez avec --verbose pour la trace complète)")
      }
      exit_code_for(e)
    }
  )
  invisible(code)
}

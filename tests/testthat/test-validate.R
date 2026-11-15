# Validation des fichiers d'entrée du module stats (sans MetaboAnalystR).

write_rows <- function(rows, sep = ",") {
  path <- tempfile(fileext = ".csv")
  writeLines(vapply(rows, paste, "", collapse = sep), path)
  path
}

valid_rows <- function() {
  c(list(c("Sample", "Group", "A", "B", "C")),
    lapply(1:6, function(i) c(paste0("S", i), if (i <= 3) "ctrl" else "case",
                              i, i * 2, if (i == 2) "" else i * 3)))
}

expect_input_error <- function(expr, pattern) {
  err <- tryCatch(expr, metabo_input_error = function(e) e)
  expect_s3_class(err, "metabo_input_error")
  expect_match(conditionMessage(err), pattern)
}

test_that("un tableau valide est accepté et décrit", {
  info <- metabocli:::validate_stats_input(write_rows(valid_rows()))
  expect_equal(info$n_samples, 6L)
  expect_equal(info$n_features, 3L)
  expect_equal(info$groups, list(case = 3L, ctrl = 3L))
  expect_equal(info$n_missing, 1L)
  expect_equal(info$separator, ",")
})

test_that("tabulations et points-virgules sont détectés", {
  expect_equal(metabocli:::validate_stats_input(write_rows(valid_rows(), "\t"))$separator, "\t")
  expect_equal(metabocli:::validate_stats_input(write_rows(valid_rows(), ";"))$separator, ";")
})

test_that("les erreurs d'entrée sont explicites", {
  expect_input_error(metabocli:::validate_stats_input("absent.csv"), "introuvable")

  r <- valid_rows(); r[[3]][3] <- "abc"
  expect_input_error(metabocli:::validate_stats_input(write_rows(r)), "non numérique.*'abc'")

  r <- valid_rows(); r[[3]][1] <- "S1"
  expect_input_error(metabocli:::validate_stats_input(write_rows(r)), "en double : S1")

  r <- valid_rows(); r[[2]][2] <- ""
  expect_input_error(metabocli:::validate_stats_input(write_rows(r)), "sans groupe")

  r <- valid_rows(); r[[7]][2] <- "ctrl"
  expect_input_error(metabocli:::validate_stats_input(write_rows(r)), "Au moins 3 échantillons")

  r <- lapply(valid_rows(), function(x) x[1:2])
  expect_input_error(metabocli:::validate_stats_input(write_rows(r)), "Au moins 3 colonnes")

  r <- valid_rows(); r[[2]][3] <- "1,5"
  expect_input_error(metabocli:::validate_stats_input(write_rows(r, ";")), "virgules décimales")
})

test_that("le format apparié exige des étiquettes de paires valides", {
  r <- valid_rows()
  for (i in 2:7) r[[i]][2] <- c("1", "2", "3", "-1", "-2", "-3")[i - 1]
  expect_equal(metabocli:::validate_stats_input(write_rows(r), "rowp")$n_samples, 6L)
  r[[7]][2] <- "-2"
  expect_input_error(metabocli:::validate_stats_input(write_rows(r), "rowp"), "paires")
})

# Validation des listes de composés du module pathway (sans réseau).

write_lines <- function(x) {
  path <- tempfile(fileext = ".txt")
  writeLines(x, path)
  path
}

test_that("une liste de composés est nettoyée", {
  info <- metabocli:::validate_pathway_input(write_lines(
    c("# commentaire", "Glycine", "", "  L-Serine  ", "\"Pyruvic acid\"", "glycine", "Creatine")))
  expect_equal(info$compounds, c("Glycine", "L-Serine", "Pyruvic acid", "Creatine"))
  expect_equal(info$n_duplicates, 1L)
})

test_that("les identifiants HMDB et KEGG sont contrôlés", {
  ids <- c("HMDB0000060", "HMDB00056", "HMDB0000064")
  expect_length(metabocli:::validate_pathway_input(write_lines(ids), "hmdb")$compounds, 3L)
  expect_input_error(metabocli:::validate_pathway_input(write_lines(c(ids, "HMDB60")), "hmdb"),
                     "HMDB invalide.*'HMDB60'")
  expect_input_error(metabocli:::validate_pathway_input(write_lines(c("C00022", "X1", "C00037")), "kegg"),
                     "KEGG invalide.*'X1'")
  expect_input_error(metabocli:::validate_pathway_input(write_lines(ids)), "--id-type hmdb")
})

test_that("les listes trop courtes ou tabulaires sont refusées", {
  expect_input_error(metabocli:::validate_pathway_input(write_lines(c("Glycine", "Serine"))),
                     "Au moins 3 composés")
  expect_input_error(metabocli:::validate_pathway_input(write_lines(
    c("Sample,Group,A,B", "S1,a,1,2", "S2,a,3,4", "S3,b,5,6"))), "ressemble à un tableau")
})

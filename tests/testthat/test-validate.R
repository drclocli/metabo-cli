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

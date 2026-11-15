# Tests de la couche CLI : main() renvoie un code de sortie sans quitter R.

# Appelle main() en capturant stdout et stderr.
run_main <- function(...) {
  code <- NULL
  stderr_lines <- character()
  stdout_lines <- capture.output(
    stderr_lines <- capture.output(code <- main(c(...)), type = "message")
  )
  list(code = code, stdout = paste(stdout_lines, collapse = "\n"),
       stderr = paste(stderr_lines, collapse = "\n"))
}

test_that("--help et --version renvoient 0", {
  expect_equal(run_main("--help")$code, 0L)
  expect_equal(run_main("stats", "--help")$code, 0L)
  expect_equal(run_main("pathway", "--help")$code, 0L)
  expect_match(run_main("--version")$stdout, "^metabo \\d")
})

test_that("les erreurs d'entrée renvoient 1", {
  expect_equal(run_main()$code, 1L)
  expect_equal(run_main("inconnue")$code, 1L)
  expect_equal(run_main("stats")$code, 1L)
  expect_equal(run_main("stats", "--input", "absent.csv")$code, 1L)
  expect_equal(run_main("stats", "--input", "x.csv", "--option-inconnue")$code, 1L)
  expect_equal(run_main("stats", "--input", "x.csv", "--norm", "Bogus")$code, 1L)
  expect_equal(run_main("stats", "--input", "x.csv", "--pval", "2")$code, 1L)
  expect_equal(run_main("pathway", "--input", "x.txt", "--organism", "HSA")$code, 1L)
})

test_that("les valeurs par défaut du CLI sont celles de stats_defaults()", {
  o <- optparse::parse_args(metabocli:::stats_parser(), args = character())
  p <- metabocli:::stats_params_from_opts(o)
  d <- stats_defaults()
  expect_equal(p, d[names(p)])
})

test_that("les valeurs par défaut du CLI sont celles de pathway_defaults()", {
  o <- optparse::parse_args(metabocli:::pathway_parser(), args = character())
  p <- metabocli:::pathway_params_from_opts(o)
  d <- pathway_defaults()
  expect_equal(p, d[names(p)])
})

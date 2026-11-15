# Module stats sur le jeu d'exemple officiel human_cachexia.csv (2 groupes).
# Exécute réellement MetaboAnalystR (~30 s), sans accès réseau.

test_that("run_stats() produit les fichiers et le résumé attendus", {
  skip_on_cran()
  out <- withr::local_tempdir()
  owd <- getwd()

  res <- run_stats(example_file("human_cachexia.csv"), out)

  # le répertoire courant est restauré
  expect_identical(getwd(), owd)

  # dossier horodaté
  expect_match(basename(res$run_dir), "^stats_\\d{8}-\\d{6}$")
  expect_true(file.exists(file.path(res$run_dir, "summary.json")))

  s <- read_summary(res$run_dir)
  expect_equal(s$module, "stats")
  expect_equal(s$status, "success")
  expect_equal(s$input$n_samples, 77)
  expect_equal(s$input$groups, list(cachexic = 47, control = 30))

  # paramètres par défaut du site
  expect_equal(s$parameters$norm, "NULL")
  expect_equal(s$parameters$pval, 0.05)

  # résultats clés : t-test sur 2 groupes, top 10 trié par p-value
  r <- s$results
  expect_equal(r$test, "t-test")
  expect_equal(r$n_groups, 2)
  expect_equal(r$n_features_analysed, 59)   # 63 - 5 % (filtre IQR automatique)
  expect_equal(r$n_significant, 44)
  expect_length(r$top_features, 10)
  expect_equal(r$top_features[[1]]$feature, "Quinolinate")
  p <- vapply(r$top_features, function(x) x$p_value, 0)
  expect_false(is.unsorted(p))

  # fichiers produits : chemins relatifs, présents, sans intermédiaires
  files <- unlist(s$files)
  expect_true(all(c("t_test.csv", "fold_change.csv", "volcano.csv", "plsda_vip.csv",
                    "volcano_0_dpi72.png", "pca_score2d_0_dpi72.png",
                    "pls_score2d_0_dpi72.png", "heatmap_0_dpi72.png") %in% files))
  expect_true(all(file.exists(file.path(res$run_dir, files))))
  expect_length(list.files(res$run_dir, pattern = "\\.qs$"), 0L)
})

test_that("une erreur d'analyse renvoie le code 2 et un summary.json en erreur", {
  skip_on_cran()
  out <- withr::local_tempdir()
  input <- withr::local_tempfile(fileext = ".csv")
  # une seule variable non constante : il n'en reste aucune après filtrage
  writeLines(c("S,G,A,B", "1,a,1,5", "2,a,2,5", "3,a,3,5", "4,b,4,5", "5,b,5,5", "6,b,6,5"), input)

  code <- NULL
  capture.output(code <- main(c("stats", "--input", input, "--out", out)), type = "message")
  expect_equal(code, 2L)

  s <- read_summary(list.dirs(out, recursive = FALSE)[1])
  expect_equal(s$status, "error")
  expect_equal(s$error$type, "analysis")
  expect_match(s$error$message, "au moins 2")
})

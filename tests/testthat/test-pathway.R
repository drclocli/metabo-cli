# Module pathway sur la liste de composés de la vignette officielle.
# Nécessite un accès aux serveurs de Xia Lab (~15-25 s) : ignoré hors ligne.

test_that("run_pathway() produit les tables, le graphique et le résumé attendus", {
  skip_on_cran()
  skip_if_not(xialab_online(), "serveurs de Xia Lab injoignables")
  out <- withr::local_tempdir()
  owd <- getwd()

  res <- run_pathway(example_file("pathway_compounds.txt"), out)
  expect_identical(getwd(), owd)
  expect_match(basename(res$run_dir), "^pathway_\\d{8}-\\d{6}$")

  s <- read_summary(res$run_dir)
  expect_equal(s$module, "pathway")
  expect_equal(s$status, "success")
  expect_equal(s$parameters$organism, "hsa")

  # correspondance : 17/18 reconnus, le nom mal orthographié est signalé
  r <- s$results
  expect_equal(r$mapping$n_input, 18)
  expect_equal(r$mapping$n_matched, 17)
  expect_equal(unlist(r$mapping$unmatched), "L-Isolucine")

  # voies : top 10 avec p-value et impact, la voie attendue en tête
  expect_gt(r$n_pathways, 20)
  expect_length(r$top_pathways, 10)
  top <- r$top_pathways[[1]]
  expect_equal(top$pathway, "Glycine, serine and threonine metabolism")
  expect_equal(top$kegg_id, "hsa00260")
  expect_lt(top$p_value, 1e-6)
  expect_true(top$impact > 0 && top$impact <= 1)

  # fichiers
  expect_true(all(c("pathway_results.csv", "compound_mapping.csv", "path_view_0_dpi72.png") %in%
                    unlist(s$files)))
  mapping <- utils::read.csv(file.path(res$run_dir, "compound_mapping.csv"))
  expect_equal(nrow(mapping), 18)
  unmatched <- mapping[mapping$query == "L-Isolucine", ]
  expect_equal(unmatched$status, "unmatched")
  expect_match(unmatched$suggestions, "Isoleucine")
})

test_that("une espèce inconnue est une erreur d'entrée (code 1)", {
  skip_on_cran()
  skip_if_not(xialab_online(), "serveurs de Xia Lab injoignables")
  code <- NULL
  capture.output(code <- main(c("pathway", "--input", example_file("pathway_compounds.txt"),
                                "--organism", "zzz", "--out", withr::local_tempdir())),
                 type = "message")
  expect_equal(code, 1L)
})

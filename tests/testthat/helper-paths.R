# Chemin d'un jeu d'exemple du dossier examples/ à la racine du repo.
example_file <- function(name) {
  path <- testthat::test_path("..", "..", "examples", name)
  if (!file.exists(path)) testthat::skip(paste("jeu d'exemple absent :", name))
  normalizePath(path)
}

read_summary <- function(run_dir) jsonlite::read_json(file.path(run_dir, "summary.json"))

# Le serveur de Xia Lab est-il joignable ? (le module pathway en dépend)
xialab_online <- function() {
  isTRUE(tryCatch({
    h <- curl::new_handle(nobody = TRUE, connecttimeout = 10, timeout = 10)
    curl::curl_fetch_memory("https://www.xialab.ca/api", handle = h)
    TRUE
  }, error = function(e) FALSE))
}

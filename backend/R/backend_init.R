load_backend_packages <- function() {
  pkgs <- c(
    "dplyr", "tibble", "tidyr", "stringr", "purrr", "readr",
    "glue", "psych", "posterior", "brms", "loo", "RColorBrewer",
    "rlang", "future", "uuid", "jsonlite", "R6", "plumber"
  )
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(sprintf("Missing required package(s): %s", paste(missing, collapse = ", ")))
  }

  if (requireNamespace("rstan", quietly = TRUE)) {
    rstan::rstan_options(auto_write = TRUE)
    options(mc.cores = max(1L, parallel::detectCores()))
  }
}

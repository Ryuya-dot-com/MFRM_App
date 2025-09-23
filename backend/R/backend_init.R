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

  brms_version <- utils::packageVersion("brms")
  required_brms <- "2.20.0"
  if (brms_version < required_brms) {
    stop(sprintf(
      "brms >= %s is required. Detected version %s. Please update the package.",
      required_brms,
      brms_version
    ))
  }

  if (requireNamespace("rstan", quietly = TRUE)) {
    rstan::rstan_options(auto_write = TRUE)
    options(mc.cores = max(1L, parallel::detectCores()))
  }
}

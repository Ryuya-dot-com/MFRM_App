options(repos = c(CRAN = Sys.getenv('CRAN_REPO', 'https://packagemanager.posit.co/cran/latest')))

base_packages <- c(
  'plumber', 'brms', 'rstan', 'StanHeaders', 'posterior', 'loo', 'future',
  'uuid', 'jsonlite', 'dplyr', 'tibble', 'tidyr', 'stringr', 'purrr', 'readr',
  'glue', 'psych', 'R6', 'RColorBrewer'
)

installed <- rownames(installed.packages())
needed <- setdiff(base_packages, installed)
if (length(needed) > 0) {
  install.packages(needed, dependencies = c('Depends', 'Imports', 'LinkingTo'))
}

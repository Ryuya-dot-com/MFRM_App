#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep('^--file=', args, value = TRUE)
if (length(script_arg) == 1) {
  script_path <- normalizePath(sub('^--file=', '', script_arg), winslash = '/', mustWork = TRUE)
  project_root <- normalizePath(file.path(dirname(script_path), '..'), winslash = '/', mustWork = TRUE)
  setwd(project_root)
}
library(plumber)
pr <- plumber::plumb('backend/plumber.R')
pr$run(host = Sys.getenv('HOST', '0.0.0.0'), port = as.integer(Sys.getenv('PORT', 8000)))

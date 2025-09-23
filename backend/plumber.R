# plumber.R -- REST API exposing the MFRM analysis backend

resolve_paths <- function() {
  frame <- sys.frames()[[1]]
  file_path <- tryCatch(frame$ofile, error = function(e) NULL)
  candidates <- c(file_path, 'plumber.R', 'backend/plumber.R')
  candidates <- unique(candidates[!is.null(candidates)])
  located <- NULL
  for (cand in candidates) {
    if (file.exists(cand)) {
      located <- cand
      break
    }
    abs_candidate <- file.path(getwd(), cand)
    if (file.exists(abs_candidate)) {
      located <- abs_candidate
      break
    }
  }
  if (is.null(located)) {
    stop('Unable to locate plumber.R for sourcing')
  }
  file_path <- normalizePath(located, winslash = '/', mustWork = TRUE)
  plumber_dir <- dirname(file_path)
  list(
    plumber_dir = plumber_dir,
    plumber_file = file_path
  )
}


paths <- resolve_paths()
root_dir <- dirname(paths$plumber_dir)

source(file.path(root_dir, 'backend/R/backend_init.R'))
load_backend_packages()

r_dir <- file.path(root_dir, 'backend/R')
r_files <- setdiff(list.files(r_dir, pattern = "\\.R$", full.names = TRUE),
                   file.path(r_dir, 'backend_init.R'))
purrr::walk(r_files, source)
source(file.path(root_dir, 'backend/jobs/job_manager.R'))

job_manager <- JobManager$new()

#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", req$HTTP_ORIGIN %||% "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  if (!is.null(req$HTTP_ACCESS_CONTROL_REQUEST_PRIVATE_NETWORK) &&
      tolower(req$HTTP_ACCESS_CONTROL_REQUEST_PRIVATE_NETWORK) == "true") {
    res$setHeader("Access-Control-Allow-Private-Network", "true")
  }
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    res$setHeader("Content-Length", "0")
    return(list())
  }
  plumber::forward()
}

#* Health check
#* @get /health
function() {
  list(
    status = "ok",
    timestamp = as.character(Sys.time())
  )
}

#* Return template metadata for the client-side data entry form
#* @get /template
function() {
  list(
    columns = get_template_columns(),
    tab_text = get_template_tab_text(),
    header_text = get_template_header_text()
  )
}

#* Download a sample dataset (CSV or TSV)
#* @serializer contentType list(type = "text/plain; charset=utf-8")
#* @get /template/sample
function(format = c("csv", "tsv")) {
  format <- match.arg(format)
  samples <- get_sample_downloads()
  if (identical(format, "csv")) {
    return(samples$csv)
  }
  samples$tsv
}

#* Submit an analysis job
#* @post /jobs
function(req) {
  payload <- jsonlite::fromJSON(req$postBody, simplifyVector = FALSE)
  model_config <- payload$model_config %||% list()
  sampler_config <- payload$sampler_config %||% list()
  result <- job_manager$submit_job(payload, model_config, sampler_config)
  list(job_id = result$job_id, status = "queued")
}

#* List job identifiers
#* @get /jobs
function() {
  job_manager$list_jobs()
}

#* Fetch job status
#* @get /jobs/<job_id>
function(job_id) {
  job <- job_manager$get_job(job_id)
  if (is.null(job)) {
    plumber::abort(404, sprintf("Job %s not found", job_id))
  }
  job
}

#* Fetch job result payload
#* @get /jobs/<job_id>/result
function(job_id) {
  job <- job_manager$get_job(job_id)
  if (is.null(job)) {
    plumber::abort(404, sprintf("Job %s not found", job_id))
  }
  if (!identical(job$status, "finished")) {
    plumber::abort(409, sprintf("Job %s not finished (status = %s)", job_id, job$status))
  }
  result <- job_manager$get_result(job_id)
  if (is.null(result)) {
    plumber::abort(500, sprintf("Job %s completed but no result payload is available", job_id))
  }
  result
}

#* Download derived CSV outputs
#* @serializer contentType list(type = 'text/csv; charset=utf-8')
#* @get /jobs/<job_id>/download/<resource>
function(job_id, resource = c('summary', 'reliability', 'fit', 'thresholds', 'facets')) {
  resource <- match.arg(resource)
  job <- job_manager$get_job(job_id)
  if (is.null(job)) {
    plumber::abort(404, sprintf('Job %s not found', job_id))
  }
  if (!identical(job$status, 'finished')) {
    plumber::abort(409, sprintf('Job %s not finished (status = %s)', job_id, job$status))
  }
  result <- job_manager$get_result(job_id)
  if (is.null(result)) {
    plumber::abort(500, sprintf('Job %s completed but no result payload is available', job_id))
  }
  df <- build_download_table(result, resource)
  readr::format_csv(df)
}

#* Standard error handler returning JSON
#* @plumber
function(pr) {
  pr |>
    plumber::pr_set_serializer(plumber::serializer_unboxed_json()) |>
    plumber::pr_set_debug(FALSE)
}

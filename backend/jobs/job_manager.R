JobManager <- R6::R6Class(
  "JobManager",
  private = list(
    storage_path = NULL,
    registry = NULL,
    futures = NULL,
    fns = NULL,
    modules_env = NULL,
    repo_root = NULL,
    modules_dir = NULL,
    module_files = NULL,
    workers = NULL,

    ensure_storage = function() {
      if (!dir.exists(private$storage_path)) {
        dir.create(private$storage_path, recursive = TRUE, showWarnings = FALSE)
      }
    },

    job_dir = function(job_id) {
      file.path(private$storage_path, job_id)
    },

    write_json_file = function(path, data) {
      jsonlite::write_json(
        data,
        path,
        pretty = TRUE,
        auto_unbox = TRUE,
        na = "null",
        dataframe = "rows"
      )
    },

    resolve_workers = function() {
      env_value <- Sys.getenv("MFRM_FUTURE_WORKERS", "")
      requested <- suppressWarnings(as.integer(env_value))

      read_cgroup_limit <- function() {
        parse_limit <- function(quota, period) {
          quota <- suppressWarnings(as.numeric(quota))
          period <- suppressWarnings(as.numeric(period))
          if (is.na(quota) || is.na(period) || quota <= 0 || period <= 0) {
            return(NA_integer_)
          }
          floor(quota / period)
        }

        cpu_max_path <- "/sys/fs/cgroup/cpu.max"
        if (file.exists(cpu_max_path)) {
          parts <- strsplit(readLines(cpu_max_path, warn = FALSE), "\\s+")[[1]]
          if (length(parts) >= 2 && !identical(parts[1], "max")) {
            limit <- parse_limit(parts[1], parts[2])
            if (!is.na(limit)) {
              return(max(1L, limit))
            }
          }
        }

        quota_path <- "/sys/fs/cgroup/cpu/cpu.cfs_quota_us"
        period_path <- "/sys/fs/cgroup/cpu/cpu.cfs_period_us"
        if (file.exists(quota_path) && file.exists(period_path)) {
          limit <- parse_limit(
            readLines(quota_path, warn = FALSE),
            readLines(period_path, warn = FALSE)
          )
          if (!is.na(limit)) {
            return(max(1L, limit))
          }
        }

        NA_integer_
      }

      future_limits <- c(
        suppressWarnings(future::availableCores(which = "all")),
        suppressWarnings(future::availableCores(constraints = "cgroups", which = "all"))
      )
      future_limits <- suppressWarnings(as.integer(future_limits))
      future_limits <- future_limits[is.finite(future_limits) & !is.na(future_limits) & future_limits > 0]

      detected_all <- suppressWarnings(parallel::detectCores())
      detect_limit <- detected_all[is.finite(detected_all) & !is.na(detected_all) & detected_all > 0]

      cgroup_limit <- read_cgroup_limit()

      candidates <- c(future_limits, detect_limit, cgroup_limit)
      candidates <- candidates[is.finite(candidates) & !is.na(candidates) & candidates > 0]
      hard_limit <- if (length(candidates) > 0) min(candidates) else 1L
      hard_limit <- max(1L, as.integer(hard_limit))

      if (!is.na(requested) && requested > 0) {
        return(max(1L, min(requested, hard_limit)))
      }

      default_target <- detected_all - 1L
      if (length(default_target) == 0 || is.na(default_target) || !is.finite(default_target) || default_target < 1L) {
        default_target <- hard_limit
      }

      max(1L, min(default_target, hard_limit))
    },

    log = function(fmt, ...) {
      message(sprintf("[JobManager] %s", sprintf(fmt, ...)))
    },

    load_modules_env = function() {
      env <- new.env(parent = globalenv())
      for (path in private$module_files) {
        sys.source(path, envir = env)
      }
      private$modules_env <- env
    },

    run_job = function(job_id, payload, model_config, sampler_config) {
      private$load_modules_env()
      private$ensure_storage()
      dir.create(private$job_dir(job_id), showWarnings = FALSE, recursive = TRUE)

      input_path <- file.path(private$job_dir(job_id), "input.json")
      private$write_json_file(input_path, payload)

      data <- payload$data
      if (is.null(data)) {
        stop("Payload missing 'data' field")
      }

      df <- tryCatch(
        tibble::as_tibble(data),
        error = function(e) {
          if (is.list(data) && all(vapply(data, is.list, logical(1)))) {
            dplyr::bind_rows(data)
          } else {
            stop(e)
          }
        }
      )

      config <- payload$config %||% list()
      if (!is.null(config$facet_cols)) {
        config$facet_cols <- unlist(config$facet_cols, use.names = FALSE)
      }
      prepared <- private$modules_env$prepare_analysis_data(df, config)
      private$log(
        "Job %s prepared data (%s rows, %s persons, %s facets).",
        job_id,
        prepared$n_obs %||% NA_integer_,
        prepared$n_persons %||% NA_integer_,
        length(prepared$facet_names)
      )
      analysis <- private$modules_env$run_mfrm_analysis(prepared, model_config, sampler_config)
      payload_out <- private$modules_env$assemble_analysis_payload(prepared, analysis, model_config)

      # Remove large objects from payload before persisting
      payload_out$model <- NULL
      payload_out$analysis_data <- NULL

      result_path <- file.path(private$job_dir(job_id), "result.json")
      private$write_json_file(result_path, payload_out)
      saveRDS(payload_out, file.path(private$job_dir(job_id), "result.rds"))
      private$log("Job %s completed successfully.", job_id)

      list(
        status = "finished",
        completed_at = Sys.time(),
        result = payload_out
      )
    },

    update_status = function(job_id, status, error = NULL, result = NULL,
                             started_at = NULL, completed_at = NULL) {
      info <- private$registry[[job_id]]
      if (is.null(info)) {
        info <- list()
      }
      info$status <- status
      info$updated_at <- Sys.time()
      if (!is.null(started_at)) {
        info$started_at <- started_at
      }
      if (!is.null(completed_at)) {
        info$completed_at <- completed_at
      }
      if (!is.null(error)) {
        info$error <- error
      }
      if (!is.null(result)) {
        info$result <- result
      }
      private$registry[[job_id]] <- info
    },

    collect_future = function(job_id) {
      fut <- private$futures[[job_id]]
      if (is.null(fut)) {
        return(invisible(NULL))
      }

      if (!future::resolved(fut)) {
        return(invisible(NULL))
      }

      outcome <- tryCatch(
        future::value(fut),
        error = function(e) {
          err_msg <- conditionMessage(e)
          err_call <- deparse(conditionCall(e))
          private$log("Job %s failed: %s", job_id, err_msg)
          list(
            status = "failed",
            error = list(
              message = err_msg,
              call = err_call
            )
          )
        }
      )

      status <- outcome$status %||% "finished"
      completed_at <- outcome$completed_at %||% Sys.time()
      error <- outcome$error %||% NULL
      result <- outcome$result %||% NULL

      private$update_status(
        job_id,
        status,
        error = error,
        result = result,
        completed_at = completed_at
      )

      if (!is.null(error)) {
        private$write_json_file(
          file.path(private$job_dir(job_id), "error.json"),
          list(error = error)
        )
      }

      private$futures[[job_id]] <- NULL
      invisible(NULL)
    }
  ),
  public = list(
    initialize = function(storage_path = file.path("backend", "jobs", "runtime"),
                          repo_root = NULL) {
      load_backend_packages()

      normalize_abs <- function(path, must_work = TRUE) {
        normalizePath(path, winslash = "/", mustWork = must_work)
      }

      locate_repo_root <- function(candidate) {
        repo_candidate <- tryCatch(normalize_abs(candidate), error = function(e) NULL)
        if (is.null(repo_candidate)) {
          return(NULL)
        }

        modules_candidate <- file.path(repo_candidate, "backend", "R")
        if (dir.exists(modules_candidate)) {
          return(list(root = repo_candidate, modules = modules_candidate))
        }

        parent_root <- dirname(repo_candidate)
        parent_modules <- file.path(parent_root, "backend", "R")
        if (dir.exists(parent_modules)) {
          return(list(root = parent_root, modules = parent_modules))
        }
        NULL
      }

      initial_root <- if (is.null(repo_root)) getwd() else repo_root
      location <- locate_repo_root(initial_root)
      if (is.null(location)) {
        stop(sprintf("Unable to locate backend module directory relative to '%s'.", initial_root))
      }

      private$repo_root <- location$root
      private$modules_dir <- location$modules

      is_absolute_path <- function(path) {
        grepl("^([A-Za-z]:)?[\\/]", path)
      }

      storage_base <- if (is_absolute_path(storage_path)) {
        storage_path
      } else {
        file.path(private$repo_root, storage_path)
      }

      private$storage_path <- normalize_abs(storage_base, must_work = FALSE)
      private$registry <- new.env(parent = emptyenv())
      private$futures <- new.env(parent = emptyenv())
      private$ensure_storage()
      private$workers <- private$resolve_workers()
      private$log("Configuring background plan with %s worker(s).", private$workers)
      if (private$workers > 1) {
        future::plan(future::multisession, workers = private$workers)
      } else {
        future::plan(future::sequential)
      }
      private$module_files <- list.files(private$modules_dir, pattern = "\\.R$", full.names = TRUE)
      private$load_modules_env()
    },

    submit_job = function(payload, model_config = list(), sampler_config = list()) {
      job_id <- uuid::UUIDgenerate()
      private$ensure_storage()
      dir.create(private$job_dir(job_id), recursive = TRUE, showWarnings = FALSE)

      private$registry[[job_id]] <- list(
        status = "queued",
        submitted_at = Sys.time(),
        updated_at = Sys.time()
      )

      private$log("Job %s queued.", job_id)
      fut <- tryCatch(
        future::future({
          private$log("Job %s started.", job_id)
          tryCatch(
            private$run_job(job_id, payload, model_config, sampler_config),
            error = function(e) {
              err <- list(
                message = conditionMessage(e),
                call = deparse(conditionCall(e))
              )
              private$log("Job %s error: %s", job_id, err$message)
              private$write_json_file(
                file.path(private$job_dir(job_id), "error.json"),
                list(error = err)
              )
              list(
                status = "failed",
                error = err,
                completed_at = Sys.time()
              )
            }
          )
        }, seed = TRUE),
        error = function(e) {
          err <- list(message = conditionMessage(e), call = deparse(conditionCall(e)))
          private$update_status(job_id, "failed", error = err, completed_at = Sys.time())
          private$write_json_file(
            file.path(private$job_dir(job_id), "error.json"),
            list(error = err)
          )
          NULL
        }
      )

      if (!is.null(fut)) {
        private$futures[[job_id]] <- fut
        private$update_status(job_id, "running", started_at = Sys.time())
      }
      list(job_id = job_id)
    },

    get_job = function(job_id) {
      private$collect_future(job_id)
      info <- private$registry[[job_id]]
      if (is.null(info)) {
        return(NULL)
      }
      if (identical(info$status, "finished") && is.null(info$result)) {
        result_path <- file.path(private$job_dir(job_id), "result.json")
        if (file.exists(result_path)) {
          info$result <- jsonlite::read_json(result_path, simplifyVector = TRUE)
        }
      }
      info
    },

    get_result = function(job_id) {
      private$collect_future(job_id)
      result_path <- file.path(private$job_dir(job_id), "result.json")
      if (!file.exists(result_path)) {
        return(NULL)
      }
      jsonlite::read_json(result_path, simplifyVector = TRUE)
    },

    list_jobs = function() {
      ls(envir = private$registry)
    }
  )
)

# Core MFRM analysis pipeline. This function mirrors the behaviour of the
# original Shiny reactive but adapts it for a headless API workflow.

run_mfrm_analysis <- function(prepared, model_config = list(), sampler_config = list()) {
  if (is.null(prepared) || is.null(prepared$data)) {
    stop("Prepared data is required.")
  }

  df <- prepared$data
  active_facets <- prepared$facet_names %||% character()

  link_function <- model_config$link_function %||% "logit"
  threshold <- model_config$threshold %||% "flexible"

  chains <- sampler_config$chains %||% 2
  iter <- sampler_config$iter %||% 2000
  warmup <- sampler_config$warmup %||% floor(iter / 2)
  cores <- sampler_config$cores %||% min(chains, parallel::detectCores())
  seed <- sampler_config$seed %||% 1234
  adapt_delta <- sampler_config$adapt_delta %||% 0.95
  max_treedepth <- sampler_config$max_treedepth %||% 12

  backend <- sampler_config$backend %||% Sys.getenv("BRMS_BACKEND", "rstan")

  random_effects_terms <- paste0("(1|", c("Person", active_facets), ")")
  formula_str <- paste("Response ~ 1 +", paste(random_effects_terms, collapse = " + "))

  priors <- c(
    brms::set_prior("normal(0, 2)", class = "sd", lb = 0),
    brms::set_prior("normal(0, 2)", class = "Intercept")
  )

  fit <- tryCatch({
    brms::brm(
      formula = stats::as.formula(formula_str),
      data = df,
      family = brms::cumulative(link = link_function, threshold = threshold),
      prior = priors,
      chains = chains,
      iter = iter,
      warmup = warmup,
      cores = cores,
      seed = seed,
      control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
      refresh = 0,
      backend = backend
    )
  }, error = function(e) {
    stop(glue::glue("Model fitting failed: {e$message}"))
  })

  draws <- posterior::as_draws_df(fit) |> to_plain_draws()
  if (is.null(draws) || ncol(draws) == 0) {
    stop("Unable to extract posterior draws from fitted model.")
  }

  ranef_cols <- grep("^r_", names(draws), value = TRUE)
  ranef_long <- if (length(ranef_cols) > 0) {
    draws |>
      dplyr::select(.data$.draw, dplyr::all_of(ranef_cols)) |>
      tidyr::pivot_longer(-.data$.draw, names_to = "parameter", values_to = "value") |>
      tidyr::extract(
        parameter,
        into = c("Facet", "Level", "Term"),
        regex = "^r_(.+)\\[([^,]+),([^\\]]+)\\]$",
        remove = TRUE
      ) |>
      dplyr::mutate(
        .draw = as.integer(.data$.draw),
        Facet = as.character(.data$Facet),
        Level = as.character(.data$Level),
        Term = as.character(.data$Term)
      ) |>
      dplyr::filter(.data$Term == "Intercept") |>
      dplyr::mutate(
        Effect = .data$value,
        Display = dplyr::if_else(.data$Facet == "Person", .data$Effect, -.data$Effect),
        Parameter = dplyr::if_else(.data$Facet == "Person", "Ability", "Difficulty")
      ) |>
      dplyr::select(.data$.draw, .data$Facet, .data$Level, .data$Parameter, .data$Effect, .data$Display)
  } else {
    tibble::tibble(.draw = integer(), Facet = character(), Level = character(),
                   Parameter = character(), Effect = numeric(), Display = numeric())
  }

  ranef_summary <- ranef_long |>
    dplyr::group_by(.data$Facet, .data$Level, .data$Parameter) |>
    dplyr::summarise(
      Mean = mean(.data$Display, na.rm = TRUE),
      Median = stats::median(.data$Display, na.rm = TRUE),
      SD = stats::sd(.data$Display, na.rm = TRUE),
      Lower66 = safe_quantile(.data$Display, 0.17),
      Upper66 = safe_quantile(.data$Display, 0.83),
      Lower95 = safe_quantile(.data$Display, 0.025),
      Upper95 = safe_quantile(.data$Display, 0.975),
      .groups = "drop"
    )

  threshold_cols <- grep("^b_Intercept\\[", names(draws), value = TRUE)
  threshold_draws <- if (length(threshold_cols) > 0) {
    draws |>
      dplyr::select(.data$.draw, dplyr::all_of(threshold_cols)) |>
      tidyr::pivot_longer(-.data$.draw, names_to = "Threshold", values_to = "Value") |>
      tidyr::extract(
        Threshold,
        into = "Threshold",
        regex = "^b_Intercept\\[(.+)\\]$",
        remove = TRUE
      ) |>
      dplyr::mutate(
        .draw = as.integer(.data$.draw),
        Threshold = as.character(.data$Threshold)
      )
  } else {
    tibble::tibble(.draw = integer(), Threshold = character(), Value = numeric())
  }

  threshold_summary <- threshold_draws |>
    dplyr::group_by(.data$Threshold) |>
    dplyr::summarise(
      Mean = mean(.data$Value, na.rm = TRUE),
      Median = stats::median(.data$Value, na.rm = TRUE),
      SD = stats::sd(.data$Value, na.rm = TRUE),
      Lower66 = safe_quantile(.data$Value, 0.17),
      Upper66 = safe_quantile(.data$Value, 0.83),
      Lower95 = safe_quantile(.data$Value, 0.025),
      Upper95 = safe_quantile(.data$Value, 0.975),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      ThresholdOrder = suppressWarnings(as.numeric(.data$Threshold))
    ) |>
    dplyr::arrange(.data$ThresholdOrder) |>
    dplyr::select(-.data$ThresholdOrder)

  available_draws <- posterior::ndraws(fit)
  nsamples <- min(400, available_draws)
  prob_array <- fitted(fit, summary = FALSE, ndraws = nsamples)

  prob_df <- as.data.frame.table(prob_array, responseName = "prob") |>
    dplyr::mutate(
      .draw = as.integer(.data$Var1),
      .obs = as.integer(.data$Var2),
      Category = as.integer(.data$Var3)
    ) |>
    dplyr::select(.data$.draw, .data$.obs, .data$Category, .data$prob)

  prob_mean <- prob_df |>
    dplyr::group_by(.data$.obs, .data$Category) |>
    dplyr::summarise(prob = mean(.data$prob, na.rm = TRUE), .groups = "drop")

  expected_summary <- prob_mean |>
    dplyr::group_by(.data$.obs) |>
    dplyr::summarise(
      expected = sum(.data$Category * .data$prob),
      second_moment = sum((.data$Category^2) * .data$prob),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      variance = pmax(second_moment - expected^2, 1e-6)
    ) |>
    dplyr::select(.data$.obs, expected, variance)

  observed_numeric <- as.numeric(df$Response)

  df_aug <- df |>
    dplyr::mutate(
      .obs = dplyr::row_number(),
      Person = as.character(.data$Person),
      dplyr::across(dplyr::all_of(active_facets), as.character),
      observed = observed_numeric
    ) |>
    dplyr::left_join(expected_summary, by = ".obs") |>
    dplyr::mutate(
      residual = observed - expected,
      std_residual = residual / sqrt(variance)
    )

  fit_base <- df_aug

  person_fit <- fit_base |>
    dplyr::group_by(.data$Person) |>
    dplyr::group_modify(~ summarize_fit_block(.x)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Facet = "Person", Level = .data$Person) |>
    dplyr::select(.data$Facet, .data$Level, dplyr::everything(), -.data$Person)

  facet_fit <- if (length(active_facets) == 0) {
    person_fit[0, ]
  } else {
    purrr::map_dfr(active_facets, function(facet) {
      fit_base |>
        dplyr::group_by(.data[[facet]]) |>
        dplyr::group_modify(~ summarize_fit_block(.x)) |>
        dplyr::ungroup() |>
        dplyr::mutate(Facet = facet, Level = as.character(.data[[facet]])) |>
        dplyr::select(.data$Facet, .data$Level, dplyr::everything(), -dplyr::all_of(facet))
    })
  }

  fit_summary <- dplyr::bind_rows(person_fit, facet_fit)

  if (nrow(ranef_long) > 0) {
    ranef_summary_means <- ranef_summary |>
      dplyr::select(.data$Facet, .data$Level, Mean)

    rmse_draws <- ranef_long |>
      dplyr::left_join(ranef_summary_means, by = c("Facet", "Level")) |>
      dplyr::mutate(Error = .data$Display - .data$Mean) |>
      dplyr::group_by(.data$Facet, .data$.draw) |>
      dplyr::summarise(RMSE = sqrt(mean(.data$Error^2, na.rm = TRUE)), .groups = "drop")

    var_draws <- ranef_long |>
      dplyr::group_by(.data$Facet, .data$.draw) |>
      dplyr::summarise(VarLevel = stats::var(.data$Display, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(VarLevel = dplyr::if_else(is.na(.data$VarLevel), 0, .data$VarLevel))

    reliability_draws <- var_draws |>
      dplyr::left_join(rmse_draws, by = c("Facet", ".draw")) |>
      dplyr::mutate(
        RMSE = dplyr::if_else(is.na(.data$RMSE), 0, .data$RMSE),
        VarLevel = pmax(.data$VarLevel, 0),
        AdjVar = pmax(.data$VarLevel - .data$RMSE^2, 0),
        ObsSD = sqrt(.data$VarLevel),
        AdjSD = sqrt(.data$AdjVar),
        Reliability = dplyr::if_else(.data$VarLevel > 0, .data$AdjVar / .data$VarLevel, 0),
        Separation = dplyr::if_else(.data$RMSE > 0, .data$AdjSD / .data$RMSE, 0),
        Strata = (4 * .data$Separation + 1) / 3
      )

    reliability_summary <- reliability_draws |>
      dplyr::group_by(.data$Facet) |>
      dplyr::summarise(
        ObsSD_Mean = mean(.data$ObsSD, na.rm = TRUE),
        ObsSD_Lower66 = safe_quantile(.data$ObsSD, 0.17),
        ObsSD_Upper66 = safe_quantile(.data$ObsSD, 0.83),
        ObsSD_Lower95 = safe_quantile(.data$ObsSD, 0.025),
        ObsSD_Upper95 = safe_quantile(.data$ObsSD, 0.975),
        RMSE_Mean = mean(.data$RMSE, na.rm = TRUE),
        RMSE_Lower66 = safe_quantile(.data$RMSE, 0.17),
        RMSE_Upper66 = safe_quantile(.data$RMSE, 0.83),
        RMSE_Lower95 = safe_quantile(.data$RMSE, 0.025),
        RMSE_Upper95 = safe_quantile(.data$RMSE, 0.975),
        AdjSD_Mean = mean(.data$AdjSD, na.rm = TRUE),
        AdjSD_Lower66 = safe_quantile(.data$AdjSD, 0.17),
        AdjSD_Upper66 = safe_quantile(.data$AdjSD, 0.83),
        AdjSD_Lower95 = safe_quantile(.data$AdjSD, 0.025),
        AdjSD_Upper95 = safe_quantile(.data$AdjSD, 0.975),
        Reliability_Mean = mean(.data$Reliability, na.rm = TRUE),
        Reliability_Lower66 = safe_quantile(.data$Reliability, 0.17),
        Reliability_Upper66 = safe_quantile(.data$Reliability, 0.83),
        Reliability_Lower95 = safe_quantile(.data$Reliability, 0.025),
        Reliability_Upper95 = safe_quantile(.data$Reliability, 0.975),
        Separation_Mean = mean(.data$Separation, na.rm = TRUE),
        Separation_Lower66 = safe_quantile(.data$Separation, 0.17),
        Separation_Upper66 = safe_quantile(.data$Separation, 0.83),
        Separation_Lower95 = safe_quantile(.data$Separation, 0.025),
        Separation_Upper95 = safe_quantile(.data$Separation, 0.975),
        Strata_Mean = mean(.data$Strata, na.rm = TRUE),
        Strata_Lower66 = safe_quantile(.data$Strata, 0.17),
        Strata_Upper66 = safe_quantile(.data$Strata, 0.83),
        Strata_Lower95 = safe_quantile(.data$Strata, 0.025),
        Strata_Upper95 = safe_quantile(.data$Strata, 0.975),
        .groups = "drop"
      ) |>
      dplyr::left_join(
        ranef_summary |>
          dplyr::count(.data$Facet, name = "N_levels") |>
          dplyr::distinct(),
        by = "Facet"
      )
  } else {
    reliability_draws <- tibble::tibble()
    reliability_summary <- tibble::tibble()
  }

  desc_stats <- tryCatch({
    response_numeric <- as.numeric(df$Response)
    list(
      overall = psych::describe(response_numeric),
      by_facet = purrr::map(active_facets, function(facet) {
        df |>
          dplyr::mutate(Level = as.character(.data[[facet]])) |>
          dplyr::group_by(.data$Level) |>
          dplyr::summarise(
            N = dplyr::n(),
            Mean = mean(as.numeric(.data$Response), na.rm = TRUE),
            SD = stats::sd(as.numeric(.data$Response), na.rm = TRUE),
            Median = stats::median(as.numeric(.data$Response), na.rm = TRUE),
            .groups = "drop"
          ) |>
          dplyr::mutate(Facet = facet) |>
          dplyr::select(.data$Facet, .data$Level, dplyr::everything())
      }) |> purrr::set_names(active_facets)
    )
  }, error = function(e) {
    list(overall = data.frame(), by_facet = purrr::set_names(vector("list", length(active_facets)), active_facets))
  })

  pca_results <- if (length(active_facets) == 0) {
    NULL
  } else {
    tryCatch({
      residual_matrix_prep <- df_aug |>
        dplyr::mutate(
          item_combination = paste(!!!rlang::syms(active_facets), sep = "_"),
          Person = as.character(.data$Person)
        ) |>
        dplyr::select(.data$Person, item_combination, .data$std_residual) |>
        dplyr::group_by(.data$Person, .data$item_combination) |>
        dplyr::summarise(std_residual = mean(.data$std_residual, na.rm = TRUE), .groups = "drop")

      residual_matrix_wide <- residual_matrix_prep |>
        tidyr::pivot_wider(
          id_cols = Person,
          names_from = item_combination,
          values_from = std_residual,
          values_fill = list(std_residual = NA)
        ) |>
        tibble::column_to_rownames("Person")

      if (ncol(residual_matrix_wide) < 2 || nrow(residual_matrix_wide) < 2) {
        NULL
      } else {
        residual_matrix_clean <- residual_matrix_wide[, colSums(is.na(residual_matrix_wide)) < nrow(residual_matrix_wide), drop = FALSE]
        if (ncol(residual_matrix_clean) < 2) {
          NULL
        } else {
          cor_matrix <- stats::cor(residual_matrix_clean, use = "pairwise.complete.obs")
          cor_matrix[is.na(cor_matrix)] <- 0
          diag(cor_matrix) <- 1
          cor_matrix <- ensure_positive_definite(cor_matrix)
          n_factors <- max(1, min(10, ncol(cor_matrix) - 1, nrow(cor_matrix) - 1))
          pca_result <- psych::principal(cor_matrix, nfactors = n_factors, rotate = "none")
          list(pca = pca_result, residual_matrix = residual_matrix_wide, cor_matrix = cor_matrix)
        }
      }
    }, error = function(e) NULL)
  }

  pca_by_facet <- compute_pca_by_facet(df_aug, active_facets)

  loo_fit <- tryCatch({
    withCallingHandlers({
      loo_obj <- loo::loo(fit)
      if (!is.null(loo_obj$diagnostics$pareto_k) && any(loo_obj$diagnostics$pareto_k > 0.7, na.rm = TRUE)) {
        loo_obj <- loo::loo(fit, moment_match = TRUE)
      }
      loo_obj
    }, warning = function(w) {
      invokeRestart("muffleWarning")
    })
  }, error = function(e) NULL)

  waic_fit <- tryCatch({
    waic_warning <- NULL
    result <- withCallingHandlers(
      loo::waic(fit),
      warning = function(w) {
        waic_warning <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
    attr(result, "warning") <- waic_warning
    result
  }, error = function(e) NULL)

  family_name <- tryCatch(fit$family$family, error = function(e) "")
  ordinal_families <- c("cumulative", "sratio", "cratio", "acat", "ocat")
  bayes_r2 <- if (family_name %in% ordinal_families) {
    NULL
  } else {
    tryCatch(
      withCallingHandlers(brms::bayes_R2(fit), warning = function(w) invokeRestart("muffleWarning")),
      error = function(e) NULL
    )
  }

  model_fit <- list(
    looic = if (!is.null(loo_fit)) loo_fit$estimates["looic", "Estimate"] else NA_real_,
    looic_se = if (!is.null(loo_fit)) loo_fit$estimates["looic", "SE"] else NA_real_,
    waic = if (!is.null(waic_fit)) waic_fit$estimates["waic", "Estimate"] else NA_real_,
    waic_se = if (!is.null(waic_fit)) waic_fit$estimates["waic", "SE"] else NA_real_,
    waic_warning = attr(waic_fit, "warning", exact = TRUE),
    bayes_r2 = if (!is.null(bayes_r2)) mean(bayes_r2) else NA_real_
  )

  list(
    model = fit,
    data = df_aug,
    facet_names = active_facets,
    n_categories = prepared$n_categories,
    ranef_summary = ranef_summary,
    ranef_draws = ranef_long,
    threshold_summary = threshold_summary,
    threshold_draws = threshold_draws,
    fit_summary = fit_summary,
    residuals = df_aug$residual,
    std_residuals = df_aug$std_residual,
    reliability_summary = reliability_summary,
    reliability_draws = reliability_draws,
    model_fit = model_fit,
    desc_stats = desc_stats,
    pca_results = pca_results,
    pca_by_facet = pca_by_facet,
    dropped_facets = prepared$dropped_facets %||% character(),
    original_facets = prepared$original_facets %||% active_facets
  )
}

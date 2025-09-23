# Helper functions to transform analysis outputs into JSON-friendly payloads
# for the Plumber API.

assemble_data_preview <- function(df, limit = 50) {
  head(df, limit)
}

assemble_data_structure <- function(df) {
  tibble::tibble(
    Column = names(df),
    Type = vapply(df, function(x) paste(class(x), collapse = ","), character(1)),
    NAs = vapply(df, function(x) sum(is.na(x)), integer(1)),
    Unique = vapply(df, function(x) length(unique(x)), integer(1))
  )
}

assemble_response_distribution <- function(df) {
  response <- factor(df$Response)
  tibble::tibble(
    Response = levels(response),
    Count = as.integer(table(response)[levels(response)])
  )
}

assemble_convergence_table <- function(model) {
  draws_summary <- tryCatch(posterior::summarise_draws(model), error = function(e) NULL)
  max_rhat <- if (!is.null(draws_summary) && "rhat" %in% names(draws_summary)) {
    suppressWarnings(max(draws_summary$rhat, na.rm = TRUE))
  } else NA_real_
  min_bulk <- if (!is.null(draws_summary) && "ess_bulk" %in% names(draws_summary)) {
    suppressWarnings(min(draws_summary$ess_bulk, na.rm = TRUE))
  } else NA_real_
  min_tail <- if (!is.null(draws_summary) && "ess_tail" %in% names(draws_summary)) {
    suppressWarnings(min(draws_summary$ess_tail, na.rm = TRUE))
  } else NA_real_

  nuts_pars <- tryCatch(brms::nuts_params(model), error = function(e) NULL)
  divergences <- if (!is.null(nuts_pars)) {
    sum(nuts_pars$Parameter == "divergent__" & nuts_pars$Value == 1)
  } else NA_integer_

  chains <- tryCatch(model$fit@sim$chains, error = function(e) NA_integer_)
  iter <- tryCatch(model$fit@sim$iter, error = function(e) NA_integer_)
  warmup <- tryCatch(model$fit@sim$warmup, error = function(e) NA_integer_)
  post_draws <- tryCatch(posterior::ndraws(model), error = function(e) NA_integer_)

  tibble::tibble(
    Metric = c(
      "Max R-hat",
      "Min Bulk ESS",
      "Min Tail ESS",
      "Divergent Transitions",
      "Chains × Iterations",
      "Post-warmup Draws"
    ),
    Value = c(
      ifelse(is.na(max_rhat), NA, max_rhat),
      ifelse(is.na(min_bulk), NA, min_bulk),
      ifelse(is.na(min_tail), NA, min_tail),
      ifelse(is.na(divergences), NA, divergences),
      ifelse(is.na(iter) || is.na(chains), NA,
             sprintf("%s × %s (warmup %s)", chains, iter, warmup)),
      ifelse(is.na(post_draws), NA, post_draws)
    )
  )
}

assemble_model_summary_table <- function(model, df) {
  link_val <- tryCatch(model$family$link, error = function(e) "logit")
  threshold_val <- tryCatch(model$family$threshold, error = function(e) "flexible")
  n_obs <- nrow(df)
  chains <- tryCatch(model$fit@sim$chains, error = function(e) NA_integer_)
  iter <- tryCatch(model$fit@sim$iter, error = function(e) NA_integer_)
  warmup <- tryCatch(model$fit@sim$warmup, error = function(e) NA_integer_)
  post_draws <- tryCatch(posterior::ndraws(model), error = function(e) NA_integer_)

  tibble::tibble(
    Property = c(
      "Link Function",
      "Threshold Structure",
      "Observations",
      "Chains",
      "Iterations (per chain)",
      "Warmup (per chain)",
      "Post-warmup Draws"
    ),
    Value = c(
      link_val,
      threshold_val,
      n_obs,
      chains,
      iter,
      warmup,
      post_draws
    )
  )
}

assemble_random_effects_variance <- function(model) {
  draws <- tryCatch(posterior::as_draws_df(model), error = function(e) NULL)
  draws <- to_plain_draws(draws)
  if (is.null(draws) || ncol(draws) == 0) {
    return(tibble::tibble())
  }

  sd_cols <- grep("^sd_", names(draws), value = TRUE)
  if (length(sd_cols) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(sd_cols, function(col) {
    values <- draws[[col]]
    facet <- sub("^sd_(.+)__.*$", "\\1", col)
    term <- sub("^sd_.+__(.+)$", "\\1", col)
    tibble::tibble(
      Facet = facet,
      Term = term,
      Mean = mean(values, na.rm = TRUE),
      SD = stats::sd(values, na.rm = TRUE),
      Lower66 = safe_quantile(values, 0.17),
      Upper66 = safe_quantile(values, 0.83),
      Lower95 = safe_quantile(values, 0.025),
      Upper95 = safe_quantile(values, 0.975)
    )
  })
}

assemble_reliability_table <- function(rel_summary) {
  rel_summary
}

assemble_interpretation_guide <- function() {
  tibble::tibble(
    Index = c("Reliability", "Separation", "Strata", "RMSE", "Adjusted SD"),
    Interpretation = c(
      "Proportion of observed variance that is true variance (>0.8 good)",
      "Spread of measures in SE units (>2 good, >3 excellent)",
      "Number of statistically distinct levels (>3 good)",
      "Root Mean Square Error - measurement precision",
      "True standard deviation after removing measurement error"
    ),
    Good_Value = c(">0.80", ">2.00", ">3.00", "Lower is better", "Higher indicates more spread")
  )
}

assemble_fit_statistics <- function(fit_summary) {
  fit_summary |>
    dplyr::mutate(MaxAbsZ = pmax(abs(.data$InfitZSTD), abs(.data$OutfitZSTD)))
}

assemble_fit_scatter_data <- function(fit_summary) {
  base_data <- fit_summary |>
    dplyr::mutate(
      Facet = as.character(.data$Facet),
      DisplayLabel = paste(.data$Facet, .data$Level, sep = ": ")
    ) |>
    dplyr::arrange(.data$Facet, .data$Level)

  msq_data <- base_data |>
    tidyr::pivot_longer(
      cols = c(.data$InfitMSQ, .data$OutfitMSQ),
      names_to = "Statistic",
      values_to = "Value"
    ) |>
    dplyr::mutate(
      Statistic = dplyr::recode(.data$Statistic, InfitMSQ = "Infit MSQ", OutfitMSQ = "Outfit MSQ"),
      RangeFlag = dplyr::case_when(
        .data$Value < 0.5 ~ "Below 0.5",
        .data$Value > 1.5 ~ "Above 1.5",
        TRUE ~ "Within 0.5-1.5"
      )
    )

  z_data <- base_data |>
    dplyr::mutate(
      Flagged = abs(.data$InfitZSTD) > 2 | abs(.data$OutfitZSTD) > 2,
      FlagStatus = dplyr::if_else(.data$Flagged, "Flagged (|Z| > 2)", "Within ±2"),
      PointSize = dplyr::if_else(.data$Flagged, 11, 9)
    )

  list(msq = msq_data, zstd = z_data)
}

assemble_residual_diagnostics <- function(df) {
  res_data <- tibble::tibble(
    Fitted = df$expected,
    Residuals = df$residual,
    StdResiduals = df$std_residual,
    Observed = as.numeric(df$Response)
  )

  qq <- stats::qqnorm(res_data$StdResiduals, plot.it = FALSE)
  qq_data <- tibble::tibble(Theoretical = qq$x, Sample = qq$y)

  density_vals <- tryCatch(stats::density(res_data$StdResiduals, na.rm = TRUE), error = function(e) NULL)
  density_df <- if (!is.null(density_vals) && length(density_vals$x) > 1) {
    step <- density_vals$x[2] - density_vals$x[1]
    n_valid <- sum(!is.na(res_data$StdResiduals))
    tibble::tibble(x = density_vals$x, y = density_vals$y * n_valid * step)
  } else {
    tibble::tibble()
  }

  list(
    residuals = res_data,
    qq = qq_data,
    density = density_df
  )
}

assemble_wright_map_data <- function(ranef_summary, threshold_summary, ranef_draws) {
  ability_draws <- ranef_draws |>
    dplyr::filter(.data$Facet == "Person") |>
    dplyr::mutate(type = "Person")

  point_data <- ranef_summary |>
    dplyr::filter(.data$Facet != "Person") |>
    dplyr::transmute(
      label = paste(.data$Facet, .data$Level, sep = ":"),
      type = .data$Facet,
      Mean = .data$Mean,
      Lower66 = .data$Lower66,
      Upper66 = .data$Upper66,
      Lower95 = .data$Lower95,
      Upper95 = .data$Upper95
    )

  if (!is.null(threshold_summary) && nrow(threshold_summary) > 0) {
    threshold_df <- threshold_summary |>
      dplyr::transmute(
        label = paste0("Threshold ", .data$Threshold),
        type = "Threshold",
        Mean = .data$Mean,
        Lower66 = .data$Lower66,
        Upper66 = .data$Upper66,
        Lower95 = .data$Lower95,
        Upper95 = .data$Upper95
      )
    point_data <- dplyr::bind_rows(point_data, threshold_df)
  }

  list(
    ability_draws = ability_draws,
    point_data = point_data
  )
}

assemble_pca_outputs <- function(pca_bundle) {
  if (is.null(pca_bundle) || is.null(pca_bundle$pca)) {
    return(list(eigen = tibble::tibble(), loadings = tibble::tibble()))
  }
  pca <- pca_bundle$pca
  n_components <- min(10, ncol(pca$loadings))
  eigen_df <- tibble::tibble(
    Component = paste0("PC", seq_len(n_components)),
    Eigenvalue = pca$values[seq_len(n_components)],
    Variance_Pct = pca$Vaccounted[2, seq_len(n_components)] * 100,
    Cumulative_Pct = pca$Vaccounted[3, seq_len(n_components)] * 100
  )

  loadings_df <- tibble::tibble(
    Item = rownames(pca$loadings),
    PC1 = pca$loadings[, 1],
    PC2 = if (ncol(pca$loadings) >= 2) pca$loadings[, 2] else NA_real_
  )

  list(eigen = eigen_df, loadings = loadings_df)
}

assemble_threshold_table <- function(threshold_summary, n_categories) {
  if (is.null(threshold_summary) || nrow(threshold_summary) == 0) {
    return(tibble::tibble())
  }

  threshold_summary |>
    dplyr::mutate(
      Spacing = dplyr::lead(.data$Mean) - .data$Mean,
      N_Categories = n_categories
    )
}

assemble_probability_curves <- function(threshold_summary, n_categories, link_function) {
  if (is.null(threshold_summary) || nrow(threshold_summary) == 0 || n_categories < 2) {
    return(tibble::tibble())
  }

  link <- link_function %||% "logit"
  inv_link <- switch(
    link,
    logit = plogis,
    probit = pnorm,
    cauchit = pcauchy,
    loglog = function(x) 1 - exp(-exp(x)),
    cloglog = function(x) 1 - exp(-exp(x)),
    plogis
  )

  ability_seq <- seq(-4, 4, length.out = 200)
  threshold_values <- threshold_summary$Mean
  n_thresh <- length(threshold_values)

  prob_list <- lapply(seq_len(n_categories), function(cat) {
    probs <- sapply(ability_seq, function(theta) {
      if (cat == 1) {
        inv_link(threshold_values[1] - theta)
      } else if (cat == n_categories) {
        1 - inv_link(threshold_values[n_thresh] - theta)
      } else {
        inv_link(threshold_values[cat] - theta) - inv_link(threshold_values[cat - 1] - theta)
      }
    })
    tibble::tibble(
      Ability = ability_seq,
      Probability = pmax(pmin(probs, 1), 0),
      Category = factor(cat)
    )
  })

  dplyr::bind_rows(prob_list)
}

assemble_desc_stats <- function(desc_stats) {
  desc <- desc_stats$overall
  if (is.null(desc) || nrow(desc) == 0) {
    return(tibble::tibble())
  }
  row <- desc[1, , drop = TRUE]
  tibble::tibble(
    Statistic = c("N", "Mean", "SD", "Median", "Min", "Max", "Skewness", "Kurtosis"),
    Value = c(row$n, row$mean, row$sd, row$median, row$min, row$max, row$skew, row$kurtosis)
  )
}

assemble_facet_stats <- function(desc_stats) {
  desc_stats$by_facet
}

assemble_analysis_payload <- function(prepared, analysis, model_config) {
  list(
    metadata = list(
      messages = prepared$messages,
      warnings = prepared$warnings,
      link_function = model_config$link_function %||% "logit",
      threshold = model_config$threshold %||% "flexible",
      n_obs = prepared$n_obs,
      n_persons = prepared$n_persons,
      n_categories = prepared$n_categories,
      facets = prepared$facet_names,
      dropped_facets = prepared$dropped_facets
    ),
    data_preview = assemble_data_preview(prepared$data),
    data_structure = assemble_data_structure(prepared$data),
    response_distribution = assemble_response_distribution(prepared$data),
    model_summary = assemble_model_summary_table(analysis$model, analysis$data),
    convergence = assemble_convergence_table(analysis$model),
    random_effects_variance = assemble_random_effects_variance(analysis$model),
    facet_parameters = analysis$ranef_summary,
    reliability_table = assemble_reliability_table(analysis$reliability_summary),
    interpretation_guide = assemble_interpretation_guide(),
    fit_statistics = assemble_fit_statistics(analysis$fit_summary),
    fit_scatter = assemble_fit_scatter_data(analysis$fit_summary),
    residuals = assemble_residual_diagnostics(analysis$data),
    wright_map = assemble_wright_map_data(analysis$ranef_summary, analysis$threshold_summary, analysis$ranef_draws),
    pca_overall = assemble_pca_outputs(analysis$pca_results),
    pca_by_facet = purrr::map(analysis$pca_by_facet, assemble_pca_outputs),
    thresholds = assemble_threshold_table(analysis$threshold_summary, analysis$n_categories),
    probability_curves = assemble_probability_curves(analysis$threshold_summary, analysis$n_categories, model_config$link_function),
    desc_stats = assemble_desc_stats(analysis$desc_stats),
    facet_stats = assemble_facet_stats(analysis$desc_stats),
    model_fit_indices = analysis$model_fit
  )
}

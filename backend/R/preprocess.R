# Data preparation pipeline extracted from the original Shiny app.

validate_analysis_config <- function(df, config) {
  missing_fields <- setdiff(c("person_col", "response_col"), names(config))
  if (length(missing_fields) > 0) {
    stop(glue::glue("Missing configuration field(s): {paste(missing_fields, collapse = ', ')}"))
  }

  if (!config$person_col %in% names(df)) {
    stop(glue::glue("Person column '{config$person_col}' not found in data."))
  }

  if (!config$response_col %in% names(df)) {
    stop(glue::glue("Response column '{config$response_col}' not found in data."))
  }

  facet_cols <- unlist(config$facet_cols %||% character(), use.names = FALSE)
  invalid_facets <- setdiff(facet_cols, names(df))
  if (length(invalid_facets) > 0) {
    stop(glue::glue("Facet column(s) not found: {paste(invalid_facets, collapse = ', ')}"))
  }

  config$facet_cols <- facet_cols
  config
}

prepare_analysis_data <- function(df, config) {
  config <- validate_analysis_config(df, config)

  messages <- list()
  warnings <- list()

  data <- tibble::as_tibble(df)
  person_raw <- data[[config$person_col]]
  response_raw <- data[[config$response_col]]

  missing_person <- is.na(person_raw)
  if (any(missing_person)) {
    removed_person <- sum(missing_person)
    data <- data[!missing_person, , drop = FALSE]
    person_raw <- person_raw[!missing_person]
    response_raw <- response_raw[!missing_person]
    warnings <- append(warnings, list(glue::glue("{removed_person} row(s) removed due to missing Person IDs.")))
  }

  missing_response <- is.na(response_raw)
  if (any(missing_response)) {
    removed_response <- sum(missing_response)
    data <- data[!missing_response, , drop = FALSE]
    person_raw <- person_raw[!missing_response]
    response_raw <- response_raw[!missing_response]
    warnings <- append(warnings, list(glue::glue("{removed_response} row(s) removed due to missing responses.")))
  }

  if (nrow(data) == 0) {
    stop("No observations remain after removing missing values.")
  }

  person_vec <- as.factor(person_raw)
  response_vec <- as.ordered(response_raw)

  unique_responses <- unique(response_vec)
  n_unique_responses <- length(unique_responses)
  if (n_unique_responses < 2) {
    stop(glue::glue("Response variable must have at least 2 categories. Found only {n_unique_responses}."))
  }

  facet_cols <- unlist(config$facet_cols %||% character(), use.names = FALSE)
  facets_df <- data[, facet_cols, drop = FALSE]
  active_facets <- character(0)
  dropped_facets <- character(0)

  if (ncol(facets_df) > 0) {
    facets_df[] <- lapply(facets_df, function(col) {
      col_chr <- as.character(col)
      col_chr[is.na(col_chr) | trimws(col_chr) == ""] <- "Missing"
      factor(col_chr)
    })

    missing_facets <- names(facets_df)[vapply(facets_df, function(col) any(col == "Missing"), logical(1))]
    if (length(missing_facets) > 0) {
      warnings <- append(warnings, list(glue::glue("Blank facet entries detected in {paste(missing_facets, collapse = ', ')} → recoded as 'Missing'.")))
    }

    facet_counts <- vapply(facets_df, function(col) dplyr::n_distinct(col, na.rm = TRUE), integer(1))
    active_facets <- names(facet_counts[facet_counts > 1])
    dropped_facets <- setdiff(names(facets_df), active_facets)

    if (length(dropped_facets) > 0) {
      warnings <- append(warnings, list(glue::glue("Excluded facets with only one observed level: {paste(dropped_facets, collapse = ', ')}.")))
    }

    facets_df <- if (length(active_facets) > 0) {
      facets_df[, active_facets, drop = FALSE]
    } else {
      facets_df[, 0, drop = FALSE]
    }
  }

  analysis_df <- dplyr::bind_cols(
    tibble::tibble(Person = person_vec, Response = response_vec),
    facets_df
  ) |>
    tibble::as_tibble()

  analysis_df$Response <- droplevels(analysis_df$Response)
  final_response_levels <- length(levels(analysis_df$Response))
  if (final_response_levels < 2) {
    stop(glue::glue("After processing the data, response variable has only {final_response_levels} categories."))
  }

  n_obs <- nrow(analysis_df)
  n_persons <- dplyr::n_distinct(analysis_df$Person)

  if (n_obs < 30) {
    warnings <- append(warnings, list("Data has fewer than 30 observations. Model may be unstable."))
  }

  if (n_persons < 2) {
    stop("At least two persons are required for estimation.")
  }

  messages <- append(messages, list(glue::glue(
    "Data prepared: {n_obs} observations, {n_persons} persons, {length(levels(analysis_df$Response))} response categories."
  )))

  list(
    data = analysis_df,
    facet_names = active_facets,
    dropped_facets = dropped_facets,
    original_facets = facet_cols,
    n_obs = n_obs,
    n_persons = n_persons,
    n_categories = length(levels(analysis_df$Response)),
    messages = messages,
    warnings = warnings
  )
}

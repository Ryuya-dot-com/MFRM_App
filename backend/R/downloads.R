build_download_table <- function(result, type = c("summary", "reliability", "fit", "thresholds", "facets")) {
  type <- match.arg(type)
  switch(
    type,
    summary = {
      model_info <- tibble::tibble(
        Section = "Model Info",
        Property = result$model_summary$Property,
        Value = as.character(result$model_summary$Value)
      )

      fit_indices <- tibble::tibble(
        Section = "Fit Indices",
        Property = names(result$model_fit_indices),
        Value = unlist(result$model_fit_indices)
      )

      reliability <- tibble::tibble(
        Section = "Reliability",
        Property = paste(result$reliability_table$Facet, "Reliability_Mean"),
        Value = result$reliability_table$Reliability_Mean
      )

      tibble::bind_rows(model_info, fit_indices, reliability)
    },
    reliability = {
      tibble::as_tibble(result$reliability_table)
    },
    fit = {
      tibble::as_tibble(result$fit_statistics)
    },
    thresholds = {
      tibble::as_tibble(result$thresholds)
    },
    facets = {
      tibble::as_tibble(result$facet_parameters)
    }
  )
}

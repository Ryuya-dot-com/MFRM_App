# Utility helpers shared across the backend analysis pipeline
# These functions are extracted from the original Shiny server logic
# and rewritten to work in a headless environment.

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

format_tab_template <- function(df) {
  char_df <- df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(as.character(.x), "")))

  widths <- vapply(seq_along(char_df), function(i) {
    max(nchar(c(names(char_df)[i], char_df[[i]])), na.rm = TRUE)
  }, integer(1))

  format_row <- function(row_vec) {
    padded <- mapply(
      function(value, width) {
        value <- ifelse(is.na(value), "", value)
        stringr::str_pad(value, width = width, side = "right")
      },
      row_vec,
      widths,
      SIMPLIFY = TRUE
    )
    paste(padded, collapse = "\t")
  }

  header <- format_row(names(char_df))
  rows <- apply(char_df, 1, format_row)
  paste(c(header, rows), collapse = "\n")
}

sanitize_download_data <- function(df) {
  df |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ stringr::str_trim(as.character(.x))
    )) |>
    tidyr::drop_na() |>
    dplyr::filter(dplyr::if_all(dplyr::everything(), ~ nzchar(.x)))
}

make_delimited_export <- function(df, sep) {
  paste(
    capture.output(
      utils::write.table(df, sep = sep, row.names = FALSE, quote = FALSE)
    ),
    collapse = "\n"
  )
}

detect_separator <- function(text_block) {
  lines <- strsplit(text_block, "\n", fixed = TRUE)[[1]]
  lines <- lines[trimws(lines) != ""]
  if (length(lines) == 0) {
    return("\t")
  }
  candidate <- lines[[1]]
  if (stringr::str_detect(candidate, "\t")) {
    "\t"
  } else if (stringr::str_detect(candidate, ";")) {
    ";"
  } else if (stringr::str_detect(candidate, ",")) {
    ","
  } else {
    "\t"
  }
}

safe_quantile <- function(x, prob) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE, type = 7)
}

compute_zstd <- function(msq, df) {
  df[df < 1 | is.na(df)] <- 1
  z <- (msq^(1/3) - (1 - 2/(9 * df))) / sqrt(2/(9 * df))
  z[is.infinite(z)] <- NA_real_
  z
}

to_plain_draws <- function(draws_obj) {
  if (is.null(draws_obj)) {
    return(NULL)
  }
  draws_copy <- draws_obj
  attr(draws_copy, "meta") <- NULL
  class(draws_copy) <- setdiff(class(draws_copy), "draws_df")
  tibble::as_tibble(draws_copy)
}

ensure_positive_definite <- function(mat) {
  eig <- tryCatch(stats::eigen(mat, symmetric = TRUE, only.values = TRUE)$values,
                  error = function(e) NULL)
  if (is.null(eig)) {
    return(mat)
  }
  if (any(eig < .Machine$double.eps)) {
    smoothed <- tryCatch(suppressWarnings(psych::cor.smooth(mat)),
                         error = function(e) NULL)
    if (!is.null(smoothed)) {
      if (is.list(smoothed) && !is.null(smoothed$R)) {
        mat <- smoothed$R
      } else {
        mat <- smoothed
      }
    }
  }
  mat
}

compute_pca_by_facet <- function(df, facet_names) {
  out <- vector("list", length(facet_names))
  names(out) <- facet_names
  for (facet in facet_names) {
    prep <- df |>
      dplyr::mutate(
        Person = as.character(Person),
        .Level = as.character(.data[[facet]])
      ) |>
      dplyr::select(Person, .Level, std_residual) |>
      dplyr::group_by(Person, .Level) |>
      dplyr::summarise(std_residual = mean(std_residual, na.rm = TRUE), .groups = "drop")

    wide <- tryCatch({
      tidyr::pivot_wider(
        prep,
        id_cols = Person,
        names_from = .Level,
        values_from = std_residual,
        values_fill = list(std_residual = NA)
      ) |>
        tibble::column_to_rownames("Person")
    }, error = function(e) NULL)

    if (is.null(wide) || nrow(wide) < 2 || ncol(wide) < 2) {
      out[[facet]] <- NULL
      next
    }

    keep <- colSums(is.na(wide)) < nrow(wide)
    wide <- wide[, keep, drop = FALSE]
    if (ncol(wide) < 2) {
      out[[facet]] <- NULL
      next
    }

    cor_mat <- tryCatch(stats::cor(wide, use = "pairwise.complete.obs"),
                        error = function(e) NULL)
    if (is.null(cor_mat)) {
      out[[facet]] <- NULL
      next
    }

    cor_mat[is.na(cor_mat)] <- 0
    diag(cor_mat) <- 1
    cor_mat <- ensure_positive_definite(cor_mat)

    n_factors <- max(1, min(10, ncol(cor_mat) - 1, nrow(cor_mat) - 1))
    pca_obj <- tryCatch(psych::principal(cor_mat, nfactors = n_factors, rotate = "none"),
                        error = function(e) NULL)
    out[[facet]] <- list(
      pca = pca_obj,
      cor_matrix = cor_mat,
      residual_matrix = wide
    )
  }
  out
}

summarize_fit_block <- function(dat) {
  n <- nrow(dat)
  variance <- dat$variance
  z2 <- (dat$residual^2) / variance
  w_sum <- sum(variance, na.rm = TRUE)
  infit <- ifelse(w_sum > 0, sum(variance * z2, na.rm = TRUE) / w_sum, NA_real_)
  outfit <- mean(z2, na.rm = TRUE)
  n_eff_infit <- ifelse(w_sum > 0, (w_sum^2) / sum((variance^2), na.rm = TRUE), NA_real_)
  df_infit <- max(1, floor(ifelse(is.na(n_eff_infit), 1, n_eff_infit)))
  df_outfit <- max(1, n)
  tibble::tibble(
    N = n,
    InfitMSQ = infit,
    OutfitMSQ = outfit,
    InfitZSTD = compute_zstd(infit, df_infit),
    OutfitZSTD = compute_zstd(outfit, df_outfit),
    MeanResid = mean(dat$residual, na.rm = TRUE),
    SdResid = stats::sd(dat$residual, na.rm = TRUE)
  )
}

# Data templates and helper accessors for sample downloads

get_template_tab_source <- function() {
  tibble::tribble(
    ~Person, ~Task,  ~Rater,   ~Criterion,   ~Rating,
    "P01",   "Task1", "Rater1", "Content",    "4",
    "P01",   "Task1", "Rater2", "Grammar",    "3",
    "P01",   "Task2", "Rater3", "Vocabulary", "5",
    "P02",   "Task1", "Rater1", "Content",    "2",
    "P02",   "Task2", "Rater2", "Grammar",    "3",
    "P02",   "Task3", "Rater3", "Delivery",   "1",
    "P03",   "Task1", "Rater1", "Content",    "5",
    "P03",   "Task2", "Rater2", "Delivery",   "4",
    "P03",   "Task3", "Rater3", "Vocabulary", "2",
    "P04",   "Task1", "Rater1", "Content",    "3",
    "P04",   "Task2", "Rater2", "Grammar",    "4",
    "P05",   "Task1", "Rater3", "Delivery",   "5",
    "P05",   "Task2", "Rater3", "Vocabulary", "",
    "P06",   "Task3", "Rater2", "",           "3"
  )
}

get_template_columns <- function() {
  names(get_template_tab_source())
}

get_template_tab_text <- function() {
  format_tab_template(get_template_tab_source())
}

get_template_header_text <- function() {
  format_tab_template(get_template_tab_source()[0, ])
}

load_sample_dataset <- function(path = file.path("data", "sample_data.csv")) {
  source_df <- get_template_tab_source()
  if (file.exists(path)) {
    data <- tryCatch({
      sanitize_download_data(readr::read_csv(path, show_col_types = FALSE))
    }, error = function(e) {
      sanitize_download_data(source_df)
    })
  } else {
    data <- sanitize_download_data(source_df)
  }

  if (nrow(data) == 0) {
    data <- sanitize_download_data(source_df)
  }
  data
}

get_sample_downloads <- function() {
  sample_data <- load_sample_dataset()
  list(
    tsv = make_delimited_export(sample_data, "\t"),
    csv = make_delimited_export(sample_data, ",")
  )
}

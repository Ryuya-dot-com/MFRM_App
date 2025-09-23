# Load required packages
pacman::p_load(
  shiny, shinythemes, tidyverse, brms, ggrepel, plotly, DT, gt, psych,
  ggridges, tidybayes, posterior, loo, performance, bayestestR, effectsize,
  parameters, emmeans, shinyjs)

template_tab_source <- tribble(
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

format_tab_template <- function(df) {
  char_df <- df |> mutate(across(everything(), ~ replace_na(as.character(.x), "")))
  widths <- vapply(seq_along(char_df), function(i) {
    max(nchar(c(names(char_df)[i], char_df[[i]])), na.rm = TRUE)
  }, integer(1))
  format_row <- function(row_vec) {
    padded <- mapply(function(value, width) {
      value <- ifelse(is.na(value), "", value)
      stringr::str_pad(value, width = width, side = "right")
    }, row_vec, widths, SIMPLIFY = TRUE)
    paste(padded, collapse = "\t")
  }
  header <- format_row(names(char_df))
  rows <- apply(char_df, 1, format_row)
  paste(c(header, rows), collapse = "\n")
}

sanitize_download_data <- function(df) {
  df |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(everything(), ~ stringr::str_trim(as.character(.x)))) |>
    tidyr::drop_na() |>
    dplyr::filter(dplyr::if_all(everything(), ~ nzchar(.x)))
}

template_columns <- names(template_tab_source)
template_column_count <- length(template_columns)
template_tab_text <- format_tab_template(template_tab_source)
template_header_text <- format_tab_template(template_tab_source[0, ])
download_sample_path <- file.path("data", "sample_data.csv")
download_sample_data <- if (file.exists(download_sample_path)) {
  tryCatch({
    sanitize_download_data(readr::read_csv(download_sample_path, show_col_types = FALSE))
  }, error = function(e) {
    sanitize_download_data(template_tab_source)
  })
} else {
  sanitize_download_data(template_tab_source)
}

if (nrow(download_sample_data) == 0) {
  download_sample_data <- sanitize_download_data(template_tab_source)
}

make_delimited_export <- function(df, sep) {
  paste(
    capture.output(
      write.table(df, sep = sep, row.names = FALSE, quote = FALSE)
    ),
    collapse = "\n"
  )
}

sample_tsv_download <- make_delimited_export(download_sample_data, "\t")
sample_csv_download <- make_delimited_export(download_sample_data, ",")

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

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Define UI
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  theme = shinytheme("cerulean"),
  
  # Hidden download link for template
  tags$head(
    tags$style(HTML("
      #pastedData {
        font-family: 'Courier New', monospace;
        white-space: pre;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('downloadTemplate', function(message) {
        try {
          var mime = message.mime || 'text/tab-separated-values;charset=utf-8;';
          var blob = new Blob([message.content], {type: mime});
          var url = URL.createObjectURL(blob);
          var link = document.createElement('a');
          link.href = url;
          link.download = message.filename;
          link.style.display = 'none';
          document.body.appendChild(link);
          link.click();
          setTimeout(function() {
            document.body.removeChild(link);
            URL.revokeObjectURL(url);
          }, 100);
        } catch(e) {
          console.error('Download failed:', e);
          alert('Download failed. Please try again.');
        }
      });
    "))
  ),
  
  # Application title
  titlePanel("Many Facet Rasch Model (MFRM) Analysis - Cumulative Link Mixed Model"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Step 1: Data Input"),
      helpText("Review the quick template below (shows optional blanks). Download the sample TSV/CSV for a longer, clean dataset without missings."),
      radioButtons(
        "dataEntryMode", "Data entry method",
        choices = c("Paste table data" = "paste", "Upload file (CSV/TSV)" = "upload"),
        selected = "paste",
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.dataEntryMode == 'paste'",
        textAreaInput(
          "pastedData",
          "Template (edit or replace before loading)",
          value = template_tab_text,
          rows = 12,
          width = "100%",
          placeholder = "Person\tTask\tRater\tCriterion\tRating\nP01\tTask1\tRater1\tContent\t4"
        ),
        tags$div(
          class = "row",
          tags$div(
            class = "col-sm-8",
            textAreaInput(
              "singleRow",
              "Paste a single row to append (optional)",
              value = "",
              rows = 2,
              width = "100%",
              placeholder = "P06\tTask1\tRater2\tContent\t4"
            )
          ),
          tags$div(
            class = "col-sm-4",
            br(),
            actionButton("appendRow", "Append row", class = "btn btn-default btn-sm", style = "width: 100%; margin-bottom: 6px;"),
            actionButton("clearRows", "Clear table", class = "btn btn-link btn-sm", style = "width: 100%;")
          )
        ),
        helpText("Tip: Paste a full table or add rows one at a time. Blank cells are allowed and will appear as 'Missing' if needed; just keep the columns aligned.")
      ),
      conditionalPanel(
        condition = "input.dataEntryMode == 'upload'",
        fileInput(
          "file1", "Choose delimited file",
          accept = c("text/csv", "text/comma-separated-values,text/plain", "text/tab-separated-values", ".csv", ".tsv"),
          width = "100%"
        )
      ),
      tags$div(
        class = "download-buttons",
        actionButton("downloadSampleTSV", "Download sample TSV", class = "btn btn-info", style = "width: 100%; margin-bottom: 6px;"),
        actionButton("downloadSampleCSV", "Download sample CSV", class = "btn btn-info", style = "width: 100%;")
      ),
      br(),
      hr(),
      
      h4("Step 2: Data Configuration"),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = "\t"),
      actionButton("loadData", "Load Data", class = "btn-primary", style = "width: 100%;"),
      hr(),
      
      # Dynamic UI for column selection
      h4("Step 3: Column Mapping"),
      uiOutput("columnMapping"),
      hr(),
      
      # Model selection
      h4("Step 4: Model Configuration"),
      radioButtons("linkFunction", "Link Function:",
                   choices = c("Logit" = "logit",
                               "Probit" = "probit",
                               "Cauchit" = "cauchit",
                               "Log-log" = "loglog"),
                   selected = "logit"),
      radioButtons("threshold", "Threshold Structure:",
                   choices = c("Flexible" = "flexible",
                               "Symmetric" = "symmetric",
                               "Equidistant" = "equidistant"),
                   selected = "flexible"),
      hr(),
      
      # Analysis button
      h4("Step 5: Analysis"),
      actionButton("analyze", "Run MFRM Analysis", class = "btn-success"),
      hr(),
      
      h4("Instructions"),
      tags$ol(
        tags$li("Paste data into the template or use file upload"),
        tags$li("Adjust settings (header, separator) if needed"),
        tags$li("Click 'Load Data' to preview your data"),
        tags$li("Map columns to MFRM components"),
        tags$li("Configure model options"),
        tags$li("Click 'Run MFRM Analysis' to perform the analysis")
      ),
      
      tags$div(
        class = "alert alert-info",
        tags$strong("Note:"), " This app uses Cumulative Link Mixed Models (CLMM) for MFRM analysis, providing proper separation and strata indices for all facets."
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "mainTabs",
        tabPanel("Data Preview",
                 h4("Uploaded Data Preview"),
                 gt_output("dataPreview"),
                 h4("Data Structure"),
                 gt_output("dataStructureTable"),
                 h4("Response Distribution"),
                 plotlyOutput("responseDistribution", height = "300px")
        ),
        tabPanel("Model Summary",
                 conditionalPanel(
                   condition = "input.analyze > 0",
                   downloadButton("downloadSummary", "Download Full Report"),
                   h4("Model Convergence"),
                   gt_output("convergenceTable"),
                   h4("Model Summary"),
                   gt_output("modelSummaryTable"),
                   h4("Fixed Effects"),
                   gt_output("fixedEffects"),
                   h4("Random Effects Variance"),
                   gt_output("randomEffectsVar")
                 )
        ),
        tabPanel("Facet Parameters",
                 conditionalPanel(
                   condition = "input.analyze > 0",
                   h4("Facet Difficulty/Severity Parameters"),
                   uiOutput("facetTables"),
                   downloadButton("downloadFacetParams", "Download Facet Parameters")
                 )
        ),
        tabPanel("Reliability & Separation",
                 conditionalPanel(
                   condition = "input.analyze > 0",
                   h4("Reliability, Separation, and Strata Indices"),
                   downloadButton("downloadReliability", "Download Reliability Table"),
                   gt_output("reliabilityTable"),
                   h4("Interpretation Guide"),
                   gt_output("interpretationGuide"),
                   plotlyOutput("separationPlot", height = "400px")
                 )
        ),
        tabPanel("Fit Statistics",
                 conditionalPanel(
                   condition = "input.analyze > 0",
                    h4("Model Fit Indices"),
                    gt_output("modelFitTable"),
                    h4("Residual Analysis"),
                    plotlyOutput("residualPlots", height = "600px"),
                    h4("Fit Diagnostics Overview"),
                    plotlyOutput("fitSummaryPlots", height = "500px"),
                    h4("Person-Item Fit"),
                    downloadButton("downloadFitStats", "Download Fit Statistics"),
                    gt_output("fitStatistics")
                 )
        ),
        tabPanel("Dimensionality Analysis",
                 conditionalPanel(
                   condition = "input.analyze > 0",
                   h4("Principal Component Analysis of Residuals"),
                   p("This analysis examines the unidimensionality assumption of the MFRM model by analyzing patterns in standardized residuals."),
                   uiOutput("pcaFacetSelector"),
                   hr(),
                   h4("Scree Plot"),
                   plotlyOutput("pcaScreePlot", height = "400px"),
                   h4("Eigenvalues and Variance Explained"),
                   gt_output("pcaEigenvaluesTable"),
                   h4("First Principal Component Loadings"),
                   plotlyOutput("pcaLoadingsPlot", height = "500px"),
                   h4("Loadings Biplot"),
                   plotlyOutput("pcaBiplot", height = "500px"),
                   h4("Dimensionality Assessment"),
                   gt_output("dimensionalityAssessment")
                 )
        ),
        tabPanel("Wright Map",
                 conditionalPanel(
                   condition = "input.analyze > 0",
                   h4("Wright Map - Variable Map"),
                   plotlyOutput("wrightMap", height = "700px"),
                   h4("Interactive Facet Comparison"),
                   plotlyOutput("facetComparison", height = "500px")
                 )
        ),
        tabPanel("Thresholds",
                 conditionalPanel(
                   condition = "input.analyze > 0",
                   h4("Category Thresholds"),
                   downloadButton("downloadThresholds", "Download Threshold Table"),
                   gt_output("thresholdTable"),
                    h4("Threshold Map"),
                    plotlyOutput("thresholdMap", height = "500px"),
                    h4("Category Probability Curves"),
                    plotlyOutput("probabilityCurves", height = "500px")
                 )
        ),
        tabPanel("Descriptive Statistics",
                 conditionalPanel(
                   condition = "input.analyze > 0",
                   h4("Overall Statistics"),
                   gt_output("overallStats"),
                   h4("Statistics by Facets"),
                   uiOutput("facetStats")
                 )
        ),
        tabPanel("Help",
                 h3("About MFRM with CLMM"),
                 p("This application implements Many Facet Rasch Models using Cumulative Link Mixed Models (CLMM)."),
                 
                 h3("Quick Start"),
                 tags$ol(
                   tags$li("Paste data into the template or upload a CSV/TSV file."),
                   tags$li("Check the parsing options (header, separator) and load the preview."),
                   tags$li("Map Person, Response, and facet columns."),
                   tags$li("Choose the link function and threshold structure, then run the analysis."),
                   tags$li("Review results across the tabs; download CSV summaries if needed.")
                 ),
                 
                 h3("Minimum Data Requirements"),
                 tags$ul(
                   tags$li("Long format table with one row per rating (Person × Task × Rater × Criterion)."),
                   tags$li("At least one ordered response column with two or more categories (ideally three or more)."),
                   tags$li("Person ID and at least one additional facet to estimate random-effect spreads."),
                   tags$li("Recommended sample: ≥30 observations and ≥3 levels per facet for stable estimates.")
                 ),
                 
                 h3("Key Indices Explained"),
                 tags$ul(
                   tags$li(strong("Reliability:"), " Consistency of estimates (0-1, >0.8 is good)"),
                   tags$li(strong("Separation:"), " Spread of estimates in SE units (>2 is good)"),
                   tags$li(strong("Strata:"), " Number of distinct levels ((4*Sep+1)/3, >3 is good)"),
                   tags$li(strong("RMSE:"), " Root Mean Square Error of estimates"),
                   tags$li(strong("Adj SD:"), " True standard deviation (adjusted for error)")
                 ),
                 
                 h3("Model Specifications"),
                 p("The CLMM approach models the probability of response categories using:"),
                 tags$ul(
                   tags$li("Cumulative link function (logit, probit, etc.)"),
                   tags$li("Random effects for persons and facets"),
                   tags$li("Flexible, symmetric, or equidistant thresholds")
                 ),
                 
                 h3("Interpreting Outputs"),
                 p("Begin with the Model Summary tab to confirm convergence, then inspect Facet Parameters, Reliability & Separation, Fit Statistics, Dimensionality Analysis, and Wright Map/Thresholds in that order."),
                 p("For a full walkthrough, consult README.md in the repository."),
                 
                 h4("Session Info"),
                 gt_output("sessionInfoTable")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    data = NULL,
    loadedData = NULL,
    columnNames = NULL,
    model = NULL,
    analysisResults = NULL,
    analysisData = NULL,
    pcaResults = NULL  # Add PCA results storage
  )
  
  # Handle analyze button click
  observeEvent(input$analyze, {
    req(values$loadedData)
    req(input$personCol, input$responseCol)
    
    # Prepare the data
    if (input$personCol == "" || input$responseCol == "") {
      showNotification("Please select both Person ID and Response columns!", type = "error")
      return()
    }
    
    if (length(input$facetCols) == 0) {
      showNotification("Please select at least one facet column!", type = "error")
      return()
    }
    
    data <- values$loadedData
    
    # Show what columns are being used
    showNotification(paste("Using columns - Person:", input$personCol, 
                           "Response:", input$responseCol,
                           "Facets:", paste(input$facetCols, collapse = ", ")), 
                     type = "message", duration = 10)
    
    # Clean and prepare data
    person_raw <- data[[input$personCol]]
    response_raw <- data[[input$responseCol]]

    missing_person <- is.na(person_raw)
    if (any(missing_person)) {
      removed_person <- sum(missing_person)
      data <- data[!missing_person, , drop = FALSE]
      person_raw <- person_raw[!missing_person]
      response_raw <- response_raw[!missing_person]
      showNotification(
        paste(removed_person, "row(s) removed due to missing Person IDs before analysis."),
        type = "warning", duration = 8
      )
    }

    missing_response <- is.na(response_raw)
    if (any(missing_response)) {
      removed_response <- sum(missing_response)
      data <- data[!missing_response, , drop = FALSE]
      person_raw <- person_raw[!missing_response]
      response_raw <- response_raw[!missing_response]
      showNotification(
        paste(removed_response, "row(s) removed due to missing responses before analysis."),
        type = "warning", duration = 8
      )
    }

    if (nrow(data) == 0) {
      showNotification("No observations remain after removing missing values.", type = "error")
      values$analysisData <- NULL
      return()
    }

    person_vec <- as.factor(person_raw)
    response_vec <- as.ordered(response_raw)
    
    # Check response variable levels
    unique_responses <- unique(response_vec)
    n_unique_responses <- length(unique_responses)
    
    if (n_unique_responses < 2) {
      showNotification(paste("Error: Response variable must have at least 2 categories. Found only", 
                             n_unique_responses, "categories."), type = "error")
      values$analysisData <- NULL
      return()
    }
    
    # Create facets dataframe (allow blanks by tagging as 'Missing')
    facets_df <- data[, input$facetCols, drop = FALSE]
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
        showNotification(
          paste("Blank facet entries detected in:", paste(missing_facets, collapse = ", "), "→ recoded as 'Missing'."),
          type = "warning", duration = 7
        )
      }

      facet_counts <- vapply(facets_df, function(col) dplyr::n_distinct(col, na.rm = TRUE), integer(1))
      active_facets <- names(facet_counts[facet_counts > 1])
      dropped_facets <- setdiff(names(facets_df), active_facets)
      if (length(dropped_facets) > 0) {
        showNotification(
          paste(
            "Excluded facets with only one observed level:",
            paste(dropped_facets, collapse = ", ")
          ),
          type = "warning", duration = 10
        )
      }

      facets_df <- if (length(active_facets) > 0) {
        facets_df[, active_facets, drop = FALSE]
      } else {
        facets_df[, 0, drop = FALSE]
      }
    }
    
    # Combine into analysis dataframe
    analysis_df <- data.frame(
      Person = person_vec,
      Response = response_vec,
      facets_df,
      stringsAsFactors = FALSE
    )

    # Final response level check
    analysis_df$Response <- droplevels(analysis_df$Response)
    final_response_levels <- length(levels(analysis_df$Response))

    if (final_response_levels < 2) {
      showNotification(paste("Error: After processing the data, response variable has only", 
                             final_response_levels, 
                             "categories. At least 2 categories are required for modeling."), 
                       type = "error")
      values$analysisData <- NULL
      return()
    }
    
    if (nrow(analysis_df) < 30) {
      showNotification("Warning: Data has fewer than 30 observations. Model may be unstable.", 
                       type = "warning")
    }

    if (dplyr::n_distinct(analysis_df$Person) < 2) {
      showNotification("Error: At least two persons are required for estimation.", type = "error")
      values$analysisData <- NULL
      return()
    }
    
    # Store prepared data
    values$analysisData <- list(
      data = analysis_df,
      facet_names = active_facets,
      dropped_facets = dropped_facets,
      original_facets = input$facetCols,
      n_obs = nrow(analysis_df),
      n_persons = length(unique(analysis_df$Person)),
      n_categories = length(levels(analysis_df$Response))
    )
    
    showNotification(paste("Data prepared:", values$analysisData$n_obs, "observations,",
                           values$analysisData$n_persons, "persons,",
                           values$analysisData$n_categories, "response categories"), 
                     type = "message", duration = 10)
  })

  observeEvent(input$appendRow, {
    new_line <- gsub("\r", "", input$singleRow %||% "")
    new_line <- trimws(new_line)
    if (!nzchar(new_line)) {
      showNotification("Paste a row before appending.", type = "error", duration = 4)
      return()
    }
    detected <- if (stringr::str_detect(new_line, "\t")) {
      "\t"
    } else if (stringr::str_detect(new_line, ";")) {
      ";"
    } else if (stringr::str_detect(new_line, ",")) {
      ","
    } else {
      "\t"
    }
    fields <- if (identical(detected, "\t") || identical(detected, ";") || identical(detected, ",")) {
      strsplit(new_line, detected, fixed = TRUE)[[1]]
    } else {
      strsplit(new_line, "[[:space:]]+")[[1]]
    }
    fields <- trimws(fields)
    if (length(fields) > template_column_count) {
      showNotification(paste("Row has", length(fields), "columns but expected", template_column_count, "."),
                       type = "error", duration = 6)
      return()
    }
    if (length(fields) < template_column_count) {
      fields <- c(fields, rep("", template_column_count - length(fields)))
    }
    new_row <- tibble::as_tibble_row(setNames(as.list(fields), template_columns))
    current_text <- isolate(input$pastedData)
    current_text <- trimws(gsub("\r", "", current_text %||% ""), which = "both")
    current_df <- tryCatch({
      if (!nzchar(current_text)) {
        template_tab_source[0, ]
      } else {
        read.table(
          text = current_text,
          header = TRUE,
          sep = detect_separator(current_text),
          stringsAsFactors = FALSE,
          fill = TRUE,
          blank.lines.skip = TRUE,
          strip.white = TRUE,
          na.strings = c("", "NA"),
          comment.char = "",
          check.names = FALSE
        ) |> tibble::as_tibble()
      }
    }, error = function(e) {
      showNotification("Could not parse existing table; resetting to template header.", type = "warning", duration = 6)
      template_tab_source[0, ]
    })
    missing_cols <- setdiff(template_columns, names(current_df))
    if (length(missing_cols) > 0) {
      current_df[missing_cols] <- ""
    }
    current_df <- current_df[, template_columns, drop = FALSE]
    combined_df <- dplyr::bind_rows(current_df, new_row)
    updated_text <- format_tab_template(combined_df)
    updateTextAreaInput(session, "pastedData", value = updated_text)
    updateTextAreaInput(session, "singleRow", value = "")
    showNotification("Row appended to the table.", type = "message", duration = 4)
  })

  observeEvent(input$clearRows, {
    updateTextAreaInput(session, "pastedData", value = template_header_text)
    updateTextAreaInput(session, "singleRow", value = "")
    showNotification("Template cleared. Header retained for new entries.", type = "message", duration = 4)
  })
  
  # Sample data downloads
  observeEvent(input$downloadSampleTSV, {
    session$sendCustomMessage("downloadTemplate", list(
      content = sample_tsv_download,
      filename = paste0("MFRM_SampleData_", format(Sys.Date(), "%Y%m%d"), ".tsv"),
      mime = "text/tab-separated-values;charset=utf-8;"
    ))
    showNotification("Sample TSV downloaded (no missing values). Ready for immediate analysis.", type = "message")
  })

  observeEvent(input$downloadSampleCSV, {
    session$sendCustomMessage("downloadTemplate", list(
      content = sample_csv_download,
      filename = paste0("MFRM_SampleData_", format(Sys.Date(), "%Y%m%d"), ".csv"),
      mime = "text/csv;charset=utf-8;"
    ))
    showNotification("Sample CSV downloaded (no missing values). Ready for immediate analysis.", type = "message")
  })

  observeEvent(input$dataEntryMode, {
    if (identical(input$dataEntryMode, "upload")) {
      updateRadioButtons(session, "sep", selected = ",")
    } else {
      updateRadioButtons(session, "sep", selected = "\t")
    }
  }, ignoreNULL = FALSE)
  
  # Load data
  observeEvent(input$loadData, {
    tryCatch({
      mode <- input$dataEntryMode
      if (is.null(mode)) {
        mode <- "paste"
      }
      sep_choice <- input$sep
      if (is.null(sep_choice) || !nzchar(sep_choice)) {
        sep_choice <- if (identical(mode, "paste")) "\t" else ","
      }
      if (identical(mode, "paste")) {
        pasted <- input$pastedData
        if (is.null(pasted)) {
          pasted <- ""
        }
        pasted <- trimws(pasted)
        if (!nzchar(pasted)) {
          showNotification("Paste data into the template before loading.", type = "error", duration = 5)
          return()
        }
        df <- read.table(text = pasted,
                         header = isTRUE(input$header),
                         sep = sep_choice,
                         stringsAsFactors = FALSE,
                         fill = TRUE,
                         blank.lines.skip = TRUE,
                         strip.white = TRUE,
                         na.strings = c("", "NA"),
                         comment.char = "",
                         check.names = FALSE)
      } else {
        req(input$file1)
        df <- read.table(input$file1$datapath,
                         header = isTRUE(input$header),
                         sep = sep_choice,
                         stringsAsFactors = FALSE,
                         fill = TRUE,
                         blank.lines.skip = TRUE,
                         strip.white = TRUE,
                         na.strings = c("", "NA"),
                         comment.char = "",
                         check.names = FALSE)
      }
      
      # Validate loaded data
      if (nrow(df) < 10) {
        showNotification("Warning: Data has fewer than 10 rows. More data may be needed for reliable analysis.", 
                         type = "warning", duration = 5)
      }
      
      if (ncol(df) < 2) {
        showNotification("Error: Data must have at least 2 columns (Person ID and Response).", 
                         type = "error", duration = 5)
        return()
      }

      if (isTRUE(input$header)) {
        col_names <- names(df)
        blank_idx <- which(!nzchar(col_names))
        if (length(blank_idx) > 0) {
          auto_names <- paste0("Column_", blank_idx)
          names(df)[blank_idx] <- auto_names
          showNotification(
            paste("Empty column headers detected and renamed to:", paste(auto_names, collapse = ", ")), 
            type = "warning", duration = 7
          )
        }
      }
      
      values$loadedData <- df
      values$columnNames <- names(df)
      
      showNotification("Data loaded successfully!", type = "message")
      updateTabsetPanel(session, "mainTabs", selected = "Data Preview")
      
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error", duration = 10)
    })
  })
  
  # Column mapping UI
  output$columnMapping <- renderUI({
    req(values$columnNames)
    
    tagList(
      selectInput("personCol", "Person ID Column:",
                  choices = c("", values$columnNames),
                  selected = ifelse("Person" %in% values$columnNames, "Person", "")),
      
      selectInput("responseCol", "Response/Rating Column:",
                  choices = c("", values$columnNames),
                  selected = ifelse("Rating" %in% values$columnNames, "Rating", 
                                    ifelse("Response" %in% values$columnNames, "Response", ""))),
      
      checkboxGroupInput("facetCols", "Select Facet Columns:",
                         choices = setdiff(values$columnNames, c(input$personCol, input$responseCol)),
                         selected = intersect(c("Task", "Rater", "Criterion", "Item"), 
                                              setdiff(values$columnNames, c(input$personCol, input$responseCol))))
    )
  })
  
  # Data preview
  output$dataPreview <- render_gt({
    req(values$loadedData)
    
    values$loadedData |>
      head(50) |>
      gt() |>
      tab_header(
        title = "Data Preview",
        subtitle = paste("Showing first 50 of", nrow(values$loadedData), "rows")
      ) |>
      tab_options(
        table.font.size = "small",
        data_row.padding = px(3)
      )
  })
  
  # Data structure
  output$dataStructureTable <- render_gt({
    req(values$loadedData)
    
    data.frame(
      Column = names(values$loadedData),
      Type = sapply(values$loadedData, class),
      NAs = sapply(values$loadedData, function(x) sum(is.na(x))),
      Unique = sapply(values$loadedData, function(x) length(unique(x))),
      stringsAsFactors = FALSE
    ) |>
      gt() |>
      tab_header(
        title = "Data Structure",
        subtitle = paste("Total rows:", nrow(values$loadedData))
      ) |>
      cols_label(
        Column = "Column Name",
        Type = "Data Type",
        NAs = "Missing Values",
        Unique = "Unique Values"
      ) |>
      tab_style(
        style = cell_fill(color = "lightyellow"),
        locations = cells_body(
          columns = NAs,
          rows = NAs > 0
        )
      )
  })
  
  # Response distribution
  output$responseDistribution <- plotly::renderPlotly({
    req(values$loadedData, input$responseCol)

    if (input$responseCol %in% names(values$loadedData)) {
      response_data <- data.frame(
        Response = factor(values$loadedData[[input$responseCol]])
      )

      gp <- ggplot(response_data, aes(x = Response)) +
        geom_bar(fill = "steelblue", color = "black", alpha = 0.7) +
        geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
        theme_minimal() +
        labs(
          title = "Distribution of Responses",
          x = "Response Category",
          y = "Frequency"
        ) +
        theme(text = element_text(size = 12))

      return(plotly::ggplotly(gp, tooltip = c("x", "count")))
    }

    plotly::plotly_empty(type = "bar", mode = "markers") |>
      plotly::layout(title = list(text = "Response column not available"))
  })
  
  # Helper functions for posterior summaries
  central_probs <- c(0.17, 0.83, 0.025, 0.975)

  summarise_ci <- function(x) {
    list(
      mean = mean(x, na.rm = TRUE),
      lower66 = safe_quantile(x, central_probs[1]),
      upper66 = safe_quantile(x, central_probs[2]),
      lower95 = safe_quantile(x, central_probs[3]),
      upper95 = safe_quantile(x, central_probs[4])
    )
  }

  safe_quantile <- function(x, prob) {
    if (all(is.na(x))) return(NA_real_)
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
    eig <- tryCatch(eigen(mat, symmetric = TRUE, only.values = TRUE)$values, error = function(e) NULL)
    if (is.null(eig)) {
      return(mat)
    }
    if (any(eig < .Machine$double.eps)) {
      smoothed <- tryCatch(suppressWarnings(psych::cor.smooth(mat)), error = function(e) NULL)
      if (!is.null(smoothed)) {
        # cor.smooth can return a list with matrix component "R"
        if (is.list(smoothed) && !is.null(smoothed$R)) {
          mat <- smoothed$R
        } else {
          mat <- smoothed
        }
      }
    }
    mat
  }
  
  # --------------------------
  # PCA of residuals by facet
  # --------------------------
  compute_pca_by_facet <- function(df, facet_names) {
    out <- list()
    for (facet in facet_names) {
      prep <- df |>
        dplyr::mutate(Person = as.character(Person), .Level = as.character(.data[[facet]])) |>
        dplyr::select(Person, .Level, std_residual) |>
        dplyr::group_by(Person, .Level) |>
        dplyr::summarise(std_residual = mean(std_residual, na.rm = TRUE), .groups = 'drop')
      
      wide <- tryCatch({
        tidyr::pivot_wider(prep, id_cols = Person, names_from = .Level,
                           values_from = std_residual, values_fill = list(std_residual = NA)) |>
          tibble::column_to_rownames("Person")
      }, error = function(e) NULL)
      
      if (is.null(wide) || nrow(wide) < 2 || ncol(wide) < 2) { out[[facet]] <- NULL; next }
      
      keep <- colSums(is.na(wide)) < nrow(wide)
      wide <- wide[, keep, drop = FALSE]
      if (ncol(wide) < 2) { out[[facet]] <- NULL; next }
      
      cor_mat <- tryCatch(stats::cor(wide, use = "pairwise.complete.obs"), error = function(e) NULL)
      if (is.null(cor_mat)) { out[[facet]] <- NULL; next }
      cor_mat[is.na(cor_mat)] <- 0; diag(cor_mat) <- 1
      cor_mat <- ensure_positive_definite(cor_mat)
      
      n_factors <- max(1, min(10, ncol(cor_mat) - 1, nrow(cor_mat) - 1))
      pca_obj <- tryCatch(psych::principal(cor_mat, nfactors = n_factors, rotate = "none"),
                          error = function(e) NULL)
      out[[facet]] <- list(pca = pca_obj, cor_matrix = cor_mat, residual_matrix = wide)
    }
    out
  }
  
  # Perform MFRM analysis using CLMM

  analysis <- reactive({
    req(values$analysisData)

    if (is.null(values$analysisData)) {
      return(NULL)
    }

    withProgress(message = 'Running MFRM Analysis...', value = 0, {
      data_input <- values$analysisData
      df <- data_input$data
      active_facets <- data_input$facet_names %||% character()

      if (is.null(df) || nrow(df) == 0) {
        showNotification("Error: Data frame is empty", type = "error")
        return(NULL)
      }

      incProgress(0.1, detail = sprintf("Processing %s observations...", nrow(df)))

      if (!"Response" %in% names(df)) {
        showNotification("Error: Response column not found in data", type = "error")
        return(NULL)
      }

      incProgress(0.2, detail = "Building brms formula...")

      random_effects_terms <- paste0("(1|", c("Person", active_facets), ")")
      formula_str <- paste("Response ~ 1 +", paste(random_effects_terms, collapse = " + "))

      priors <- c(
        set_prior("normal(0, 2)", class = "sd", lb = 0),
        set_prior("normal(0, 2)", class = "Intercept")
      )

      showNotification(
        sprintf("Fitting brms model with %s observations, %s persons, and %s response categories",
                nrow(df), length(unique(df$Person)), length(levels(df$Response))),
        type = "message", duration = 10
      )

      incProgress(0.35, detail = "Sampling posterior with brms...")

      fit <- tryCatch({
        brm(
          formula = as.formula(formula_str),
          data = df,
          family = cumulative(link = input$linkFunction, threshold = input$threshold),
          prior = priors,
          chains = 2,
          iter = 2000,
          warmup = 1000,
          cores = min(2, parallel::detectCores()),
          seed = 1234,
          control = list(adapt_delta = 0.95),
          refresh = 0
        )
      }, error = function(e) {
        showNotification(paste("Model fitting failed:", e$message), type = "error", duration = 10)
        return(NULL)
      })

      if (is.null(fit)) {
        return(NULL)
      }

      incProgress(0.55, detail = "Extracting posterior draws...")

      draws <- posterior::as_draws_df(fit) |> to_plain_draws()
      if (is.null(draws) || ncol(draws) == 0) {
        showNotification("Unable to extract posterior draws from fitted model.", type = "error", duration = 10)
        return(NULL)
      }

      ranef_cols <- grep("^r_", names(draws), value = TRUE)

      ranef_long <- if (length(ranef_cols) > 0) {
        draws |>
          dplyr::select(.draw, dplyr::all_of(ranef_cols)) |>
          tidyr::pivot_longer(-.draw, names_to = "parameter", values_to = "value") |>
          tidyr::extract(parameter, into = c("Facet", "Level", "Term"),
                         regex = "^r_(.+)\\[([^,]+),([^\\]]+)\\]$", remove = TRUE) |>
          dplyr::mutate(
            .draw = as.integer(.draw),
            Facet = as.character(Facet),
            Level = as.character(Level),
            Term = as.character(Term)
          ) |>
          dplyr::filter(Term == "Intercept") |>
          dplyr::mutate(
            Effect = value,
            Display = dplyr::if_else(Facet == "Person", Effect, -Effect),
            Parameter = dplyr::if_else(Facet == "Person", "Ability", "Difficulty")
          ) |>
          dplyr::select(.draw, Facet, Level, Parameter, Effect, Display)
      } else {
        tibble::tibble(.draw = integer(), Facet = character(), Level = character(),
                       Parameter = character(), Effect = numeric(), Display = numeric())
      }

      if (nrow(ranef_long) == 0) {
        showNotification("Warning: No random effects extracted. Check model specification.", type = "warning")
      }

      ranef_summary <- ranef_long |>
        dplyr::group_by(Facet, Level, Parameter) |>
        dplyr::summarise(
          Mean = mean(Display, na.rm = TRUE),
          Median = stats::median(Display, na.rm = TRUE),
          SD = stats::sd(Display, na.rm = TRUE),
          Lower66 = safe_quantile(Display, 0.17),
          Upper66 = safe_quantile(Display, 0.83),
          Lower95 = safe_quantile(Display, 0.025),
          Upper95 = safe_quantile(Display, 0.975),
          .groups = "drop"
        )

      threshold_cols <- grep("^b_Intercept\\[", names(draws), value = TRUE)

      threshold_draws <- if (length(threshold_cols) > 0) {
        draws |>
          dplyr::select(.draw, dplyr::all_of(threshold_cols)) |>
          tidyr::pivot_longer(-.draw, names_to = "Threshold", values_to = "Value") |>
          tidyr::extract(Threshold, into = "Threshold",
                         regex = "^b_Intercept\\[(.+)\\]$", remove = TRUE) |>
          dplyr::mutate(
            .draw = as.integer(.draw),
            Threshold = as.character(Threshold)
          )
      } else {
        tibble::tibble(.draw = integer(), Threshold = character(), Value = numeric())
      }

      threshold_summary <- threshold_draws |>
        dplyr::group_by(Threshold) |>
        dplyr::summarise(
          Mean = mean(Value, na.rm = TRUE),
          Median = stats::median(Value, na.rm = TRUE),
          SD = stats::sd(Value, na.rm = TRUE),
          Lower66 = safe_quantile(Value, 0.17),
          Upper66 = safe_quantile(Value, 0.83),
          Lower95 = safe_quantile(Value, 0.025),
          Upper95 = safe_quantile(Value, 0.975),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          ThresholdOrder = suppressWarnings(as.numeric(Threshold))
        ) |>
        dplyr::arrange(ThresholdOrder) |>
        dplyr::select(-ThresholdOrder)

      incProgress(0.7, detail = "Computing posterior predictive summaries...")

      available_draws <- posterior::ndraws(fit)
      nsamples <- min(400, available_draws)
      prob_array <- fitted(fit, summary = FALSE, ndraws = nsamples)

      prob_df <- as.data.frame.table(prob_array, responseName = "prob") |>
        dplyr::mutate(
          .draw = as.integer(Var1),
          .obs = as.integer(Var2),
          Category = as.integer(Var3)
        ) |>
        dplyr::select(.draw, .obs, Category, prob)

      prob_mean <- prob_df |>
        dplyr::group_by(.obs, Category) |>
        dplyr::summarise(prob = mean(prob, na.rm = TRUE), .groups = "drop")

      expected_summary <- prob_mean |>
        dplyr::group_by(.obs) |>
        dplyr::summarise(
          expected = sum(Category * prob),
          second_moment = sum((Category^2) * prob),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          variance = pmax(second_moment - expected^2, 1e-6)
        ) |>
        dplyr::select(.obs, expected, variance)

      observed_numeric <- as.numeric(df$Response)

      df_aug <- df |>
        dplyr::mutate(
          .obs = dplyr::row_number(),
          Person = as.character(Person),
          dplyr::across(dplyr::all_of(active_facets), as.character),
          observed = observed_numeric
        ) |>
        dplyr::left_join(expected_summary, by = ".obs") |>
        dplyr::mutate(
          residual = observed - expected,
          std_residual = residual / sqrt(variance)
        )

      fit_base <- df_aug

      summarize_fit <- function(dat) {
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

      person_fit <- fit_base |>
        dplyr::group_by(Person) |>
        dplyr::group_modify(~ summarize_fit(.x)) |>
        dplyr::ungroup() |>
        dplyr::mutate(Facet = "Person", Level = Person) |>
        dplyr::select(Facet, Level, dplyr::everything(), -Person)

      facet_fit <- if (length(active_facets) == 0) {
        person_fit[0, ]
      } else {
        purrr::map_dfr(active_facets, function(facet) {
          fit_base |>
            dplyr::group_by(.data[[facet]]) |>
            dplyr::group_modify(~ summarize_fit(.x)) |>
            dplyr::ungroup() |>
            dplyr::mutate(Facet = facet, Level = as.character(.data[[facet]])) |>
            dplyr::select(Facet, Level, dplyr::everything(), -dplyr::all_of(facet))
        })
      }

      fit_summary <- dplyr::bind_rows(person_fit, facet_fit)

      incProgress(0.82, detail = "Summarising reliability metrics...")

      if (nrow(ranef_long) > 0) {
        ranef_summary_means <- ranef_summary |>
          dplyr::select(Facet, Level, Mean)

        rmse_draws <- ranef_long |>
          dplyr::left_join(ranef_summary_means, by = c("Facet", "Level")) |>
          dplyr::mutate(Error = Display - Mean) |>
          dplyr::group_by(Facet, .draw) |>
          dplyr::summarise(RMSE = sqrt(mean(Error^2, na.rm = TRUE)), .groups = "drop")

        var_draws <- ranef_long |>
          dplyr::group_by(Facet, .draw) |>
          dplyr::summarise(VarLevel = stats::var(Display, na.rm = TRUE), .groups = "drop") |>
          dplyr::mutate(VarLevel = dplyr::if_else(is.na(VarLevel), 0, VarLevel))

        reliability_draws <- var_draws |>
          dplyr::left_join(rmse_draws, by = c("Facet", ".draw")) |>
          dplyr::mutate(
            RMSE = dplyr::if_else(is.na(RMSE), 0, RMSE),
            VarLevel = pmax(VarLevel, 0),
            AdjVar = pmax(VarLevel - RMSE^2, 0),
            ObsSD = sqrt(VarLevel),
            AdjSD = sqrt(AdjVar),
            Reliability = dplyr::if_else(VarLevel > 0, AdjVar / VarLevel, 0),
            Separation = dplyr::if_else(RMSE > 0, AdjSD / RMSE, 0),
            Strata = (4 * Separation + 1) / 3
          )

        reliability_summary <- reliability_draws |>
          dplyr::group_by(Facet) |>
          dplyr::summarise(
            ObsSD_Mean = mean(ObsSD, na.rm = TRUE),
            ObsSD_Lower66 = safe_quantile(ObsSD, 0.17),
            ObsSD_Upper66 = safe_quantile(ObsSD, 0.83),
            ObsSD_Lower95 = safe_quantile(ObsSD, 0.025),
            ObsSD_Upper95 = safe_quantile(ObsSD, 0.975),
            RMSE_Mean = mean(RMSE, na.rm = TRUE),
            RMSE_Lower66 = safe_quantile(RMSE, 0.17),
            RMSE_Upper66 = safe_quantile(RMSE, 0.83),
            RMSE_Lower95 = safe_quantile(RMSE, 0.025),
            RMSE_Upper95 = safe_quantile(RMSE, 0.975),
            AdjSD_Mean = mean(AdjSD, na.rm = TRUE),
            AdjSD_Lower66 = safe_quantile(AdjSD, 0.17),
            AdjSD_Upper66 = safe_quantile(AdjSD, 0.83),
            AdjSD_Lower95 = safe_quantile(AdjSD, 0.025),
            AdjSD_Upper95 = safe_quantile(AdjSD, 0.975),
            Reliability_Mean = mean(Reliability, na.rm = TRUE),
            Reliability_Lower66 = safe_quantile(Reliability, 0.17),
            Reliability_Upper66 = safe_quantile(Reliability, 0.83),
            Reliability_Lower95 = safe_quantile(Reliability, 0.025),
            Reliability_Upper95 = safe_quantile(Reliability, 0.975),
            Separation_Mean = mean(Separation, na.rm = TRUE),
            Separation_Lower66 = safe_quantile(Separation, 0.17),
            Separation_Upper66 = safe_quantile(Separation, 0.83),
            Separation_Lower95 = safe_quantile(Separation, 0.025),
            Separation_Upper95 = safe_quantile(Separation, 0.975),
            Strata_Mean = mean(Strata, na.rm = TRUE),
            Strata_Lower66 = safe_quantile(Strata, 0.17),
            Strata_Upper66 = safe_quantile(Strata, 0.83),
            Strata_Lower95 = safe_quantile(Strata, 0.025),
            Strata_Upper95 = safe_quantile(Strata, 0.975),
            .groups = "drop"
          ) |>
          dplyr::left_join(ranef_summary |>
                             dplyr::count(Facet, name = "N_levels") |>
                             dplyr::distinct(),
                           by = "Facet")
      } else {
        reliability_draws <- tibble::tibble()
        reliability_summary <- tibble::tibble()
      }

      incProgress(0.9, detail = "Preparing descriptive summaries...")

      desc_stats <- tryCatch({
        response_numeric <- as.numeric(df$Response)
        list(
          overall = psych::describe(response_numeric),
          by_facet = purrr::map(active_facets, function(facet) {
            df |>
              dplyr::mutate(Level = as.character(.data[[facet]])) |>
              dplyr::group_by(Level) |>
              dplyr::summarise(
                N = dplyr::n(),
                Mean = mean(as.numeric(Response), na.rm = TRUE),
                SD = stats::sd(as.numeric(Response), na.rm = TRUE),
                Median = stats::median(as.numeric(Response), na.rm = TRUE),
                .groups = "drop"
              ) |>
              dplyr::mutate(Facet = facet) |>
              dplyr::select(Facet, Level, dplyr::everything())
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
              Person = as.character(Person)
            ) |>
            dplyr::select(Person, item_combination, std_residual) |>
            dplyr::group_by(Person, item_combination) |>
            dplyr::summarise(std_residual = mean(std_residual, na.rm = TRUE), .groups = 'drop')

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
            showNotification("Pareto k > 0.7 detected; re-running loo with moment matching.", type = "warning", duration = 10)
            loo_obj <- loo::loo(fit, moment_match = TRUE)
          }
          loo_obj
        }, warning = function(w) {
          invokeRestart("muffleWarning")
        })
      }, error = function(e) {
        showNotification(paste("loo failed:", e$message), type = "error", duration = 10)
        NULL
      })

      waic_fit <- tryCatch({
        waic_warning <- NULL
        result <- withCallingHandlers(
          loo::waic(fit),
          warning = function(w) {
            waic_warning <<- conditionMessage(w)
            invokeRestart("muffleWarning")
          }
        )
        if (!is.null(waic_warning)) {
          showNotification(waic_warning, type = "warning", duration = 10)
        }
        result
      }, error = function(e) {
        showNotification(paste("WAIC failed:", e$message), type = "error", duration = 10)
        NULL
      })

      family_name <- tryCatch(fit$family$family, error = function(e) "")
      ordinal_families <- c("cumulative", "sratio", "cratio", "acat", "ocat")
      bayes_r2 <- if (family_name %in% ordinal_families) {
        showNotification("Skipping bayes_R2 for ordinal family; metric assumes continuous response.", type = "message", duration = 8)
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
        bayes_r2 = if (!is.null(bayes_r2)) mean(bayes_r2) else NA_real_
      )

      incProgress(1, detail = "Analysis complete!")

      values$pcaResults <- pca_results

      list(
        model = fit,
        data = df_aug,
        facet_names = active_facets,
        n_categories = data_input$n_categories,
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
        dropped_facets = data_input$dropped_facets %||% character(),
        original_facets = data_input$original_facets %||% active_facets
      )
    })
  })
  # PCA target selector UI and selected PCA bundle
  output$pcaFacetSelector <- renderUI({
    req(analysis())
    choices <- c("Overall", analysis()$facet_names)
    selectInput("pcaFacet", "Facet for residual PCA", choices = choices, selected = "Overall")
  })
  
  get_pca_selected <- reactive({
    req(analysis())
    if (is.null(input$pcaFacet) || input$pcaFacet == "Overall") {
      analysis()$pca_results
    } else {
      analysis()$pca_by_facet[[input$pcaFacet]]
    }
  })
  
  output$pcaScreePlot <- plotly::renderPlotly({
    req(get_pca_selected())
    pca_bundle <- get_pca_selected()
    if (is.null(pca_bundle) || is.null(pca_bundle$pca)) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "lines") |>
          plotly::layout(title = list(text = "PCA results not available"))
      )
    }
    pca <- pca_bundle$pca
    eigenvalues <- pca$values[1:min(20, length(pca$values))]
    
    scree_data <- data.frame(Component = 1:length(eigenvalues), Eigenvalue = eigenvalues)
    
    gp <- ggplot(scree_data, aes(x = Component, y = Eigenvalue)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      geom_hline(yintercept = 1, linetype = "dashed", linewidth = 1) +
      scale_x_continuous(breaks = 1:length(eigenvalues)) +
      labs(
        title = paste0("Scree Plot of Residual PCA (", ifelse(is.null(input$pcaFacet), "Overall", input$pcaFacet), ")"),
        subtitle = "Dashed line indicates eigenvalue = 1 (Kaiser criterion)",
        x = "Principal Component",
        y = "Eigenvalue"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10)
      )

    plotly::ggplotly(gp, tooltip = c("x", "y"))
  })
  
  # PCA Eigenvalues Table
  output$pcaEigenvaluesTable <- render_gt({
    req(get_pca_selected())
    pca_bundle <- get_pca_selected()
    if (is.null(pca_bundle) || is.null(pca_bundle$pca)) {
      return(
        data.frame(Message = "PCA results not available. Check if data has sufficient variability.") |>
          gt() |>
          tab_header(title = "Principal Component Analysis Results")
      )
    }
    pca <- pca_bundle$pca
    n_components <- min(10, ncol(pca$loadings))
    
    eigen_df <- data.frame(
      Component = paste0("PC", 1:n_components),
      Eigenvalue = pca$values[1:n_components],
      Variance_Pct = pca$Vaccounted[2, 1:n_components] * 100,
      Cumulative_Pct = pca$Vaccounted[3, 1:n_components] * 100
    )
    
    eigen_df |>
      gt() |>
      tab_header(
        title = "Principal Component Analysis Results",
        subtitle = "Eigenvalues and Variance Explained"
      ) |>
      fmt_number(columns = c(Eigenvalue), decimals = 3) |>
      fmt_number(columns = c(Variance_Pct, Cumulative_Pct), decimals = 1) |>
      cols_label(
        Component = "Component",
        Eigenvalue = "Eigenvalue",
        Variance_Pct = "% Variance",
        Cumulative_Pct = "Cumulative %"
      ) |>
      tab_style(
        style = cell_fill(color = "lightyellow"),
        locations = cells_body(
          columns = everything(),
          rows = Eigenvalue > 1
        )
      ) |>
      tab_footnote(
        footnote = "Highlighted rows have eigenvalues > 1",
        locations = cells_column_labels(columns = Eigenvalue)
      )
  })
  
  # PCA Loadings Plot
  output$pcaLoadingsPlot <- plotly::renderPlotly({
    req(get_pca_selected())
    pca_bundle <- get_pca_selected()
    if (is.null(pca_bundle) || is.null(pca_bundle$pca)) {
      return(
        plotly::plotly_empty(type = "bar") |>
          plotly::layout(title = list(text = "PCA loadings not available"))
      )
    }
    pca <- pca_bundle$pca
    # Extract first component loadings
    loadings_pc1 <- data.frame(
      Item = rownames(pca$loadings),
      Loading = pca$loadings[, 1]
    ) |>
      arrange(desc(abs(Loading))) |>
      head(20)  # Show top 20 items
    
    gp <- ggplot(loadings_pc1, aes(x = reorder(Item, Loading), y = Loading, text = sprintf("%s<br>Loading: %.3f", Item, Loading))) +
      geom_bar(stat = "identity", fill = ifelse(loadings_pc1$Loading > 0, "steelblue", "coral")) +
      coord_flip() +
      labs(
        title = "First Principal Component Loadings",
        subtitle = "Top 20 items by absolute loading value",
        x = "Item (Rater_Task_Criterion)",
        y = "Loading on PC1"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 8)
      ) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black")

    plotly::ggplotly(gp, tooltip = "text")
  })
  
  # PCA Biplot
  output$pcaBiplot <- plotly::renderPlotly({
    req(get_pca_selected())
    pca_bundle <- get_pca_selected()
    if (is.null(pca_bundle) || is.null(pca_bundle$pca)) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "markers") |>
          plotly::layout(title = list(text = "PCA biplot not available"))
      )
    }
    pca <- pca_bundle$pca
    if(ncol(pca$loadings) >= 2) {
      loadings_df <- data.frame(
        Item = rownames(pca$loadings),
        PC1 = pca$loadings[, 1],
        PC2 = pca$loadings[, 2]
      )
      
      # Select top items by distance from origin
      loadings_df$Distance <- sqrt(loadings_df$PC1^2 + loadings_df$PC2^2)
      top_loadings <- loadings_df |>
        arrange(desc(Distance)) |>
        head(30)
      
      gp <- ggplot(top_loadings, aes(x = PC1, y = PC2, label = Item, text = sprintf("%s<br>PC1: %.3f<br>PC2: %.3f", Item, PC1, PC2))) +
        geom_point(color = "darkblue", size = 2) +
        geom_text(size = 3, hjust = 0, vjust = 0, check_overlap = TRUE) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
        labs(
          title = "PCA Biplot of Residuals",
          subtitle = "Top 30 items by distance from origin",
          x = paste0("PC1 (", round(pca$Vaccounted[2, 1] * 100, 1), "% variance)"),
          y = paste0("PC2 (", round(pca$Vaccounted[2, 2] * 100, 1), "% variance)")
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold")
        )

      plotly::ggplotly(gp, tooltip = "text")
    } else {
      plotly::plotly_empty(type = "scatter", mode = "markers") |>
        plotly::layout(title = list(text = "Insufficient components for biplot"))
    }
  })
  
  # Dimensionality Assessment
  output$dimensionalityAssessment <- render_gt({
    req(get_pca_selected())
    pca_bundle <- get_pca_selected()
    if (is.null(pca_bundle) || is.null(pca_bundle$pca)) {
      return(
        data.frame(Message = "PCA analysis not completed. Unable to assess dimensionality.") |>
          gt() |>
          tab_header(title = "Unidimensionality Assessment")
      )
    }
    pca <- pca_bundle$pca
    # Calculate assessment metrics
    pc1_variance <- pca$Vaccounted[2, 1] * 100
    eigenvalue_ratio <- if(length(pca$values) >= 2) pca$values[1] / pca$values[2] else NA
    eigenvalues_above_1 <- sum(pca$values > 1)
    
    assessment_df <- data.frame(
      Criterion = c(
        "First eigenvalue",
        "PC1 variance explained",
        "Eigenvalue ratio (1st/2nd)",
        "Number of eigenvalues > 1",
        "Unidimensionality assessment"
      ),
      Value = c(
        round(pca$values[1], 3),
        paste0(round(pc1_variance, 1), "%"),
        ifelse(is.na(eigenvalue_ratio), "N/A", round(eigenvalue_ratio, 2)),
        eigenvalues_above_1,
        ifelse(pc1_variance > 20 || eigenvalue_ratio < 3, 
               "Potential multidimensionality detected",
               "Acceptable unidimensionality")
      ),
      Interpretation = c(
        ifelse(pca$values[1] > 2, "Strong first dimension", "Weak first dimension"),
        ifelse(pc1_variance > 20, "High residual variance in PC1", "Acceptable residual variance"),
        ifelse(!is.na(eigenvalue_ratio) && eigenvalue_ratio > 3, 
               "Good separation of dimensions", 
               "Poor separation of dimensions"),
        ifelse(eigenvalues_above_1 > 1, 
               paste("Multiple significant components"), 
               "Single dominant component"),
        ifelse(pc1_variance < 20 && (!is.na(eigenvalue_ratio) && eigenvalue_ratio > 3),
               "Model shows good unidimensionality",
               "Consider investigating model fit")
      )
    )
    
    assessment_df |>
      gt() |>
      tab_header(
        title = "Unidimensionality Assessment",
        subtitle = "Based on PCA of standardized residuals"
      ) |>
      tab_style(
        style = cell_fill(color = ifelse(pc1_variance > 20, "lightyellow", "lightgreen")),
        locations = cells_body(rows = 2)
      ) |>
      tab_footnote(
        footnote = "PC1 variance < 20% indicates good unidimensionality",
        locations = cells_body(columns = Value, rows = 2)
      ) |>
      tab_footnote(
        footnote = "Eigenvalue ratio > 3 indicates good dimension separation",
        locations = cells_body(columns = Value, rows = 3)
      )
  })
  
  # Model Convergence Table
  output$convergenceTable <- render_gt({
    req(analysis())
    model <- analysis()$model

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

    convergence_data <- tibble::tibble(
      Metric = c(
        "Max R-hat",
        "Min Bulk ESS",
        "Min Tail ESS",
        "Divergent Transitions",
        "Chains × Iterations",
        "Post-warmup Draws"
      ),
      Value = c(
        ifelse(is.na(max_rhat), "NA", formatC(max_rhat, digits = 3, format = "f")),
        ifelse(is.na(min_bulk), "NA", formatC(min_bulk, digits = 0, format = "f")),
        ifelse(is.na(min_tail), "NA", formatC(min_tail, digits = 0, format = "f")),
        ifelse(is.na(divergences), "NA", as.character(divergences)),
        ifelse(is.na(iter) || is.na(chains), "NA", sprintf("%s × %s (warmup %s)", chains, iter, warmup)),
        ifelse(is.na(post_draws), "NA", as.character(post_draws))
      )
    )

    convergence_data |>
      gt() |>
      tab_header(title = "MCMC Convergence Diagnostics") |>
      tab_style(
        style = cell_fill(color = ifelse(!is.na(max_rhat) && max_rhat <= 1.01, "lightgreen", "lightyellow")),
        locations = cells_body(rows = Metric == "Max R-hat")
      ) |>
      tab_style(
        style = cell_fill(color = ifelse(!is.na(divergences) && divergences == 0, "lightgreen", "mistyrose")),
        locations = cells_body(rows = Metric == "Divergent Transitions")
      )
  })
  
  # Model Summary Table
  output$modelSummaryTable <- render_gt({
    req(analysis())
    model <- analysis()$model
    data <- analysis()$data

    link_val <- tryCatch(model$family$link, error = function(e) "logit")
    threshold_val <- tryCatch(model$family$threshold, error = function(e) "flexible")
    n_obs <- nrow(data)
    chains <- tryCatch(model$fit@sim$chains, error = function(e) NA_integer_)
    iter <- tryCatch(model$fit@sim$iter, error = function(e) NA_integer_)
    warmup <- tryCatch(model$fit@sim$warmup, error = function(e) NA_integer_)
    post_draws <- tryCatch(posterior::ndraws(model), error = function(e) NA_integer_)

    model_info <- tibble::tibble(
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
        formatC(n_obs, format = "d"),
        ifelse(is.na(chains), "NA", as.character(chains)),
        ifelse(is.na(iter), "NA", as.character(iter)),
        ifelse(is.na(warmup), "NA", as.character(warmup)),
        ifelse(is.na(post_draws), "NA", as.character(post_draws))
      )
    )

    model_info |>
      gt() |>
      tab_header(
        title = "Model Summary",
        subtitle = "brms cumulative link specification"
      )
  })
  
  # Fixed Effects (Thresholds)
  output$fixedEffects <- render_gt({
    req(analysis())
    thresholds <- analysis()$threshold_summary

    if (is.null(thresholds) || nrow(thresholds) == 0) {
      return(
        data.frame(Message = "No threshold parameters available") |>
          gt() |>
          tab_header(title = "Threshold Parameters")
      )
    }

    thresholds |>
      dplyr::mutate(
        `Mean` = Mean,
        `66% CrI` = sprintf("[%0.2f, %0.2f]", Lower66, Upper66),
        `95% CrI` = sprintf("[%0.2f, %0.2f]", Lower95, Upper95)
      ) |>
      dplyr::select(Threshold, `Mean`, Median, SD, `66% CrI`, `95% CrI`) |>
      gt() |>
      tab_header(
        title = "Threshold Parameters",
        subtitle = "Posterior summaries with 66% and 95% credible intervals"
      ) |>
      fmt_number(columns = c(`Mean`, Median, SD), decimals = 3)
  })
  
  # Random Effects Variance
  output$randomEffectsVar <- render_gt({
    req(analysis())
    model <- analysis()$model

    draws <- tryCatch(posterior::as_draws_df(model), error = function(e) NULL)
    draws <- to_plain_draws(draws)
    if (is.null(draws) || ncol(draws) == 0) {
      return(
        data.frame(Message = "Unable to extract variance components") |>
          gt() |>
          tab_header(title = "Random Effects Variance Components")
      )
    }

    sd_cols <- grep("^sd_", names(draws), value = TRUE)
    if (length(sd_cols) == 0) {
      return(
        data.frame(Message = "No random-effect standard deviations in model") |>
          gt() |>
          tab_header(title = "Random Effects Variance Components")
      )
    }

    sd_summary <- purrr::map_dfr(sd_cols, function(col) {
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

    sd_summary |>
      dplyr::mutate(
        `66% CrI` = sprintf("[%0.2f, %0.2f]", Lower66, Upper66),
        `95% CrI` = sprintf("[%0.2f, %0.2f]", Lower95, Upper95)
      ) |>
      dplyr::select(Facet, Term, Mean, SD, `66% CrI`, `95% CrI`) |>
      gt() |>
      tab_header(
        title = "Random Effects Standard Deviations",
        subtitle = "Posterior summaries for each group-level term"
      ) |>
      fmt_number(columns = c(Mean, SD), decimals = 3)
  })
  
  # Facet Parameters
  output$facetTables <- renderUI({
    req(analysis())

    ranef_summary <- analysis()$ranef_summary

    if (is.null(ranef_summary) || nrow(ranef_summary) == 0) {
      return(p("No facet parameters available"))
    }

    facet_names <- unique(ranef_summary$Facet)

    table_list <- purrr::map(seq_along(facet_names), function(i) {
      facet_name <- facet_names[i]
      table_id <- paste0("facetTable_", i)

      output[[table_id]] <- render_gt({
        facet_df <- ranef_summary |>
          dplyr::filter(Facet == facet_name) |>
          dplyr::arrange(dplyr::desc(Mean)) |>
          dplyr::mutate(
            `66% CrI` = sprintf("[%0.2f, %0.2f]", Lower66, Upper66),
            `95% CrI` = sprintf("[%0.2f, %0.2f]", Lower95, Upper95)
          ) |>
          dplyr::select(Level, Parameter, Mean, SD, `66% CrI`, `95% CrI`)

        if (nrow(facet_df) == 0) {
          return(data.frame(Message = paste("No parameters available for", facet_name)) |>
                   gt())
        }

        facet_df |>
          gt() |>
          tab_header(
            title = paste(facet_name, "Posterior Parameters"),
            subtitle = "Display values account for Rasch sign conventions"
          ) |>
          fmt_number(columns = c(Mean, SD), decimals = 3)
      })

      tagList(
        h5(paste(facet_name, "Parameters")),
        gt_output(table_id),
        br()
      )
    })

    do.call(tagList, table_list)
  })
  
  # Reliability Table
  output$reliabilityTable <- render_gt({
    req(analysis())
    rel_summary <- analysis()$reliability_summary

    if (is.null(rel_summary) || nrow(rel_summary) == 0) {
      return(
        data.frame(Message = "No reliability indices available. Model may not have converged properly.") |>
          gt() |>
          tab_header(title = "Reliability, Separation, and Strata Indices")
      )
    }

    rel_tbl <- rel_summary |>
      dplyr::rename(Levels = N_levels)

    rel_tbl |>
      gt() |>
      tab_header(
        title = "Reliability, Separation, and Strata Indices",
        subtitle = "Posterior means with 66% and 95% credible intervals"
      ) |>
      fmt_number(columns = Levels, decimals = 0) |>
      fmt_number(
        columns = tidyselect::matches("_Mean$"),
        decimals = 3
      ) |>
      fmt_number(
        columns = tidyselect::matches("_(Lower66|Upper66|Lower95|Upper95)$"),
        decimals = 3
      ) |>
      tab_spanner(
        label = "ObsSD",
        columns = dplyr::matches("^ObsSD_")
      ) |>
      tab_spanner(
        label = "RMSE",
        columns = dplyr::matches("^RMSE_")
      ) |>
      tab_spanner(
        label = "AdjSD",
        columns = dplyr::matches("^AdjSD_")
      ) |>
      tab_spanner(
        label = "Reliability",
        columns = dplyr::matches("^Reliability_")
      ) |>
      tab_spanner(
        label = "Separation",
        columns = dplyr::matches("^Separation_")
      ) |>
      tab_spanner(
        label = "Strata",
        columns = dplyr::matches("^Strata_")
      ) |>
      tab_style(
        style = cell_fill(color = "lightgreen"),
        locations = cells_body(
          columns = dplyr::matches("^Reliability_"),
          rows = !is.na(Reliability_Mean) & Reliability_Mean >= 0.8
        )
      ) |>
      tab_style(
        style = cell_fill(color = "lightgreen"),
        locations = cells_body(
          columns = dplyr::matches("^Separation_"),
          rows = !is.na(Separation_Mean) & Separation_Mean >= 2
        )
      ) |>
      tab_style(
        style = cell_fill(color = "lightgreen"),
        locations = cells_body(
          columns = dplyr::matches("^Strata_"),
          rows = !is.na(Strata_Mean) & Strata_Mean >= 3
        )
      )
  })
  
  # Interpretation Guide
  output$interpretationGuide <- render_gt({
    data.frame(
      Index = c("Reliability", "Separation", "Strata", "RMSE", "Adjusted SD"),
      Interpretation = c(
        "Proportion of observed variance that is true variance (>0.8 good)",
        "Spread of measures in SE units (>2 good, >3 excellent)",
        "Number of statistically distinct levels (>3 good)",
        "Root Mean Square Error - measurement precision",
        "True standard deviation after removing measurement error"
      ),
      `Good_Value` = c(">0.80", ">2.00", ">3.00", "Lower is better", "Higher indicates more spread"),
      stringsAsFactors = FALSE
    ) |>
      gt() |>
      tab_header(title = "Interpretation Guide for Indices") |>
      cols_label(Good_Value = "Good Value")
  })
  
  # Separation Plot
  output$separationPlot <- plotly::renderPlotly({
    req(analysis())
    
    rel_data <- analysis()$reliability_summary

    # Check if we have data to plot
    if (is.null(rel_data) || nrow(rel_data) == 0) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "markers") |>
          plotly::layout(title = list(text = "No reliability data available"))
      )
    }

    rel_plot_data <- rel_data |>
      dplyr::select(Facet, tidyselect::matches("^(Reliability|Separation|Strata)_(Mean|Lower66|Upper66|Lower95|Upper95)$")) |>
      tidyr::pivot_longer(
        cols = -Facet,
        names_to = c("Metric", "Summary"),
        names_pattern = "(Reliability|Separation|Strata)_(Mean|Lower66|Upper66|Lower95|Upper95)",
        values_to = "Value"
      ) |>
      tidyr::pivot_wider(names_from = Summary, values_from = Value)

    thresholds_df <- tibble::tibble(
      Metric = c("Reliability", "Separation", "Strata"),
      threshold = c(0.8, 2, 3)
    )

    gp <- ggplot(rel_plot_data, aes(x = Facet, y = Mean, text = sprintf("Facet: %s<br>Mean: %.3f\n66%% CI: [%.3f, %.3f]\n95%% CI: [%.3f, %.3f]", Facet, Mean, Lower66, Upper66, Lower95, Upper95))) +
      geom_hline(data = thresholds_df, aes(yintercept = threshold), linetype = "dashed", color = "red", alpha = 0.5) +
      geom_linerange(aes(ymin = Lower95, ymax = Upper95), linewidth = 0.6, alpha = 0.6, color = "grey30") +
      geom_linerange(aes(ymin = Lower66, ymax = Upper66), linewidth = 1.6, color = "black") +
      geom_point(size = 2, color = "black") +
      facet_wrap(~Metric, scales = "free_y") +
      theme_bw() +
      labs(title = "Measurement Quality Indices by Facet",
           subtitle = "Thick segments = 66% CrI, thin segments = 95% CrI",
           x = "Facet",
           y = "Estimate") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    plotly::ggplotly(gp, tooltip = "text")
  })
  
  # Fit Statistics Table (Person / Facet Fit Statistics)
  output$fitStatistics <- render_gt({
    req(analysis())
    fs <- analysis()$fit_summary
    if (is.null(fs) || nrow(fs) == 0) {
      return(data.frame(Message = "Fit statistics unavailable") |>
               gt() |> tab_header(title = "Person / Facet Fit Statistics"))
    }

    fs |>
      dplyr::mutate(MaxAbsZ = pmax(abs(InfitZSTD), abs(OutfitZSTD))) |>
      dplyr::arrange(dplyr::desc(MaxAbsZ)) |>
      gt() |>
      tab_header(
        title = "Person / Facet Fit Statistics",
        subtitle = "Mean-square and Z-standardized residual diagnostics"
      ) |>
      fmt_number(columns = c(N), decimals = 0) |>
      fmt_number(columns = c(InfitMSQ, OutfitMSQ, InfitZSTD, OutfitZSTD, MeanResid, SdResid), decimals = 3) |>
      tab_style(
        style = cell_fill(color = "mistyrose"),
        locations = cells_body(rows = InfitMSQ > 1.5 | OutfitMSQ > 1.5)
      ) |>
      tab_style(
        style = cell_fill(color = "lightcyan"),
        locations = cells_body(rows = InfitMSQ < 0.5 | OutfitMSQ < 0.5)
      ) |>
      cols_move_to_start(columns = c(Facet, Level, N)) |>
      cols_label(Facet = "Facet", Level = "Level")
  })

  output$fitSummaryPlots <- plotly::renderPlotly({
    req(analysis())

    fs <- analysis()$fit_summary

    if (is.null(fs) || nrow(fs) == 0) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "markers") |>
          plotly::layout(title = list(text = "Fit statistics unavailable"))
      )
    }

    base_data <- fs |>
      dplyr::mutate(
        Facet = as.character(Facet),
        DisplayLabel = paste(Facet, Level, sep = ": ")
      ) |>
      dplyr::arrange(Facet, Level)

    display_levels <- rev(unique(base_data$DisplayLabel))

    msq_data <- base_data |>
      tidyr::pivot_longer(
        cols = c(InfitMSQ, OutfitMSQ),
        names_to = "Statistic",
        values_to = "Value"
      ) |>
      dplyr::mutate(
        Statistic = dplyr::recode(Statistic, InfitMSQ = "Infit MSQ", OutfitMSQ = "Outfit MSQ"),
        RangeFlag = dplyr::case_when(
          Value < 0.5 ~ "Below 0.5",
          Value > 1.5 ~ "Above 1.5",
          TRUE ~ "Within 0.5-1.5"
        ),
        RangeFlag = factor(RangeFlag, levels = c("Below 0.5", "Within 0.5-1.5", "Above 1.5")),
        Tooltip = sprintf("%s<br>%s: %.2f<br>Status: %s", DisplayLabel, Statistic, Value, RangeFlag)
      ) |>
      dplyr::filter(!is.na(Value))

    if (nrow(msq_data) == 0) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "markers") |>
          plotly::layout(title = list(text = "Fit statistics unavailable"))
      )
    }

    msq_plot <- plotly::plot_ly(
      data = msq_data,
      x = ~Value,
      y = ~DisplayLabel,
      color = ~Statistic,
      colors = c("Infit MSQ" = "#1f77b4", "Outfit MSQ" = "#ff7f0e"),
      symbol = ~RangeFlag,
      symbols = c("triangle-down", "circle", "triangle-up"),
      text = ~Tooltip,
      hoverinfo = "text",
      type = "scatter",
      mode = "markers",
      marker = list(size = 10, opacity = 0.9)
    ) |>
      plotly::layout(
        title = list(text = "Mean-square Fit by Facet"),
        xaxis = list(title = "Mean-square statistic"),
        yaxis = list(
          title = "",
          categoryorder = "array",
          categoryarray = display_levels
        ),
        legend = list(orientation = "h", y = -0.15)
      )

    z_data <- base_data |>
      dplyr::mutate(
        Flagged = abs(InfitZSTD) > 2 | abs(OutfitZSTD) > 2,
        FlagStatus = dplyr::if_else(Flagged, "Flagged (|Z| > 2)", "Within ±2"),
        FlagStatus = factor(FlagStatus, levels = c("Within ±2", "Flagged (|Z| > 2)")),
        Tooltip = sprintf("%s<br>Infit ZSTD: %.2f<br>Outfit ZSTD: %.2f<br>Status: %s", DisplayLabel, InfitZSTD, OutfitZSTD, FlagStatus),
        PointSize = ifelse(Flagged, 11, 9)
      ) |>
      dplyr::filter(!is.na(InfitZSTD), !is.na(OutfitZSTD))

    if (nrow(z_data) == 0) {
      z_plot <- plotly::plotly_empty(type = "scatter", mode = "markers") |>
        plotly::layout(title = list(text = "Z-standardized Fit"), xaxis = list(title = "Infit ZSTD"), yaxis = list(title = "Outfit ZSTD"))
    } else {
      z_plot <- plotly::plot_ly(
      data = z_data,
      x = ~InfitZSTD,
      y = ~OutfitZSTD,
      color = ~Facet,
      symbol = ~FlagStatus,
      symbols = c("circle", "diamond"),
      sizes = c(9, 12),
      size = ~PointSize,
      text = ~Tooltip,
      hoverinfo = "text",
      type = "scatter",
      mode = "markers",
      marker = list(opacity = 0.85)
    ) |>
      plotly::layout(
        title = list(text = "Z-standardized Fit"),
        xaxis = list(title = "Infit ZSTD"),
        yaxis = list(title = "Outfit ZSTD"),
        legend = list(orientation = "h", y = -0.2)
      )
    }

    plotly::subplot(
      msq_plot,
      z_plot,
      nrows = 2,
      heights = c(0.65, 0.35),
      shareX = FALSE,
      titleY = TRUE
    ) |>
      plotly::layout(margin = list(l = 140, r = 20, t = 60, b = 60))
  })

  # Residual Plots
  output$residualPlots <- plotly::renderPlotly({
    req(analysis())

    df <- analysis()$data

    if (!"expected" %in% names(df)) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "markers") |>
          plotly::layout(title = list(text = "Expected values not available"))
      )
    }

    res_data <- data.frame(
      Fitted = df$expected,
      Residuals = analysis()$residuals,
      StdResiduals = analysis()$std_residuals,
      Observed = as.numeric(df$Response)
    )

    # Residuals vs Fitted with optional smooth
    scatter <- plotly::plot_ly(
      res_data,
      x = ~Fitted,
      y = ~Residuals,
      type = "scatter",
      mode = "markers",
      marker = list(opacity = 0.55),
      name = "Residual"
    ) |>
      plotly::add_trace(
        x = range(res_data$Fitted, na.rm = TRUE),
        y = c(0, 0),
        type = "scatter",
        mode = "lines",
        line = list(color = "red", dash = "dash"),
        showlegend = FALSE,
        inherit = FALSE
      )

    if (length(unique(res_data$Fitted)) >= 4 && nrow(res_data) >= 6) {
      smooth_vals <- tryCatch({
        suppressWarnings(stats::loess(Residuals ~ Fitted, data = res_data, span = 0.75))
      }, warning = function(w) NULL, error = function(e) NULL)
      if (!is.null(smooth_vals)) {
        fitted_curve <- stats::predict(smooth_vals, se = FALSE)
        smooth_df <- data.frame(Fitted = res_data$Fitted, Smooth = fitted_curve)
        smooth_df <- smooth_df[order(smooth_df$Fitted), , drop = FALSE]
        scatter <- scatter |>
          plotly::add_trace(
            data = smooth_df,
            x = ~Fitted,
            y = ~Smooth,
            type = "scatter",
            mode = "lines",
            line = list(color = "blue"),
            name = "Loess",
            inherit = FALSE
          )
      }
    }

    scatter <- scatter |>
      plotly::layout(
        title = list(text = "Residuals vs Fitted Values"),
        xaxis = list(title = "Fitted Values"),
        yaxis = list(title = "Residuals")
      )

    # Q-Q plot
    qq <- qqnorm(res_data$StdResiduals, plot.it = FALSE)
    qq_data <- data.frame(Theoretical = qq$x, Sample = qq$y)
    qq_plot <- plotly::plot_ly(
      qq_data,
      x = ~Theoretical,
      y = ~Sample,
      type = "scatter",
      mode = "markers",
      marker = list(color = "steelblue"),
      name = "Sample"
    ) |>
      plotly::add_trace(
        x = qq_data$Theoretical,
        y = qq_data$Theoretical,
        type = "scatter",
        mode = "lines",
        line = list(color = "red"),
        name = "Reference",
        showlegend = FALSE,
        inherit = FALSE
      ) |>
      plotly::layout(
        title = list(text = "Q-Q Plot of Standardized Residuals"),
        xaxis = list(title = "Theoretical Quantiles"),
        yaxis = list(title = "Sample Quantiles")
      )

    # Histogram with density overlay
    hist_plot <- plotly::plot_ly(
      res_data,
      x = ~StdResiduals,
      type = "histogram",
      nbinsx = 30,
      marker = list(color = "steelblue", line = list(color = "black", width = 0.5)),
      name = "Histogram"
    )

    density_vals <- tryCatch(stats::density(res_data$StdResiduals, na.rm = TRUE), error = function(e) NULL)
    if (!is.null(density_vals) && length(density_vals$x) > 1) {
      step <- density_vals$x[2] - density_vals$x[1]
      n_valid <- sum(!is.na(res_data$StdResiduals))
      density_df <- data.frame(x = density_vals$x, y = density_vals$y * n_valid * step)
      hist_plot <- hist_plot |>
        plotly::add_trace(
          data = density_df,
          x = ~x,
          y = ~y,
          type = "scatter",
          mode = "lines",
          line = list(color = "red"),
          name = "Density",
          inherit = FALSE
        )
    }
    hist_plot <- hist_plot |>
      plotly::layout(
        title = list(text = "Distribution of Standardized Residuals"),
        xaxis = list(title = "Standardized Residuals"),
        yaxis = list(title = "Count"),
        barmode = "overlay"
      )

    # Observed vs Fitted
    obs_plot <- plotly::plot_ly(
      res_data,
      x = ~Observed,
      y = ~Fitted,
      type = "scatter",
      mode = "markers",
      marker = list(opacity = 0.55),
      name = "Observed"
    ) |>
      plotly::add_trace(
        x = range(res_data$Observed, na.rm = TRUE),
        y = range(res_data$Observed, na.rm = TRUE),
        type = "scatter",
        mode = "lines",
        line = list(color = "red", dash = "dash"),
        name = "Reference",
        showlegend = FALSE,
        inherit = FALSE
      ) |>
      plotly::layout(
        title = list(text = "Observed vs Fitted Values"),
        xaxis = list(title = "Observed"),
        yaxis = list(title = "Fitted")
      )

    plotly::subplot(
      scatter,
      qq_plot,
      hist_plot,
      obs_plot,
      nrows = 2,
      shareX = FALSE,
      shareY = FALSE,
      titleX = TRUE,
      titleY = TRUE,
      margin = 0.05
    ) |>
      plotly::layout(margin = list(l = 60, r = 30, t = 60, b = 60))
  })
  
  # Wright Map

  output$wrightMap <- plotly::renderPlotly({
    req(analysis())
    ranef_summary <- analysis()$ranef_summary
    thresholds <- analysis()$threshold_summary
    ranef_draws <- analysis()$ranef_draws

    if (is.null(ranef_summary) || nrow(ranef_summary) == 0) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "markers") |>
          plotly::layout(title = list(text = "No random effects data available"))
      )
    }

    ability_draws <- ranef_draws |>
      dplyr::filter(Facet == "Person") |>
      dplyr::mutate(type = "Person")

    point_data <- ranef_summary |>
      dplyr::filter(Facet != "Person") |>
      dplyr::transmute(
        label = paste(Facet, Level, sep = ":"),
        type = Facet,
        Mean = Mean,
        Lower66 = Lower66,
        Upper66 = Upper66,
        Lower95 = Lower95,
        Upper95 = Upper95
      )

    if (!is.null(thresholds) && nrow(thresholds) > 0) {
      threshold_df <- thresholds |>
        dplyr::transmute(
          label = paste0("Threshold ", Threshold),
          type = "Threshold",
          Mean = Mean,
          Lower66 = Lower66,
          Upper66 = Upper66,
          Lower95 = Lower95,
          Upper95 = Upper95
        )
      point_data <- dplyr::bind_rows(point_data, threshold_df)
    }

    if (nrow(point_data) == 0) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "markers") |>
          plotly::layout(title = list(text = "No facet parameters available for Wright map"))
      )
    }

    point_data <- point_data |>
      dplyr::mutate(
        type = factor(type, levels = unique(c("Person", setdiff(unique(type), "Person")))),
        label = factor(label, levels = rev(unique(label))),
        tooltip = sprintf("%s<br>Mean: %.3f<br>66%% CI: [%.3f, %.3f]<br>95%% CI: [%.3f, %.3f]",
                          label, Mean, Lower66, Upper66, Lower95, Upper95)
      )

    plot_obj <- ggplot()

    if (nrow(ability_draws) > 0) {
      plot_obj <- plot_obj +
        geom_density(
          data = ability_draws,
          aes(x = Display),
          fill = "skyblue",
          alpha = 0.35,
          color = NA
        )
    }

    plot_obj <- plot_obj +
      geom_linerange(
        data = point_data,
        aes(y = label, xmin = Lower95, xmax = Upper95, color = type),
        linewidth = 0.6,
        alpha = 0.6,
        inherit.aes = FALSE
      ) +
      geom_linerange(
        data = point_data,
        aes(y = label, xmin = Lower66, xmax = Upper66, color = type),
        linewidth = 1.5,
        inherit.aes = FALSE
      ) +
      geom_point(
        data = point_data,
        aes(y = label, x = Mean, color = type, text = tooltip),
        size = 2
      ) +
      facet_grid(type ~ ., scales = "free_y", space = "free") +
      theme_minimal() +
      labs(
        title = "Wright Map (Variable Map)",
        subtitle = "Posterior means with 66% (thick) and 95% (thin) credible intervals",
        x = "Latent Scale",
        y = "",
        color = "Facet"
      ) +
      theme(
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom"
      ) +
      scale_color_brewer(palette = "Set1")

    plotly::ggplotly(plot_obj, tooltip = "text")
  })

  # Facet Comparison

  output$facetComparison <- plotly::renderPlotly({
    req(analysis())

    draws <- analysis()$ranef_draws
    if (is.null(draws) || nrow(draws) == 0) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "markers") |>
          plotly::layout(title = list(text = "No posterior draws available"))
      )
    }

    interval_df <- draws |>
      dplyr::group_by(Facet) |>
      dplyr::summarise(
        Mean = mean(Display, na.rm = TRUE),
        Lower66 = safe_quantile(Display, 0.17),
        Upper66 = safe_quantile(Display, 0.83),
        Lower95 = safe_quantile(Display, 0.025),
        Upper95 = safe_quantile(Display, 0.975),
        .groups = "drop"
      )

    gp <- ggplot(draws, aes(x = Display, y = Facet, fill = Facet)) +
      ggridges::geom_density_ridges(alpha = 0.35, color = NA, scale = 1.1) +
      geom_segment(
        data = interval_df,
        aes(x = Lower95, xend = Upper95, y = Facet, yend = Facet),
        linewidth = 0.6,
        color = "black",
        inherit.aes = FALSE
      ) +
      geom_segment(
        data = interval_df,
        aes(x = Lower66, xend = Upper66, y = Facet, yend = Facet),
        linewidth = 2,
        color = "black",
        inherit.aes = FALSE
      ) +
      geom_point(
        data = interval_df,
        aes(x = Mean, y = Facet, text = sprintf("Facet: %s<br>Mean: %.3f\n66%% CI: [%.3f, %.3f]\n95%% CI: [%.3f, %.3f]",
                                                Facet, Mean, Lower66, Upper66, Lower95, Upper95)),
        color = "black",
        size = 2,
        inherit.aes = FALSE
      ) +
      theme_minimal() +
      labs(
        title = "Posterior Ridgeline Comparison",
        subtitle = "Thick segments = 66% CrI, thin segments = 95% CrI",
        x = "Latent Scale",
        y = "Facet"
      ) +
      theme(legend.position = "none")

    plotly::ggplotly(gp, tooltip = "text")
  })

  # Threshold Table

  output$thresholdTable <- render_gt({
    req(analysis())

    thresholds <- analysis()$threshold_summary

    if (is.null(thresholds) || nrow(thresholds) == 0) {
      return(
        data.frame(Message = "No thresholds available") |>
          gt() |>
          tab_header(title = "Category Thresholds")
      )
    }

    n_cat <- analysis()$n_categories

    thresh_df <- thresholds |>
      dplyr::mutate(
        Spacing = dplyr::lead(Mean) - Mean,
        `66% CrI` = sprintf("[%0.2f, %0.2f]", Lower66, Upper66),
        `95% CrI` = sprintf("[%0.2f, %0.2f]", Lower95, Upper95)
      ) |>
      dplyr::select(Threshold, Mean, SD, Spacing, `66% CrI`, `95% CrI`)

    thresh_df |>
      gt() |>
      tab_header(
        title = "Category Thresholds",
        subtitle = paste("Posterior summaries for", n_cat, "response categories")
      ) |>
      fmt_number(columns = c(Mean, SD, Spacing), decimals = 3)
  })


  output$thresholdMap <- plotly::renderPlotly({
    req(analysis())

    thresholds <- analysis()$threshold_summary

    if (is.null(thresholds) || nrow(thresholds) == 0) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "markers") |>
          plotly::layout(title = list(text = "No thresholds available"))
      )
    }

    thresh_df <- thresholds |>
      dplyr::mutate(
        Threshold = factor(paste0("T", Threshold), levels = paste0("T", Threshold))
      )

    gp <- ggplot(thresh_df, aes(x = Mean, y = Threshold, text = sprintf("%s<br>Mean: %.3f\n66%% CI: [%.3f, %.3f]\n95%% CI: [%.3f, %.3f]",
                                                                    Threshold, Mean, Lower66, Upper66, Lower95, Upper95))) +
      geom_point(size = 4, color = "darkred") +
      geom_linerange(aes(xmin = Lower95, xmax = Upper95), linewidth = 0.6, alpha = 0.6, color = "darkred") +
      geom_linerange(aes(xmin = Lower66, xmax = Upper66), linewidth = 2, color = "darkred") +
      theme_minimal() +
      labs(title = "Threshold Locations on Latent Scale",
           subtitle = paste(nrow(thresh_df), "thresholds for", analysis()$n_categories, "categories"),
           x = "Latent Scale",
           y = "")

    plotly::ggplotly(gp, tooltip = "text")
  })

  # Probability Curves

  output$probabilityCurves <- plotly::renderPlotly({
    req(analysis())

    model <- analysis()$model
    thresholds <- analysis()$threshold_summary
    n_cat <- analysis()$n_categories

    if (is.null(thresholds) || nrow(thresholds) == 0 || n_cat < 2) {
      return(
        plotly::plotly_empty(type = "scatter", mode = "lines") |>
          plotly::layout(title = list(text = "Probability curves unavailable"))
      )
    }

    link <- tryCatch(model$family$link, error = function(e) "logit")

    inv_link <- switch(link,
      logit = plogis,
      probit = pnorm,
      cauchit = pcauchy,
      loglog = function(x) 1 - exp(-exp(x)),
      cloglog = function(x) 1 - exp(-exp(x)),
      plogis
    )

    ability_seq <- seq(-4, 4, length.out = 200)
    threshold_values <- thresholds$Mean
    n_thresh <- length(threshold_values)

    prob_list <- lapply(seq_len(n_cat), function(cat) {
      probs <- sapply(ability_seq, function(theta) {
        if (cat == 1) {
          inv_link(threshold_values[1] - theta)
        } else if (cat == n_cat) {
          1 - inv_link(threshold_values[n_thresh] - theta)
        } else {
          inv_link(threshold_values[cat] - theta) - inv_link(threshold_values[cat - 1] - theta)
        }
      })
      data.frame(
        Ability = ability_seq,
        Probability = pmax(pmin(probs, 1), 0),
        Category = factor(cat)
      )
    })

    prob_df <- dplyr::bind_rows(prob_list)

    palette <- if (n_cat <= 8) {
      RColorBrewer::brewer.pal(max(3, n_cat), "Set1")
    } else {
      grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(n_cat)
    }

    plotly::plot_ly(
      prob_df,
      x = ~Ability,
      y = ~Probability,
      color = ~Category,
      colors = palette,
      type = "scatter",
      mode = "lines",
      text = ~sprintf("Ability: %.2f<br>Probability: %.3f<br>Category: %s", Ability, Probability, Category),
      hoverinfo = "text"
    ) |>
      plotly::layout(
        title = list(text = "Category Probability Curves"),
        xaxis = list(title = "Ability (Latent Scale)"),
        yaxis = list(title = "Probability", range = c(0, 1)),
        legend = list(orientation = "h", y = -0.2),
        annotations = list(
          list(
            text = paste("Predicted category curves using", link, "link"),
            x = 0.5,
            y = 1.08,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 12)
          )
        )
      )
  })

  # Overall Statistics
  output$overallStats <- render_gt({
    req(analysis())
    
    desc <- analysis()$desc_stats$overall
    
    # Check if desc is NULL or doesn't have required fields
    if (is.null(desc) || !all(c("n", "mean", "sd", "median", "min", "max", "skew", "kurtosis") %in% names(desc))) {
      # Create a basic statistics table from the raw data with proper error handling
      response_data <- as.numeric(analysis()$data$Response)
      
      # Check if response_data is valid
      if (length(response_data) == 0 || all(is.na(response_data))) {
        # Return empty table with message if no valid data
        return(
          data.frame(
            Message = "No valid response data available for statistics"
          ) |>
            gt() |>
            tab_header(title = "Overall Descriptive Statistics")
        )
      }
      
      # Calculate statistics with NA handling
      desc <- data.frame(
        n = length(response_data[!is.na(response_data)]),
        mean = ifelse(all(is.na(response_data)), NA, mean(response_data, na.rm = TRUE)),
        sd = ifelse(all(is.na(response_data)), NA, sd(response_data, na.rm = TRUE)),
        median = ifelse(all(is.na(response_data)), NA, median(response_data, na.rm = TRUE)),
        min = ifelse(all(is.na(response_data)), NA, min(response_data, na.rm = TRUE)),
        max = ifelse(all(is.na(response_data)), NA, max(response_data, na.rm = TRUE)),
        skew = 0,  # Default value
        kurtosis = 0  # Default value
      )
    }
    
    # Replace any Inf/-Inf values with NA
    desc$min <- ifelse(is.infinite(desc$min), NA, desc$min)
    desc$max <- ifelse(is.infinite(desc$max), NA, desc$max)
    
    # Create the statistics data frame with guaranteed 8 values
    stats_df <- data.frame(
      Statistic = c("N", "Mean", "SD", "Median", "Min", "Max", "Skewness", "Kurtosis"),
      Value = c(
        ifelse(is.null(desc$n) || is.na(desc$n), 0, desc$n),
        ifelse(is.null(desc$mean) || is.na(desc$mean), 0, desc$mean),
        ifelse(is.null(desc$sd) || is.na(desc$sd), 0, desc$sd),
        ifelse(is.null(desc$median) || is.na(desc$median), 0, desc$median),
        ifelse(is.null(desc$min) || is.na(desc$min), 0, desc$min),
        ifelse(is.null(desc$max) || is.na(desc$max), 0, desc$max),
        ifelse(is.null(desc$skew) || is.na(desc$skew), 0, desc$skew),
        ifelse(is.null(desc$kurtosis) || is.na(desc$kurtosis), 0, desc$kurtosis)
      ),
      stringsAsFactors = FALSE
    )
    
    stats_df |>
      gt() |>
      tab_header(title = "Overall Descriptive Statistics") |>
      fmt_number(columns = Value, decimals = 3)
  })
  
  # Facet Statistics
  output$facetStats <- renderUI({
    req(analysis())
    
    stats_list <- list()
    desc_by_facet <- analysis()$desc_stats$by_facet
    
    if (length(desc_by_facet) == 0) {
      return(p("No facet statistics available"))
    }
    
    for (i in seq_along(desc_by_facet)) {
      facet_name <- names(desc_by_facet)[i]
      table_id <- paste0("facetStatsTable_", i)
      plot_id <- paste0("facetStatsPlot_", i)
      
      stats_list[[i]] <- tagList(
        h5(paste("Statistics by", facet_name)),
        gt_output(table_id),
        plotlyOutput(plot_id, height = "300px"),
        br()
      )
      
      local({
        my_i <- i
        my_facet <- facet_name
        my_data <- desc_by_facet[[my_facet]]
        
        output[[paste0("facetStatsTable_", my_i)]] <- render_gt({
          summary_tbl <- my_data |>
            dplyr::select(Level, N, Mean, SD, Median)

          gt_tbl <- summary_tbl |>
            gt() |>
            fmt_number(columns = c(Mean, SD, Median), decimals = 2)

          color_domain <- range(summary_tbl$Mean, na.rm = TRUE)
          if (all(is.finite(color_domain))) {
            if (diff(color_domain) == 0) {
              color_domain <- color_domain + c(-0.5, 0.5)
            }
            color_fn <- scales::col_numeric(
              palette = c("blue", "white", "red"),
              domain = color_domain
            )
            gt_tbl <- gt_tbl |>
              data_color(
                columns = Mean,
                fn = color_fn
              )
          }

          gt_tbl
        })
        
        output[[paste0("facetStatsPlot_", my_i)]] <- plotly::renderPlotly({
          gp <- ggplot(my_data, aes(x = Level, y = Mean, text = sprintf("Level: %s<br>Mean: %.2f<br>SD: %.2f", Level, Mean, SD))) +
            geom_col(fill = "steelblue", alpha = 0.7) +
            geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
            theme_minimal() +
            labs(title = paste("Mean Ratings by", my_facet),
                 x = my_facet,
                 y = "Mean Rating") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))

          plotly::ggplotly(gp, tooltip = "text")
        })
      })
    }
    
    do.call(tagList, stats_list)
  })
  
  # Download Summary Report
  output$downloadSummary <- downloadHandler(
    filename = function() {
      paste0("MFRM_Analysis_Report_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      # Check if analysis results exist
      if (is.null(analysis()) || is.null(analysis()$model)) {
        write.csv(data.frame(Message = "No analysis results available"), file)
        return()
      }
      
      # Create comprehensive CSV report
      model_info <- data.frame(
        Section = "Model Info",
        Property = c("Link Function", "Threshold Structure", "Observations", "Date"),
        Value = c(
          analysis()$model$family$link,
          analysis()$model$family$threshold,
          as.character(nrow(analysis()$data)),
          as.character(Sys.Date())
        ),
        stringsAsFactors = FALSE
      )
      
      # Reliability indices
      rel_data <- analysis()$reliability_summary
      if (!is.null(rel_data) && nrow(rel_data) > 0) {
        rel_report <- rel_data |>
          dplyr::select(Facet, dplyr::ends_with("Mean")) |>
          tidyr::pivot_longer(-Facet, names_to = "Metric", values_to = "Value") |>
          dplyr::mutate(Section = "Reliability", Property = paste(Facet, Metric, sep = ": ")) |>
          dplyr::select(Section, Property, Value)
      } else {
        rel_report <- data.frame(Section = character(), Property = character(), Value = character())
      }
      
      # Random effects summaries
      ranef_summary <- analysis()$ranef_summary
      ranef_report <- if (!is.null(ranef_summary) && nrow(ranef_summary) > 0) {
        ranef_summary |>
          dplyr::mutate(Section = paste("Facet:", Facet),
                        Property = paste(Level, "(Mean)"),
                        Value = Mean) |>
          dplyr::select(Section, Property, Value)
      } else {
        data.frame(Section = character(), Property = character(), Value = character())
      }
      
      # Thresholds
      thresholds <- analysis()$threshold_summary
      thresh_data <- if (!is.null(thresholds) && nrow(thresholds) > 0) {
        thresholds |>
          dplyr::mutate(Section = "Thresholds",
                        Property = paste0("Threshold ", Threshold),
                        Value = Mean) |>
          dplyr::select(Section, Property, Value)
      } else {
        data.frame(Section = character(), Property = character(), Value = character())
      }
      
      # Combine all data
      full_report <- bind_rows(
        model_info,
        rel_report,
        ranef_report,
        thresh_data
      )
      
      write.csv(full_report, file, row.names = FALSE, fileEncoding = "UTF-8")
    },
    contentType = "text/csv; charset=utf-8"
  )

  output$downloadReliability <- downloadHandler(
    filename = function() {
      paste0("MFRM_Reliability_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      rel_summary <- analysis()$reliability_summary
      if (is.null(rel_summary) || nrow(rel_summary) == 0) {
        write.csv(data.frame(Message = "No reliability indices available"), file, row.names = FALSE)
        return()
      }
      write.csv(rel_summary, file, row.names = FALSE, fileEncoding = "UTF-8")
    },
    contentType = "text/csv; charset=utf-8"
  )

  output$downloadFitStats <- downloadHandler(
    filename = function() {
      paste0("MFRM_Fit_Statistics_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      fit_stats <- analysis()$fit_summary
      if (is.null(fit_stats) || nrow(fit_stats) == 0) {
        write.csv(data.frame(Message = "Fit statistics unavailable"), file, row.names = FALSE)
        return()
      }
      write.csv(fit_stats, file, row.names = FALSE, fileEncoding = "UTF-8")
    },
    contentType = "text/csv; charset=utf-8"
  )

  output$downloadThresholds <- downloadHandler(
    filename = function() {
      paste0("MFRM_Thresholds_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      thresholds <- analysis()$threshold_summary
      if (is.null(thresholds) || nrow(thresholds) == 0) {
        write.csv(data.frame(Message = "No threshold parameters available"), file, row.names = FALSE)
        return()
      }
      write.csv(thresholds, file, row.names = FALSE, fileEncoding = "UTF-8")
    },
    contentType = "text/csv; charset=utf-8"
  )

  # Download Facet Parameters
  output$downloadFacetParams <- downloadHandler(
    filename = function() {
      paste0("MFRM_Facet_Parameters_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      ranef_summary <- analysis()$ranef_summary
      if (is.null(ranef_summary) || nrow(ranef_summary) == 0) {
        write.csv(data.frame(Message = "No facet parameters available"), file, row.names = FALSE)
        return()
      }

      params <- ranef_summary |>
        dplyr::select(Facet, Level, Parameter, Mean, SD, Lower66, Upper66, Lower95, Upper95)

      write.csv(params, file, row.names = FALSE, fileEncoding = "UTF-8")
    },
    contentType = "text/csv; charset=utf-8"
  )
  
  # Session Info Table
  output$sessionInfoTable <- render_gt({
    session_info <- sessionInfo()
    
    data.frame(
      Component = c("R Version", "Platform", "OS", "Date"),
      Value = c(
        paste(session_info$R.version$major, session_info$R.version$minor, sep = "."),
        session_info$platform,
        session_info$running,
        as.character(Sys.Date())
      ),
      stringsAsFactors = FALSE
    ) |>
      gt() |>
      tab_header(
        title = "Session Information"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

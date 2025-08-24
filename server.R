###############################################################################
###############################################################################
###############################################################################


## I am loading a module with user-defined functions --------------------------

source(
    file = "functions.R",
    echo = FALSE,
    encoding = "UTF-8"
)


## I am loading a module with global options ----------------------------------

source(
    file = "global.R",
    echo = FALSE,
    encoding = "UTF-8"
)


## I am loading packages ------------------------------------------------------

# List of packages to be installed and loaded
required_packages <- c(
    "shiny",
    "shinyjs",
    "DT",
    "ggplot2",
    "readxl",
    "jsonlite",
    "openai",
    "commonmark"
)

# Call the function to install and load the packages
install_and_load_packages(required_packages)


## ----------------------------------------------------------------------------

###############################################################################

## I am creating a backend of the application ---------------------------------

my_server <- function(

    input,
    output,
    session

){

    ## Register all system information variables for about tab
    register_sysinfo_outputs(output)

    ## I am introducing a counter ---------------------------------------------
    output$my_counter <- renderText({
        if(
            ! file.exists("my_counter.Rdata")
        ) {
            my_counter <- 0
        } else {
            load(file = "my_counter.Rdata")
        }

        my_counter <- my_counter + 1
        save(my_counter, file = "my_counter.Rdata" )
        paste("Count of visits: ", my_counter, sep = "")
    })


    ## logic of user of inbuilt data upload -----------------------------------
    my_data <- reactiveVal(NULL)

    builtin_file <- reactiveVal(NULL)
    uploaded_file <- reactiveVal(NULL)  # potřebuji, protože fileinput si i po reset drží název souboru


    # Add all supported built-in datasets to selectInput
    updateSelectInput(session, "builtin_dataset", choices = get_builtin_named_datasets())



    load_dataset <- function(session, fullpath, name, header_included, col_separator, use_first_col_as_rownames, col_types, builtin = TRUE, quiet = FALSE) {
      source_type_str <- paste(if (builtin) "built-in dataset" else "file", sQuote(name))

      if (!quiet && !is.null(builtin_file())) {
        toastr_info(sprintf("Built-in dataset %s was successfully unloaded.", sQuote(builtin_file())))
        builtin_file(NULL)
      }
      if (!quiet && !is.null(uploaded_file())) {
        toastr_info(sprintf("File %s was successfully unloaded.", sQuote(uploaded_file())))
        uploaded_file(NULL)
      }

      if (!file.exists(fullpath)) {
        msg_error <- sprintf("The %s not found!", source_type_str)
        toastr_error(msg_error)
        showNotification(msg_error, type = "error")

        session$sendCustomMessage("setNavbarItemsState", FALSE)
        return(NULL)
      }

      # enable all data settings (checkboxes and radiobuttons)
      enable(selector = "div#data_options")
      enable(selector = "input[name='col_separator']")

      tryCatch({
        file_ext <- tools::file_ext(fullpath)

        if (file_ext %in% c("xls", "xlsx")) {
          # XLS/XLSX data
          disable(selector = "input[name='col_separator']")

          sheet_names <- readxl::excel_sheets(fullpath)
          if (!quiet && length(sheet_names) > 1) {
            msg_multiple_sheets <- sprintf(
              "Multiple sheets detected (%d) in the %s file. Due to application limitations, only the first sheet will be loaded.",
              length(sheet_names), file_ext
            )

            toastr_info(msg_multiple_sheets)
            #showNotification(msg_multiple_sheets, type = "message")
          }

          data <- readxl::read_excel(fullpath, col_names = header_included)

        } else if (file_ext %in% c("json")) {
          # JSON data (newline-delimited)
          #disable(selector = "div#data_options")
          disable(selector = "#data_options input#header")
          disable(selector = "input[name='col_separator']")

          json_file <- file(fullpath, "r")
          data <- jsonlite::stream_in(json_file, verbose = FALSE)
          close(json_file)

          # Additional type conversion: attempt to automatically cast character columns
          # to more appropriate types (numeric, integer, logical, etc.)
          data[] <- lapply(data, function(col) type.convert(col, as.is = TRUE))

        } else {
          # CSV/TXT/TSV
          data <- read.csv(fullpath, header = header_included, sep = col_separator)
        }

        if (nrow(data) <= 1) {
          toastr_warning(sprintf("The %s appears to be empty or contains only header data.", source_type_str))
        }

        if (use_first_col_as_rownames && ncol(data) > 1) {
          rownames(data) <- data[[1]]
          data <- data[, -1, drop = FALSE]
        }

        num_missings <- sum(is.na(data))
        if (num_missings > 0) {
          if (!quiet) toastr_warning(sprintf("The %s contains %s missing value(s).", source_type_str, num_missings))
          # enablovat volbu pro řešení missingů
        } else {
          # disablovat volbu pro řešení missingů
        }

        if (!quiet) toastr_success(sprintf("The %s loaded successfully!", source_type_str))
        session$sendCustomMessage("setNavbarItemsState", TRUE)

        # vše OK, uložíme si název souboru pro případný upload
        if (builtin) {
            builtin_file(name)
        } else {
            uploaded_file(name)
        }

        # buď první načtení dat, nebo změna, chci odstranit odpověď od AI (pokud už byla)
        output$ai_response <- renderUI({
          NULL
        })

        return(data)

      }, error = function(e) {
        msg_error <- sprintf("Error reading %s %s.", source_type_str, e$message)
        toastr_error(paste(msg_error))
        showNotification(msg_error, type = "error")
        session$sendCustomMessage("setNavbarItemsState", FALSE)

        return(NULL)
      })
    }


    # Reagujeme na nahrání souboru
    observeEvent(input$file_upload, ignoreInit = TRUE, {
      req(input$file_upload)

      if (input$use_builtin) {
        # odškrtneme případný nahraný vestavěný dataset
        updateCheckboxInput(session, "use_builtin", value = FALSE)
      }

      data <- load_dataset(
        session = session,
        fullpath = input$file_upload$datapath,
        name = input$file_upload$name,
        header_included = input$header,
        col_separator = input$col_separator,
        use_first_col_as_rownames = input$use_first_col_as_rownames,
        col_types = input$col_types,
        builtin = FALSE,
        quiet = FALSE
      )

      my_data(data)
    })


    # Reagujeme na vybrání vestavěného datasetu
    observeEvent(list(input$use_builtin, input$builtin_dataset), ignoreInit = TRUE, {
      if (input$use_builtin) {
        if (!is.null(uploaded_file())) {
          reset("file_upload")
        }

        data <- load_dataset(
          session = session,
          fullpath = file.path(SAMPLE_DATASETS_DIR, paste(input$builtin_dataset)),
          name = input$builtin_dataset,
          header_included = input$header,
          col_separator = input$col_separator,
          use_first_col_as_rownames = input$use_first_col_as_rownames,
          col_types = input$col_types,
          builtin = TRUE,
          quiet = FALSE
        )

        my_data(data)

      } else {
        # odškrtnutí vestavěného datasetu
        if (!is.null(builtin_file())) {
          toastr_info(paste("Built-in dataset", sQuote(builtin_file()), "was successfully unloaded."))
          session$sendCustomMessage("setNavbarItemsState", FALSE)
          builtin_file(NULL)
          my_data(NULL)
        }
      }
    })


    # Reagujeme na změnu voleb checkboxů a radiobuttonů
    observeEvent(list(input$header, input$use_first_col_as_rownames, input$col_separator, input$col_types), ignoreInit = TRUE, {
        req(my_data())  # pokud už máme nějaká data, budeme dělat jen tichý reload s jinými parametry

        if (input$use_builtin) {
          name = input$builtin_dataset
          header_included = input$header
        } else {
          name = input$file_upload$name
          header_included = input$header
        }

        data <- load_dataset(
          session = session,
          fullpath = file.path(SAMPLE_DATASETS_DIR, paste(input$builtin_dataset)),
          name = name,
          header_included = header_included,
          col_separator = input$col_separator,
          use_first_col_as_rownames = input$use_first_col_as_rownames,
          col_types = input$col_types,
          builtin = FALSE,
          quiet = TRUE
        )

        my_data(data)
    })




    # render Data Table only if dataset exists --------------------------------

    output$data_table <- renderDT({
        req(my_data())

        # Nahradíme NA hodnoty řetězcem "N/A"
        data_display <- my_data()
        data_display[is.na(data_display)] <- "N/A"

        datatable(
            data_display,
            options = list(
                pageLength = 10,
                lengthMenu = c(10, 25, 50, 100, 250),
                autoWidth = TRUE,
                searchHighlight = TRUE,
                dom = 'Blfrtip',
                rowCallback = JS("highlightMissingCells")
            )
        )
    })




    # AI insight (experimental) -----------------------------------------------
    observeEvent(input$my_navbar_page_set, ignoreInit = TRUE, {
      req(my_data())

      # pokud nejsme na AI Insight tab, ukončíme
      if (input$my_navbar_page_set != "tab_ai_insight") return()

      data_df <- my_data()
      sample_rows_to_ai <- 25

      prompt <- paste0(
        "Analyze this dataset and provide your response in structured sections. ",
        "Use Markdown formatting with headings (## Section Title) for each major topic, and bullet points where appropriate. ",
        "When referring to column names, always format them as inline code using backticks in Markdown (e.g., `column_name`).",
        "Include sections such as: Overview, Key Statistics, Missing Values, Suggested Actions, Exploration Ideas.\n\n",
        "Dataset summary (only first ", sample_rows_to_ai, " rows):\n",
        "Columns: ", paste(colnames(data_df), collapse = ","), "\n",
        "Sample rows:\n", paste(capture.output(head(data_df, sample_rows_to_ai)), collapse = "\n"), "\n"
      )

      # vypíše celý prompt do konzole (kontrola/debug)
      cat(prompt)

      # volání OpenAI API synchronně
      response <- tryCatch({
        create_chat_completion(
          model = "gpt-4",
          messages = list(
            list(role = "system", content = "You are a helpful assistant for data analysis."),
            list(role = "user", content = prompt)
          )
        )
      }, error = function(e) {
        list(error = if (!is.null(e$message) && nchar(e$message) > 0) e$message
                   else "Something went wrong, no details...")
      })

      md_text <- NULL
      error_msg <- paste0(
        "<p class='text-danger'><strong>We couldn't process your request right now. ",
        "Please try again later or contact the application developer.</strong></p>\n\n"
      )

      if (!is.null(response$error)) {
        # chyba při volání API
        md_text <- paste0(
          error_msg,
          "**Technical details (for debugging):**\n\n",
          "```text\n", response$error, "\n```"
        )
      } else if (is.list(response) && "choices" %in% names(response) &&
                 is.data.frame(response$choices) &&
                 "message.content" %in% colnames(response$choices)) {
        # úspěšná odpověď
        md_text <- response$choices$`message.content`[1]
      } else {
        # neočekávaný formát
        md_text <- paste0(
          error_msg,
          "**Technical details (for debugging):**\n\n",
          "```text\nUnexpected response format\n```"
        )
      }

      # markdown konvertuji a vrátím HTML
      output$ai_response <- renderUI({
        HTML(commonmark::markdown_html(md_text))
      })
    })




    # nikde se nevyužívá, volá se jen s prvním načtením dat (ne při změně datasetu)
    observeEvent(my_data(), {
      is_data_valid <- !is.null(my_data()) && nrow(my_data()) > 0 && ncol(my_data()) > 0
      #toastr_info(paste(!is.null(my_data())))

    })


    # reactive function to get upload summary ---------------------------------

    upload_summary <- reactive({

        req(my_data())
        data <- my_data()

        col_info <- data.frame(
            "variable" = colnames(data),
            "data type" = sapply(data, function(col) class(col)[1]),
            stringsAsFactors = FALSE
        )

        col_info

    })

    # render upload summary table ---------------------------------------------

    output$upload_summary_table <- renderDT({
        req(upload_summary())

        datatable(
            upload_summary(),
            escape = FALSE,
            options = list(
                pageLength = 10,
                autoWidth = TRUE,
                dom = 'Bfrtip',
                ordering = FALSE,
                stateSave = TRUE,  # Save the table state, but for visible rows only
                columnDefs = list(list(
                    targets = 1,
                    render = JS(sprintf(
                        "function(data, type, row, meta) {
                           var options = ['%s'];
                           return renderDataTypeSelector(data, type, row, options);
                         }",
                        paste(DATA_TYPE_OPTIONS, collapse = "','")
                    ))
                ))
            ),
            rownames = FALSE,
            colnames = c("Variable", "Data type"),
            selection = "none"
        )
    }, server = FALSE)


    # dynamically show "data preview" label only if a dataset is available ----

    output$data_preview_label <- renderUI({

        req(my_data())
        h2("Data preview")

    })

    output$upload_summary_label <- renderUI({

        req(upload_summary())
        h2("Variable data types")

    })

    output$plot_section_label <- renderUI({

        req(my_data())
        h2("Variable distributions")

    })


    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------

    ## Reusable helper for a nice banner
    intro_box <- function(title, lead, bullets = NULL, note = NULL) {
      tags$div(
        class = "intro-box",
        tags$div(class = "intro-title", title),
        tags$p(class = "intro-lead", lead),
        if (!is.null(bullets)) tags$ul(lapply(bullets, function(b) tags$li(b))),
        if (!is.null(note)) tags$p(class = "intro-note", note)
      )
    }

    ## Two-sample t-test
    output$ttest_intro <- renderUI({
      intro_box(
        HTML("Two-sample <em>t</em>-test (independent groups)"),
        "Tests whether two independent group means differ (H₀: mean difference = μ, typically 0). Uses Welch’s t by default (no equal-variance assumption).",
        bullets = c(
          "Use for a numeric outcome and a 2-level grouping variable.",
          "Assumptions: independent observations; roughly normal within groups (or large n).",
          "Outputs here include: CI for mean diff, Cohen’s d, Shapiro–Wilk per group, box/violin/density plots, and a one-line interpretation."
        )
      )
    })

    ## Paired t-test
    output$ptt_intro <- renderUI({
      intro_box(
        HTML("Paired <em>t</em>-test (within-subject / matched)"),
        "Tests whether the mean of within-pair differences equals μ (typically 0). This is for repeated measures on the same units.",
        bullets = c(
          "Use when you have two numeric measurements per subject (before/after, left/right, etc.).",
          "Assumption: differences are approximately normal; pairs are independent.",
          "Outputs: CI for mean diff, Cohen’s dz, Shapiro–Wilk on differences, boxplots (incl. differences), violin/density, and interpretation."
        )
      )
    })

    ## Mann–Whitney (Wilcoxon rank-sum)
    output$mw_intro <- renderUI({
      intro_box(
        "Mann–Whitney U (Wilcoxon rank-sum)",
        "Nonparametric alternative to the two-sample t-test; assesses whether values in one group tend to be larger than the other.",
        bullets = c(
          "Use for two independent groups when normality is doubtful or you prefer rank-based inference.",
          "Assumptions: independent samples; similar distribution shapes if interpreting as a location shift.",
          "Outputs: Hodges–Lehmann location shift + CI, Cliff’s δ effect size, Shapiro per group (optional), and plots."
        ),
        note = "Tip: If shapes differ strongly, interpret results as stochastic dominance rather than a pure median shift."
      )
    })

    ## Paired Wilcoxon (signed-rank)
    output$pwx_intro <- renderUI({
      intro_box(
        "Paired Wilcoxon signed-rank",
        "Nonparametric paired test; checks if the median of within-pair differences equals μ (typically 0).",
        bullets = c(
          "Use when you have paired/repeated measures and want a rank-based alternative to the paired t-test.",
          "Assumptions: paired differences are symmetrically distributed around the median; ignores zero differences.",
          "Outputs: Hodges–Lehmann shift + CI, matched-pairs rank-biserial effect size, Shapiro on differences (diagnostic), and plots."
        )
      )
    })

    ## One-way ANOVA
    output$anova_intro <- renderUI({
      intro_box(
        "One-way ANOVA",
        "Tests whether at least one group mean differs among 2+ groups using an F-test on between- vs within-group variability.",
        bullets = c(
          "Use for a numeric outcome and a factor with 2 or more levels (typically 3+).",
          "Assumptions: independent observations; approximately normal residuals; homogeneous variances across groups.",
          "Outputs: ANOVA table, group means + CIs, residual & per-group Shapiro, Bartlett/Levene, η² effect size, Tukey HSD (≥3 groups), and plots."
        ),
        note = "If assumptions look poor (e.g., strong non-normality or heteroscedasticity), consider transformations or a nonparametric alternative (e.g., Kruskal–Wallis)."
      )
    })


    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------

    ## ======================= Summary Plots tab (server) =======================

    # --- Intro banner -----------------------------------------------------------
    output$summary_intro <- renderUI({
      intro_box(
        "Summary plots & descriptive analytics",
        "A quick visual overview of your data.",
        bullets = c(
          "Histograms to see distribution shape for numeric/date variables.",
          "Barplots to see counts of each category for categorical variables.",
          "Below the plots, tables summarize basic statistics by variable type."
        ),
        note = "Tip: Use these visuals to decide which inferential method fits best (e.g., t-tests/ANOVA vs. rank-based tests)."
      )
    })

    # --- Helpers ----------------------------------------------------------------
    safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", x)

    top_levels_str <- function(x, k = 3) {
      x <- x[!is.na(x)]
      if (!length(x)) return(NA_character_)
      tb  <- sort(table(x), decreasing = TRUE)
      tb  <- head(tb, k)
      pct <- round(100 * as.numeric(tb) / sum(tb), 1)
      paste0(names(tb), " (", pct, "%)", collapse = ", ")
    }

    is_date_like <- function(z) inherits(z, "Date") || inherits(z, "POSIXct") || inherits(z, "POSIXt")

    # --- Variable types (friendly) ---------------------------------------------
    variable_types <- reactive({
      req(my_data())
      df <- my_data()
      vtype <- vapply(df, function(col) {
        if (is.numeric(col)) "numeric"
        else if (is_date_like(col)) "date"
        else if (is.factor(col) || is.character(col) || is.logical(col)) "categorical"
        else class(col)[1]
      }, character(1))
      data.frame(Variable = names(df), Type = unname(vtype), stringsAsFactors = FALSE)
    })

    # --- Numeric stats table ----------------------------------------------------
    numeric_stats <- reactive({
      req(my_data())
      df <- my_data()
      keep <- sapply(df, is.numeric)
      if (!any(keep)) return(NULL)
      nums <- df[, keep, drop = FALSE]
      rows <- lapply(names(nums), function(nm) {
        x <- nums[[nm]]
        n <- length(x); nn <- sum(is.finite(x)); miss <- n - nn
        x2 <- x[is.finite(x)]
        data.frame(
          Variable   = nm,
          n          = n,
          Missing    = miss,
          NonMissing = nn,
          Mean       = if (nn) round(mean(x2), 3) else NA_real_,
          Median     = if (nn) round(stats::median(x2), 3) else NA_real_,
          SD         = if (nn > 1) round(stats::sd(x2), 3) else NA_real_,
          Min        = if (nn) round(min(x2), 3) else NA_real_,
          Max        = if (nn) round(max(x2), 3) else NA_real_,
          check.names = FALSE
        )
      })
      do.call(rbind, rows)
    })

    output$summary_numeric_block <- renderUI({
      tbl <- numeric_stats()
      if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
      tagList(
        tags$h2("Numeric variables – basic statistics"),
        div(class = "centered-table", tableOutput("summary_stats_numeric")),
        div(class = "dl-row no-print", downloadButton("download_stats_numeric", "Download numeric stats"))
      )
    })
    output$summary_stats_numeric <- renderTable({
      tbl <- numeric_stats()
      req(!is.null(tbl), nrow(tbl) > 0, cancelOutput = TRUE)
      tbl
    }, rownames = FALSE)
    output$download_stats_numeric <- downloadHandler(
      filename = function() sprintf("summary_numeric_%s.csv", Sys.Date()),
      content  = function(file) utils::write.csv(numeric_stats(), file, row.names = FALSE)
    )

    # --- Categorical stats table ------------------------------------------------
    categorical_stats <- reactive({
      req(my_data())
      df <- my_data()
      keep <- sapply(df, function(z) is.factor(z) || is.character(z) || is.logical(z))
      if (!any(keep)) return(NULL)
      cats <- df[, keep, drop = FALSE]
      rows <- lapply(names(cats), function(nm) {
        x <- cats[[nm]]
        n <- length(x); miss <- sum(is.na(x)); x2 <- x[!is.na(x)]
        tb <- sort(table(x2), decreasing = TRUE)
        mode_name <- if (length(tb)) names(tb)[1] else NA_character_
        mode_n    <- if (length(tb)) as.integer(tb[1]) else NA_integer_
        mode_pct  <- if (length(tb)) round(100 * mode_n / length(x2), 1) else NA_real_
        data.frame(
          Variable          = nm,
          n                 = n,
          Missing           = miss,
          NonMissing        = n - miss,
          `Distinct levels` = length(tb),
          `Most common`     = mode_name,
          `Count (most)`    = mode_n,
          `Percent (most)`  = mode_pct,
          `Top levels`      = top_levels_str(x2, k = 3),
          check.names = FALSE
        )
      })
      do.call(rbind, rows)
    })

    output$summary_categorical_block <- renderUI({
      tbl <- categorical_stats()
      if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
      tagList(
        tags$h2("Categorical variables – counts & levels"),
        div(class = "centered-table", tableOutput("summary_stats_categorical")),
        div(class = "dl-row no-print", downloadButton("download_stats_categorical", "Download categorical stats"))
      )
    })
    output$summary_stats_categorical <- renderTable({
      tbl <- categorical_stats()
      req(!is.null(tbl), nrow(tbl) > 0, cancelOutput = TRUE)
      tbl
    }, rownames = FALSE)
    output$download_stats_categorical <- downloadHandler(
      filename = function() sprintf("summary_categorical_%s.csv", Sys.Date()),
      content  = function(file) utils::write.csv(categorical_stats(), file, row.names = FALSE)
    )

    # --- Date stats table -------------------------------------------------------
    date_stats <- reactive({
      req(my_data())
      df <- my_data()
      keep <- sapply(df, is_date_like)
      if (!any(keep)) return(NULL)
      dts <- df[, keep, drop = FALSE]
      rows <- lapply(names(dts), function(nm) {
        x <- dts[[nm]]
        n <- length(x); miss <- sum(is.na(x)); x2 <- x[!is.na(x)]
        data.frame(
          Variable   = nm,
          n          = n,
          Missing    = miss,
          NonMissing = n - miss,
          Min        = if (length(x2)) as.character(min(x2)) else NA_character_,
          Median     = if (length(x2)) as.character(stats::median(x2)) else NA_character_,
          Max        = if (length(x2)) as.character(max(x2)) else NA_character_,
          check.names = FALSE
        )
      })
      do.call(rbind, rows)
    })

    output$summary_date_block <- renderUI({
      tbl <- date_stats()
      if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
      tagList(
        tags$h2("Date/time variables – range"),
        div(class = "centered-table", tableOutput("summary_stats_dates")),
        div(class = "dl-row no-print", downloadButton("download_stats_dates", "Download date/time stats"))
      )
    })
    output$summary_stats_dates <- renderTable({
      tbl <- date_stats()
      req(!is.null(tbl), nrow(tbl) > 0, cancelOutput = TRUE)
      tbl
    }, rownames = FALSE)
    output$download_stats_dates <- downloadHandler(
      filename = function() sprintf("summary_dates_%s.csv", Sys.Date()),
      content  = function(file) utils::write.csv(date_stats(), file, row.names = FALSE)
    )

    # Generate plots and download buttons for each variable dynamically -------

    # --- Dynamic per-variable plots + downloads ---------------------------------
    output$variable_plots <- renderUI({
      req(my_data())
      vt <- variable_types()
      if (nrow(vt) == 0) return(NULL)

      plot_list <- lapply(seq_len(nrow(vt)), function(i) {
        var_name <- vt$Variable[i]
        var_type <- vt$Type[i]
        sid <- safe_id(var_name)
        tagList(
          div(
            h3(var_name),
            plotOutput(outputId = paste0("plot_", sid), height = "300px"),
            downloadButton(paste0("download_", sid), "Download Plot", class = "btn no-print")
          )
        )
      })
      do.call(tagList, plot_list)
    })

    observe({
      req(my_data())
      df <- my_data()
      vt <- variable_types()
      if (nrow(vt) == 0) return()

      for (i in seq_len(nrow(vt))) {
        local({
          var_name <- vt$Variable[i]
          var_type <- vt$Type[i]
          sid <- safe_id(var_name)
          plot_output_id <- paste0("plot_", sid)
          download_button_id <- paste0("download_", sid)

          output[[plot_output_id]] <- renderPlot({
            req(df[[var_name]], cancelOutput = TRUE)

            # Numeric/date histogram
            if (var_type %in% c("numeric", "date")) {
              x <- df[[var_name]]
              if (is_date_like(x)) {
                # Draw histogram on numeric representation (days since epoch)
                x_num <- as.numeric(as.Date(x))
                hist(
                  x_num,
                  col = "lightblue", border = "black",
                  main = paste("Histogram of", var_name),
                  xlab = paste0(var_name, " (date as days)"),
                  ylab = "Frequency"
                )
                axis(1, at = pretty(x_num), labels = as.character(as.Date(pretty(x_num))), las = 2, cex.axis = 0.8)
              } else {
                hist(
                  x,
                  col = "lightblue", border = "black",
                  main = paste("Histogram of", var_name),
                  xlab = var_name, ylab = "Frequency"
                )
              }

              # Categorical barplot
            } else if (var_type == "categorical") {
              x <- df[[var_name]]
              counts <- sort(table(x), decreasing = TRUE)
              barplot(
                counts,
                col = "orange", border = "black",
                main = paste("Barplot of", var_name),
                xlab = var_name, ylab = "Count",
                las = 2, cex.names = 0.8
              )

              # Fallback: try numeric
            } else {
              x <- df[[var_name]]
              if (is.numeric(x)) {
                hist(x, col = "lightblue", border = "black",
                     main = paste("Histogram of", var_name),
                     xlab = var_name, ylab = "Frequency")
              }
            }
          })

          output[[download_button_id]] <- downloadHandler(
            filename = function() paste0("plot_", sid, ".png"),
            content = function(file) {
              png(file, width = 1000, height = 700, res = 150, bg = "white")
              on.exit(dev.off(), add = TRUE)

              if (var_type %in% c("numeric", "date")) {
                x <- df[[var_name]]
                if (is_date_like(x)) {
                  x_num <- as.numeric(as.Date(x))
                  hist(
                    x_num,
                    col = "lightblue", border = "black",
                    main = paste("Histogram of", var_name),
                    xlab = paste0(var_name, " (date as days)"),
                    ylab = "Frequency"
                  )
                  axis(1, at = pretty(x_num), labels = as.character(as.Date(pretty(x_num))), las = 2, cex.axis = 0.7)
                } else {
                  hist(
                    x,
                    col = "lightblue", border = "black",
                    main = paste("Histogram of", var_name),
                    xlab = var_name, ylab = "Frequency"
                  )
                }
              } else if (var_type == "categorical") {
                x <- df[[var_name]]
                counts <- sort(table(x), decreasing = TRUE)
                barplot(
                  counts,
                  col = "orange", border = "black",
                  main = paste("Barplot of", var_name),
                  xlab = var_name, ylab = "Count",
                  las = 2, cex.names = 0.8
                )
              } else {
                x <- df[[var_name]]
                if (is.numeric(x)) {
                  hist(x, col = "lightblue", border = "black",
                       main = paste("Histogram of", var_name),
                       xlab = var_name, ylab = "Frequency")
                }
              }
            }
          )
        })
      }
    })


    ## ==========================================================================

    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------

    ## logic of the two-sample t-test -----------------------------------------

    output$data_ready <- reactive({
        !is.null(my_data()) && nrow(my_data()) > 0 && ncol(my_data()) > 0
    })
    outputOptions(output, "data_ready", suspendWhenHidden = FALSE)

    observe({
        df <- my_data()
        req(df)
        shiny::validate(need(ncol(df) > 0, "No columns in uploaded data."))

        isolate({
            numeric_vars <- names(df)[sapply(df, is.numeric)]
            updateSelectInput(
                session,
                "ttest_num_var",
                choices = numeric_vars
            )

            group_var_candidates <- names(df)
            updateSelectInput(
                session, "ttest_group_var",
                choices = group_var_candidates
            )
        })
    })

    output$ttest_group_levels_ui <- renderUI({
        df <- my_data()
        req(df)
        req(input$ttest_group_var)

        group_raw <- df[[input$ttest_group_var]]
        group <- try(as.factor(group_raw), silent = TRUE)
        shiny::validate(
            need(
                !inherits(group, "try-error"),
                "Cannot coerce grouping variable to factor."
            ),
            need(
                nlevels(group) >= 2,
                "Grouping variable must have at least 2 levels."
            )
        )

        selectInput(
            "ttest_selected_levels",
            "Select two groups to compare:",
            choices = levels(group),
            selected = levels(group)[1:2],
            multiple = TRUE
        )
    })

    ttest_result <- reactive({
        df <- my_data()
        req(df)
        req(
            input$ttest_num_var,
            input$ttest_group_var,
            input$ttest_selected_levels
        )
        shiny::validate(
            need(
                length(input$ttest_selected_levels) == 2,
                "Select exactly two groups."
            )
        )

        num_var_name <- isolate(input$ttest_num_var)
        group_var_name <- isolate(input$ttest_group_var)

        shiny::validate(
            need(
                num_var_name %in% names(df),
                "Numeric variable not found."
            ),
            need(
                group_var_name %in% names(df),
                "Grouping variable not found."
            )
        )

        y <- df[[num_var_name]]
        group_raw <- df[[group_var_name]]

        # checks
        shiny::validate(
            need(
                is.numeric(y),
                "Selected outcome variable must be numeric."
            )#,
            # need(
                # !is.numeric(group_raw),
                # "Grouping variable should not be numeric."
            # )   # allow characters, factors, logicals
        )

        group <- as.factor(group_raw)

        # Subset only the selected two groups
        subset_idx <- group %in% input$ttest_selected_levels
        y <- y[subset_idx]
        group <- factor(
            group[subset_idx],
            levels = input$ttest_selected_levels
        )   # set correct order

        shiny::validate(
            need(nlevels(group) == 2,
            "After subsetting, must have exactly 2 groups.")
        )

        # get selected group names
        group_names <- levels(group)

        # local data frame for t-test
        local_df <- data.frame(y = y, group = group)

        test <- t.test(
            y ~ group,
            data = local_df,
            alternative = input$ttest_alt,
            mu = input$ttest_mu,
            var.equal = FALSE
        )

        group_levels <- levels(group)

        df_out <- data.frame(
            "statistic" = round(test$statistic, 3),
            "degrees of freedom" = round(test$parameter, 2),
            "n_group1" = sum(group == group_names[1], na.rm = TRUE),
            "n_group2" = sum(group == group_names[2], na.rm = TRUE),
            "p-value" = signif(test$p.value, 4),
            "confidence interval" = paste(
                "(",
                round(test$conf.int[1], 3),
                ", ",
                round(test$conf.int[2], 3),
                ")",
                sep = ""
            ),
            "mean_group1" = round(
                mean(y[group == group_names[1]], na.rm = TRUE),
                3
            ),
            "mean_group2" = round(
                mean(y[group == group_names[2]], na.rm = TRUE),
                3
            ),
            check.names = FALSE
        )

        names(df_out)[3:4] <- paste0("n (", group_names, ")")
        names(df_out)[7:8] <- paste0("mean (", group_names, ")")

        df_out

    })

    output$ttest_result <- renderTable({
        tryCatch({
            ttest_result()
        }, error = function(e) {
            NULL
        })
    })

    output$ttest_boxplot <- renderPlot({
        tryCatch(
            {
                df <- my_data()
                req(df)
                req(
                    input$ttest_num_var,
                    input$ttest_group_var,
                    input$ttest_selected_levels
                )
                shiny::validate(
                    need(
                        length(input$ttest_selected_levels) == 2,
                        "Select exactly two groups."
                    )
                )

                y <- df[[input$ttest_num_var]]
                group_raw <- df[[input$ttest_group_var]]

                group <- as.factor(group_raw)
                subset_idx <- group %in% input$ttest_selected_levels
                y <- y[subset_idx]
                group <- group[subset_idx]

                # set the factor levels explicitly according to the user selection order
                group <- factor(group, levels = input$ttest_selected_levels)

                # salculate sample sizes
                group_counts <- table(group)
                group_labels <- paste(
                    names(group_counts),
                    " (n = ",
                    as.numeric(group_counts),
                    ")",
                    sep = ""
                )

                boxplot(
                    y ~ group,
                    main = "Boxplot of Selected Groups",
                    xlab = input$ttest_group_var,
                    ylab = input$ttest_num_var,
                    names = group_labels,
                    col = "lightblue",
                    border = "darkblue"
                )
            },
            error = function(e) {
                # do nothing (catch the error silently)
                NULL
            }
        )
    })

    output$ttest_h0_statement <- renderUI({
        req(
            input$ttest_num_var,
            input$ttest_group_var,
            input$ttest_selected_levels
        )
        shiny::validate(
            need(
                length(input$ttest_selected_levels) == 2,
                "Select two groups."
            )
        )

        group1 <- input$ttest_selected_levels[1]
        group2 <- input$ttest_selected_levels[2]
        var <- input$ttest_num_var
        mu <- input$ttest_mu

        h0 <- paste(
            "<b>Null Hypothesis</b> (H<sub>0</sub>): The mean of '",
            var,
            "' for group '",
            group1,
            "' minus group '",
            group2,
            "' equals ",
            mu,
            ".",
            sep = ""
        )

        # Based on selected alternative
        h1 <- switch(input$ttest_alt,
            "two.sided" = paste(
                "<b>Alternative Hypothesis</b> (H<sub>1</sub>): The mean difference is not equal to ",
                mu,
                ".",
                sep = ""
            ),
            "less" = paste(
                "<b>Alternative Hypothesis</b> (H<sub>1</sub>): The mean of '",
                var,
                "' for group '",
                group1,
                "' is less than that for group '",
                group2,
                "'.",
                sep = ""
            ),
            "greater" = paste(
                "<b>Alternative Hypothesis</b> (H<sub>1</sub>): The mean of '",
                var,
                "' for group '",
                group1,
                "' is greater than that for group '",
                group2,
                "'.",
                sep = ""
            )
        )

        HTML(paste(h0, "<br>", h1, "<br><br>", sep = ""))

    })

    ## --- helpers ---------------------------------------------------------------

    mean_ci <- function(x, conf.level = 0.95) {
      x <- x[is.finite(x)]
      n <- length(x)
      m <- mean(x)
      s <- stats::sd(x)
      se <- s / sqrt(n)
      alpha <- 1 - conf.level
      tcrit <- stats::qt(1 - alpha/2, df = n - 1)
      c(mean = m, lwr = m - tcrit * se, upr = m + tcrit * se, n = n)
    }

    cohens_d_pooled <- function(x, g, order = NULL) {
      # x numeric, g factor with 2 levels
      stopifnot(length(levels(g)) == 2)
      if (!is.null(order)) g <- factor(g, levels = order)
      lvs <- levels(g)
      x1 <- x[g == lvs[1]]
      x2 <- x[g == lvs[2]]
      n1 <- sum(is.finite(x1)); n2 <- sum(is.finite(x2))
      m1 <- mean(x1, na.rm = TRUE); m2 <- mean(x2, na.rm = TRUE)
      s1 <- stats::sd(x1, na.rm = TRUE); s2 <- stats::sd(x2, na.rm = TRUE)
      s_pooled <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
      d <- (m1 - m2) / s_pooled
      list(d = d, n1 = n1, n2 = n2, s_pooled = s_pooled, levels = lvs, m1 = m1, m2 = m2)
    }

    describe_d <- function(d) {
      ad <- abs(d)
      if (is.na(ad)) return("unknown magnitude")
      if (ad < 0.2) "negligible"
      else if (ad < 0.5) "small"
      else if (ad < 0.8) "medium"
      else "large"
    }

    ## --- common reactive to reuse cleaned data ---------------------------------

    ttest_local <- reactive({
      df <- my_data()
      req(df, input$ttest_num_var, input$ttest_group_var, input$ttest_selected_levels)
      shiny::validate(need(length(input$ttest_selected_levels) == 2, "Select exactly two groups."))
      y <- df[[input$ttest_num_var]]
      g_raw <- df[[input$ttest_group_var]]
      shiny::validate(need(is.numeric(y), "Selected outcome variable must be numeric."))
      g <- as.factor(g_raw)
      idx <- g %in% input$ttest_selected_levels
      y <- y[idx]
      g <- factor(g[idx], levels = input$ttest_selected_levels) # respect user order
      shiny::validate(need(nlevels(g) == 2, "After subsetting, must have exactly 2 groups."))
      data.frame(y = y, group = g)
    })

    # Labels that show only when data exists
    output$ttest_label_means_ci <- renderUI({
      df_loc <- try(ttest_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 0)
      tags$h2("Confidence intervals for group means")
    })

    output$ttest_label_shapiro <- renderUI({
      df_loc <- try(ttest_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 2)  # need at least 3 per group for Shapiro
      tags$h2("Normality check (Shapiro–Wilk test)")
    })

    output$ttest_label_effectsize <- renderUI({
      df_loc <- try(ttest_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 1)
      tags$h2("Effect size (Cohen's d)")
    })

    output$ttest_label_interpretation <- renderUI({
      df_loc <- try(ttest_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 1)
      tags$h2("Interpretation")
    })


    ## --- confidence intervals for group means ----------------------------------

    output$ttest_means_ci <- renderTable({
      tryCatch({
        df_loc <- ttest_local()
        lvs <- levels(df_loc$group)

        ci1 <- mean_ci(df_loc$y[df_loc$group == lvs[1]])
        ci2 <- mean_ci(df_loc$y[df_loc$group == lvs[2]])

        out <- data.frame(
          Group = lvs,
          n = c(ci1["n"], ci2["n"]),
          Mean = round(c(ci1["mean"], ci2["mean"]), 3),
          `95% CI` = c(
            sprintf("(%0.3f, %0.3f)", ci1["lwr"], ci1["upr"]),
            sprintf("(%0.3f, %0.3f)", ci2["lwr"], ci2["upr"])
          ),
          check.names = FALSE
        )
        out
      }, error = function(e) NULL)
    })

    ## --- Shapiro–Wilk normality per group --------------------------------------

    output$ttest_shapiro <- renderTable({
      tryCatch({
        df_loc <- ttest_local()
        lvs <- levels(df_loc$group)
        res <- lapply(lvs, function(L) {
          x <- df_loc$y[df_loc$group == L]
          if (sum(is.finite(x)) < 3) {
            c(W = NA_real_, `p-value` = NA_real_, Note = "Needs at least 3 observations")
          } else if (sum(is.finite(x)) > 5000) {
            # Shapiro is defined for n <= 5000
            c(W = NA_real_, `p-value` = NA_real_, Note = "n > 5000; test not computed")
          } else {
            s <- stats::shapiro.test(x)
            c(W = unname(round(s$statistic, 3)), `p-value` = signif(s$p.value, 4), Note = "")
          }
        })
        df <- do.call(rbind, res)
        data.frame(Group = lvs, df, check.names = FALSE, row.names = NULL)
      }, error = function(e) NULL)
    })

    ## --- Cohen's d -------------------------------------------------------------

    output$ttest_effectsize <- renderTable({
      tryCatch({
        df_loc <- ttest_local()
        cd <- cohens_d_pooled(df_loc$y, df_loc$group, order = levels(df_loc$group))
        data.frame(
          `Cohen's d (pooled SD)` = round(cd$d, 3),
          `Magnitude` = describe_d(cd$d),
          `Group order` = paste(cd$levels[1], "vs", cd$levels[2]),
          check.names = FALSE
        )
      }, error = function(e) NULL)
    })

    ## --- One-sentence interpretation -------------------------------------------

    output$ttest_interpretation <- renderUI({
      tryCatch({
        df_loc <- ttest_local()
        test <- stats::t.test(
          y ~ group, data = df_loc,
          alternative = input$ttest_alt, mu = input$ttest_mu, var.equal = FALSE
        )
        cd <- cohens_d_pooled(df_loc$y, df_loc$group, order = levels(df_loc$group))
        lvs <- levels(df_loc$group)

        direction <- if (is.na(cd$d)) "no clear difference" else if (cd$d > 0) paste0(lvs[1], " > ", lvs[2]) else paste0(lvs[1], " < ", lvs[2])
        mag <- describe_d(cd$d)
        sig <- if (is.na(test$p.value)) "with unclear significance" else if (test$p.value < 0.001) "and this difference is statistically significant (p < 0.001)"
        else if (test$p.value < 0.05) "and this difference is statistically significant (p < 0.05)"
        else "but this difference is not statistically significant (p ≥ 0.05)"

        sentence <- sprintf(
          "On average, %s shows a %s effect (Cohen's d = %0.2f) in the direction %s, %s.",
          input$ttest_num_var, mag, cd$d, direction, sig
        )
        HTML(paste("<b>Interpretation:</b> ", sentence))
      }, error = function(e) NULL)
    })


    ## --- Violin plot ------------------------------------------------------------

    output$ttest_violin <- renderPlot({
      tryCatch({
        df_loc <- ttest_local()
        # lazy load ggplot2 if available
        if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
        library(ggplot2)
        ggplot(df_loc, aes(x = group, y = y)) +
          geom_violin(trim = FALSE, alpha = 0.6) +
          geom_boxplot(width = 0.15, outlier.shape = NA) +
          geom_jitter(width = 0.08, height = 0, alpha = 0.5, size = 1.6) +
          labs(
            title = "Violin + boxplot with raw points",
            x = input$ttest_group_var,
            y = input$ttest_num_var
          )
      }, error = function(e) NULL)
    })


    ## --- Density plot -----------------------------------------------------------

    output$ttest_density <- renderPlot({
      tryCatch({
        df_loc <- ttest_local()
        if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
        library(ggplot2)
        ggplot(df_loc, aes(x = y, fill = group)) +
          geom_density(alpha = 0.4, adjust = 1) +
          labs(
            title = "Kernel density by group",
            x = input$ttest_num_var,
            y = "Density"
          )
      }, error = function(e) NULL)
    })


    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------

    ## ===== Paired t-test (server) ===========================================

    ## populate selectors (assuming output$data_ready already exists)
    observe({
      df <- my_data(); req(df)
      isolate({
        numeric_vars <- names(df)[sapply(df, is.numeric)]
        updateSelectInput(session, "ptt_var1", choices = numeric_vars)
        updateSelectInput(session, "ptt_var2", choices = numeric_vars)
      })
    })

    ## Populate and keep selectors mutually exclusive
    observeEvent(my_data(), {
      df <- my_data(); req(df)
      numeric_vars <- names(df)[sapply(df, is.numeric)]

      # var1 choices
      sel1 <- isolate(if (!is.null(input$ptt_var1) && input$ptt_var1 %in% numeric_vars) input$ptt_var1 else head(numeric_vars, 1))
      updateSelectInput(session, "ptt_var1", choices = numeric_vars, selected = sel1)

      # var2 choices exclude var1
      v2_choices <- setdiff(numeric_vars, sel1)
      sel2 <- isolate(if (!is.null(input$ptt_var2) && input$ptt_var2 %in% v2_choices) input$ptt_var2 else head(v2_choices, 1))
      updateSelectInput(session, "ptt_var2", choices = v2_choices, selected = sel2)
    }, ignoreInit = FALSE)

    observeEvent(input$ptt_var1, {
      df <- my_data(); req(df)
      numeric_vars <- names(df)[sapply(df, is.numeric)]
      v2_choices <- setdiff(numeric_vars, input$ptt_var1)
      sel2 <- if (!is.null(input$ptt_var2) && input$ptt_var2 %in% v2_choices) input$ptt_var2 else head(v2_choices, 1)
      updateSelectInput(session, "ptt_var2", choices = v2_choices, selected = sel2)
    }, ignoreInit = TRUE)

    observeEvent(input$ptt_var2, {
      df <- my_data(); req(df)
      numeric_vars <- names(df)[sapply(df, is.numeric)]
      v1_choices <- setdiff(numeric_vars, input$ptt_var2)
      sel1 <- if (!is.null(input$ptt_var1) && input$ptt_var1 %in% v1_choices) input$ptt_var1 else head(v1_choices, 1)
      updateSelectInput(session, "ptt_var1", choices = v1_choices, selected = sel1)
    }, ignoreInit = TRUE)

    ## --- helpers ----------------------------------------------------------------
    mean_ci <- function(x, conf.level = 0.95) {
      x <- x[is.finite(x)]
      n <- length(x); m <- mean(x); s <- stats::sd(x)
      se <- s / sqrt(n); alpha <- 1 - conf.level
      tcrit <- stats::qt(1 - alpha/2, df = n - 1)
      c(mean = m, lwr = m - tcrit * se, upr = m + tcrit * se, n = n)
    }

    describe_d <- function(d) {
      ad <- abs(d); if (is.na(ad)) return("unknown magnitude")
      if (ad < 0.2) "negligible" else if (ad < 0.5) "small" else if (ad < 0.8) "medium" else "large"
    }

    cohens_d_paired <- function(x, y) {  # dz = mean(diff)/sd(diff)
      d <- x - y; d_mean <- mean(d, na.rm = TRUE); d_sd <- stats::sd(d, na.rm = TRUE)
      d_z <- d_mean / d_sd
      list(d = d_z, diff_mean = d_mean, diff_sd = d_sd)
    }

    ## --- paired data reactive (pairwise complete) --------------------------------
    ptt_local <- reactive({
      df <- my_data()
      req(df, input$ptt_var1, input$ptt_var2)
      shiny::validate(
        need(input$ptt_var1 %in% names(df), "First variable not found."),
        need(input$ptt_var2 %in% names(df), "Second variable not found."),
        need(input$ptt_var1 != input$ptt_var2, "Pick two different variables.")
      )
      x <- df[[input$ptt_var1]]
      y <- df[[input$ptt_var2]]
      shiny::validate(need(is.numeric(x) && is.numeric(y), "Both variables must be numeric."))
      idx <- is.finite(x) & is.finite(y)
      shiny::validate(need(sum(idx) >= 2, "Need at least 2 complete pairs."))
      data.frame(x = x[idx], y = y[idx], id = seq_len(sum(idx)))
    })

    ## --- H0/H1 statement ---------------------------------------------------------
    output$ptt_h0_statement <- renderUI({
      req(input$ptt_var1, input$ptt_var2, input$ptt_mu, input$ptt_alt)
      v1 <- input$ptt_var1; v2 <- input$ptt_var2; mu <- input$ptt_mu
      h0 <- paste0("<b>Null Hypothesis</b> (H<sub>0</sub>): mean(", v1, " − ", v2, ") = ", mu, ".")
      h1 <- switch(input$ptt_alt,
                   "two.sided" = paste0("<b>Alternative Hypothesis</b> (H<sub>1</sub>): mean(", v1, " − ", v2, ") ≠ ", mu, "."),
                   "less"      = paste0("<b>Alternative Hypothesis</b> (H<sub>1</sub>): mean(", v1, " − ", v2, ") < ",  mu, "."),
                   "greater"   = paste0("<b>Alternative Hypothesis</b> (H<sub>1</sub>): mean(", v1, " − ", v2, ") > ",  mu, ".")
      )
      HTML(paste(h0, "<br>", h1, "<br><br>"))
    })

    ## --- main test result --------------------------------------------------------
    ptt_result <- reactive({
      df_loc <- ptt_local()
      test <- stats::t.test(
        df_loc$x, df_loc$y,
        alternative = input$ptt_alt,
        mu = input$ptt_mu,
        paired = TRUE
      )

      # dynamic mean columns
      means_tbl <- setNames(
        data.frame(
          round(mean(df_loc$x), 3),
          round(mean(df_loc$y), 3),
          check.names = FALSE
        ),
        c(sprintf("mean (%s)", input$ptt_var1),
          sprintf("mean (%s)", input$ptt_var2))
      )

      out <- cbind(
        data.frame(
          statistic = round(unname(test$statistic), 3),
          `degrees of freedom` = round(unname(test$parameter), 2),
          `n pairs` = nrow(df_loc),
          `p-value` = signif(test$p.value, 4),
          `CI (diff)` = sprintf("(%0.3f, %0.3f)", test$conf.int[1], test$conf.int[2]),
          check.names = FALSE
        ),
        means_tbl,
        check.names = FALSE
      )

      out
    })

    output$ptt_result <- renderTable({
      tryCatch(ptt_result(), error = function(e) NULL)
    })


    # ---- Labels ----
    output$ptt_label_boxplot <- renderUI({
      df_loc <- try(ptt_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 1)
      tags$h2("Boxplot of the two paired variables")
    })

    output$ptt_label_boxplot_diff <- renderUI({
      df_loc <- try(ptt_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 1)
      tags$h2("Boxplot of paired differences")
    })

    # ---- Boxplot: variables side-by-side ----
    output$ptt_boxplot <- renderPlot({
      tryCatch({
        df_loc <- ptt_local()
        if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
        library(ggplot2)

        long <- rbind(
          data.frame(variable = input$ptt_var1, value = df_loc$x),
          data.frame(variable = input$ptt_var2, value = df_loc$y)
        )

        ggplot(long, aes(x = variable, y = value)) +
          geom_boxplot(outlier.alpha = 0.35) +
          geom_jitter(width = 0.08, height = 0, alpha = 0.5, size = 1.6) +
          labs(title = "Boxplots of paired measurements",
               x = NULL, y = "Value")
      }, error = function(e) NULL)
    })

    # ---- Boxplot: paired differences (x - y) ----
    output$ptt_boxplot_diff <- renderPlot({
      tryCatch({
        df_loc <- ptt_local()
        if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
        library(ggplot2)

        dd <- data.frame(diff = df_loc$x - df_loc$y)
        ggplot(dd, aes(x = "", y = diff)) +
          geom_boxplot(width = 0.25, outlier.alpha = 0.35) +
          geom_hline(yintercept = 0, linetype = "dashed") +
          labs(title = sprintf("Paired differences: %s − %s", input$ptt_var1, input$ptt_var2),
               x = NULL, y = "Difference") +
          coord_flip()  # optional: horizontal boxplot; remove if you prefer vertical
      }, error = function(e) NULL)
    })


    ## --- labels + tables: CIs per variable --------------------------------------
    output$ptt_label_means_ci <- renderUI({
      df_loc <- try(ptt_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 0)
      tags$h2("Confidence intervals for each mean")
    })

    output$ptt_means_ci <- renderTable({
      tryCatch({
        df_loc <- ptt_local()
        ci1 <- mean_ci(df_loc$x); ci2 <- mean_ci(df_loc$y)
        data.frame(
          Variable = c(input$ptt_var1, input$ptt_var2),
          n = c(ci1["n"], ci2["n"]),
          Mean = round(c(ci1["mean"], ci2["mean"]), 3),
          `95% CI` = c(
            sprintf("(%0.3f, %0.3f)", ci1["lwr"], ci1["upr"]),
            sprintf("(%0.3f, %0.3f)", ci2["lwr"], ci2["upr"])
          ),
          check.names = FALSE
        )
      }, error = function(e) NULL)
    })

    ## --- labels + tables: Shapiro–Wilk on differences ---------------------------
    output$ptt_label_shapiro <- renderUI({
      df_loc <- try(ptt_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) >= 3)  # Shapiro needs >= 3
      tags$h2("Normality check on paired differences (Shapiro–Wilk)")
    })

    output$ptt_shapiro <- renderTable({
      tryCatch({
        df_loc <- ptt_local()
        d <- df_loc$x - df_loc$y
        n <- sum(is.finite(d))
        if (n < 3) {
          data.frame(`W` = NA_real_, `p-value` = NA_real_, Note = "Needs at least 3 pairs")
        } else if (n > 5000) {
          data.frame(`W` = NA_real_, `p-value` = NA_real_, Note = "n > 5000; test not computed")
        } else {
          s <- stats::shapiro.test(d)
          data.frame(`W` = round(unname(s$statistic), 3),
                     `p-value` = signif(s$p.value, 4),
                     Note = "", check.names = FALSE)
        }
      }, error = function(e) NULL)
    })

    ## --- labels + tables: Effect size (paired dz) --------------------------------
    output$ptt_label_effectsize <- renderUI({
      df_loc <- try(ptt_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 1)
      tags$h2("Effect size (Cohen’s d for paired data, dz)")
    })

    output$ptt_effectsize <- renderTable({
      tryCatch({
        df_loc <- ptt_local()
        cd <- cohens_d_paired(df_loc$x, df_loc$y)
        data.frame(
          `Cohen's d (paired, dz)` = round(cd$d, 3),
          `Magnitude` = describe_d(cd$d),
          check.names = FALSE
        )
      }, error = function(e) NULL)
    })

    ## --- label + interpretation line --------------------------------------------
    output$ptt_label_interpretation <- renderUI({
      df_loc <- try(ptt_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 1)
      tags$h2("Interpretation")
    })

    output$ptt_interpretation <- renderUI({
      tryCatch({
        df_loc <- ptt_local()
        test <- stats::t.test(df_loc$x, df_loc$y,
                              alternative = input$ptt_alt,
                              mu = input$ptt_mu,
                              paired = TRUE)
        d_vec <- df_loc$x - df_loc$y
        mdiff <- mean(d_vec)
        cd <- cohens_d_paired(df_loc$x, df_loc$y)
        dir <- if (is.na(mdiff)) "no clear difference"
        else if (mdiff > 0) paste0(input$ptt_var1, " > ", input$ptt_var2)
        else if (mdiff < 0) paste0(input$ptt_var1, " < ", input$ptt_var2)
        else "no difference"
        mag <- describe_d(cd$d)
        sig <- if (is.na(test$p.value)) "with unclear significance"
        else if (test$p.value < 0.001) "and this paired difference is statistically significant (p < 0.001)"
        else if (test$p.value < 0.05)  "and this paired difference is statistically significant (p < 0.05)"
        else "but this paired difference is not statistically significant (p ≥ 0.05)"
        sentence <- sprintf(
          "On average, the paired difference %s − %s is %0.3f, a %s effect (Cohen's dz = %0.2f), %s.",
          input$ptt_var1, input$ptt_var2, mdiff, mag, cd$d, sig
        )
        HTML(sentence)
      }, error = function(e) NULL)
    })

    ## --- optional plots (unchanged) ---------------------------------------------
    output$ptt_violin <- renderPlot({
      tryCatch({
        df_loc <- ptt_local()
        if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
        library(ggplot2)
        long <- rbind(
          data.frame(id = df_loc$id, variable = input$ptt_var1, value = df_loc$x),
          data.frame(id = df_loc$id, variable = input$ptt_var2, value = df_loc$y)
        )
        ggplot(long, aes(x = variable, y = value)) +
          geom_violin(trim = FALSE, alpha = 0.6) +
          geom_boxplot(width = 0.15, outlier.shape = NA) +
          geom_line(aes(group = id), position = position_dodge(width = 0.2), alpha = 0.25) +
          geom_jitter(width = 0.08, height = 0, alpha = 0.5, size = 1.6) +
          labs(title = "Paired data: violin + box + paired lines",
               x = NULL, y = "Value")
      }, error = function(e) NULL)
    })

    output$ptt_density <- renderPlot({
      tryCatch({
        df_loc <- ptt_local()
        if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
        library(ggplot2)
        long <- rbind(
          data.frame(variable = input$ptt_var1, value = df_loc$x),
          data.frame(variable = input$ptt_var2, value = df_loc$y)
        )
        ggplot(long, aes(x = value, fill = variable)) +
          geom_density(alpha = 0.4, adjust = 1) +
          labs(title = "Kernel density by variable", x = "Value", y = "Density")
      }, error = function(e) NULL)
    })


    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------

    ## ===== Mann–Whitney (Wilcoxon rank-sum) =================================

    output$mw_selection_notice <- renderUI({
      # show the notice only when the selection exists AND length != 2
      sels <- input$mw_selected_levels
      if (is.null(sels) || length(sels) == 2) return(NULL)

      # pretty, non-intrusive banner
      tags$div(
        class = "alert alert-warning",
        tags$b("Select exactly two groups"),
        tags$span(" – please choose two groups in the sidebar to run the test.")
      )
    })

    # Are inputs + data ready for 2-group computations?
    mw_ready <- reactive({
      df <- my_data()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(FALSE)
      if (is.null(input$mw_num_var) || is.null(input$mw_group_var) || is.null(input$mw_selected_levels)) return(FALSE)
      if (!(input$mw_num_var %in% names(df)) || !(input$mw_group_var %in% names(df))) return(FALSE)
      if (length(input$mw_selected_levels) != 2) return(FALSE)

      y <- df[[input$mw_num_var]]
      g <- as.factor(df[[input$mw_group_var]])
      if (!is.numeric(y) || all(is.na(g))) return(FALSE)
      if (any(!(input$mw_selected_levels %in% levels(g)))) return(FALSE)

      idx <- g %in% input$mw_selected_levels
      if (!any(idx)) return(FALSE)
      g2 <- factor(g[idx], levels = input$mw_selected_levels)
      y2 <- y[idx]
      if (nlevels(g2) != 2) return(FALSE)

      n1 <- sum(is.finite(y2[g2 == levels(g2)[1]]))
      n2 <- sum(is.finite(y2[g2 == levels(g2)[2]]))
      if (n1 == 0 || n2 == 0) return(FALSE)

      TRUE
    })

    # Cliff's delta from pooled ranks (robust w/ ties)
    cliffs_delta_vec <- function(x, y) {
      x <- x[is.finite(x)]; y <- y[is.finite(y)]
      n1 <- length(x); n2 <- length(y)
      if (n1 == 0 || n2 == 0) return(NA_real_)
      r <- rank(c(x, y), ties.method = "average")
      W1 <- sum(r[seq_len(n1)])
      U1 <- W1 - n1 * (n1 + 1) / 2
      2 * U1 / (n1 * n2) - 1
    }

    ## populate selectors
    observe({
      df <- my_data(); req(df)
      shiny::validate(need(ncol(df) > 0, "No columns in uploaded data."))
      isolate({
        numeric_vars <- names(df)[sapply(df, is.numeric)]
        updateSelectInput(session, "mw_num_var", choices = numeric_vars)
        updateSelectInput(session, "mw_group_var", choices = names(df))
      })
    })

    ## group levels picker
    output$mw_group_levels_ui <- renderUI({
      df <- my_data(); req(df, input$mw_group_var)
      g_raw <- df[[input$mw_group_var]]
      g <- try(as.factor(g_raw), silent = TRUE)
      shiny::validate(
        need(!inherits(g, "try-error"), "Cannot coerce grouping variable to factor."),
        need(nlevels(g) >= 2, "Grouping variable must have at least 2 levels.")
      )
      selectInput(
        "mw_selected_levels",
        "Select two groups to compare:",
        choices = levels(g),
        selected = levels(g)[1:2],
        multiple = TRUE
      )
    })

    ## helpers (reuse if already defined)
    mean_ci <- function(x, conf.level = 0.95) {
      x <- x[is.finite(x)]
      n <- length(x); m <- mean(x); s <- stats::sd(x)
      se <- s / sqrt(n); alpha <- 1 - conf.level
      tcrit <- stats::qt(1 - alpha/2, df = n - 1)
      c(mean = m, lwr = m - tcrit * se, upr = m + tcrit * se, n = n)
    }

    cliffs_delta_from_W <- function(W, n1, n2) {
      # U1 = W - n1*(n1+1)/2  (with average ranks -> ties get 0.5)
      U1 <- W - n1 * (n1 + 1) / 2
      delta <- 2 * U1 / (n1 * n2) - 1
      as.numeric(delta)
    }

    describe_cliffs <- function(delta) {
      a <- abs(delta)
      if (is.na(a)) return("unknown magnitude")
      if (a < 0.147) "negligible"
      else if (a < 0.33) "small"
      else if (a < 0.474) "medium"
      else "large"
    }

    ## local two-group data
    mw_local <- reactive({
      df <- my_data()
      req(df, input$mw_num_var, input$mw_group_var, input$mw_selected_levels)
      shiny::validate(need(length(input$mw_selected_levels) == 2, "Select exactly two groups."))

      y <- df[[input$mw_num_var]]
      g_raw <- df[[input$mw_group_var]]
      shiny::validate(need(is.numeric(y), "Selected outcome variable must be numeric."))
      g <- as.factor(g_raw)

      idx <- g %in% input$mw_selected_levels
      y <- y[idx]; g <- factor(g[idx], levels = input$mw_selected_levels)
      shiny::validate(need(nlevels(g) == 2, "After subsetting, must have exactly 2 groups."))

      data.frame(y = y, group = g)
    })

    ## H0/H1 statement
    output$mw_h0_statement <- renderUI({
      req(input$mw_num_var, input$mw_group_var, input$mw_selected_levels, input$mw_mu, input$mw_alt)
      g1 <- input$mw_selected_levels[1]; g2 <- input$mw_selected_levels[2]
      mu <- input$mw_mu
      h0 <- paste0("<b>Null Hypothesis</b> (H<sub>0</sub>): The location shift (median of ",
                   input$mw_num_var, " in ", g1, " minus ", g2, ") equals ", mu, ".")
      h1 <- switch(input$mw_alt,
                   "two.sided" = paste0("<b>Alternative Hypothesis</b> (H<sub>1</sub>): The location shift is not equal to ", mu, "."),
                   "less"      = paste0("<b>Alternative Hypothesis</b> (H<sub>1</sub>): ", g1, " tends to be less than ", g2, "."),
                   "greater"   = paste0("<b>Alternative Hypothesis</b> (H<sub>1</sub>): ", g1, " tends to be greater than ", g2, ".")
      )
      HTML(paste(h0, "<br>", h1, "<br><br>"))
    })

    ## main test result
    mw_result <- reactive({
      df_loc <- mw_local()
      g1 <- levels(df_loc$group)[1]; g2 <- levels(df_loc$group)[2]
      n1 <- sum(df_loc$group == g1); n2 <- sum(df_loc$group == g2)

      test <- stats::wilcox.test(
        y ~ group, data = df_loc,
        alternative = input$mw_alt, mu = input$mw_mu,
        conf.int = TRUE, exact = NA # let R choose exact/approx based on data
      )

      # dynamic mean columns
      means_tbl <- setNames(
        data.frame(
          round(mean(df_loc$y[df_loc$group == g1]), 3),
          round(mean(df_loc$y[df_loc$group == g2]), 3),
          check.names = FALSE
        ),
        c(sprintf("mean (%s)", g1), sprintf("mean (%s)", g2))
      )

      # medians too (often useful for MW)
      med_tbl <- setNames(
        data.frame(
          round(stats::median(df_loc$y[df_loc$group == g1]), 3),
          round(stats::median(df_loc$y[df_loc$group == g2]), 3),
          check.names = FALSE
        ),
        c(sprintf("median (%s)", g1), sprintf("median (%s)", g2))
      )

      out <- cbind(
        data.frame(
          `W statistic` = unname(test$statistic),
          `n (group 1)` = n1,
          `n (group 2)` = n2,
          `p-value` = signif(test$p.value, 4),
          `Hodges–Lehmann Δ (g1−g2)` = if (!is.null(test$estimate)) round(unname(test$estimate), 3) else NA_real_,
          `95% CI for Δ` = if (!is.null(test$conf.int)) sprintf("(%0.3f, %0.3f)", test$conf.int[1], test$conf.int[2]) else NA_character_,
          check.names = FALSE
        ),
        means_tbl,
        med_tbl,
        check.names = FALSE
      )
      out
    })

    output$mw_result <- renderTable({
      tryCatch(mw_result(), error = function(e) NULL)
    })

    ## labels + boxplot
    output$mw_label_boxplot <- renderUI({
      df_loc <- try(mw_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 1)
      tags$h2("Boxplot of selected groups")
    })

    # ---- Boxplot: variables side-by-side ----
    output$mw_boxplot <- renderPlot({
      df_loc <- try(mw_local(), silent = TRUE)

      # Quietly bail out until ready (no red error text)
      req(!inherits(df_loc, "try-error"), cancelOutput = TRUE)
      req(!is.null(df_loc), nrow(df_loc) > 1, cancelOutput = TRUE)
      req(length(unique(df_loc$group)) == 2, cancelOutput = TRUE)
      req(isTRUE(requireNamespace("ggplot2", quietly = TRUE)), cancelOutput = TRUE)

      library(ggplot2)

      # Safe counts for labels; ensure both groups have data
      g_counts <- table(droplevels(df_loc$group))
      req(length(g_counts) == 2, all(g_counts > 0), cancelOutput = TRUE)

      labs <- paste0(names(g_counts), " (n = ", as.integer(g_counts), ")")
      names(labs) <- names(g_counts)

      ggplot(df_loc, aes(x = group, y = y)) +
        geom_boxplot(outlier.alpha = 0.35) +
        geom_jitter(width = 0.08, height = 0, alpha = 0.5, size = 1.6) +
        scale_x_discrete(labels = labs, drop = FALSE) +
        labs(title = "Boxplots", x = input$mw_group_var, y = input$mw_num_var)
    })

    ## labels + CIs for means (to match your t-test UX)
    output$mw_label_means_ci <- renderUI({
      df_loc <- try(mw_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 0)
      tags$h2("Confidence intervals for group means")
    })

    output$mw_means_ci <- renderTable({
      tryCatch({
        df_loc <- mw_local()
        lvs <- levels(df_loc$group)
        ci1 <- mean_ci(df_loc$y[df_loc$group == lvs[1]])
        ci2 <- mean_ci(df_loc$y[df_loc$group == lvs[2]])
        data.frame(
          Group = lvs,
          n = c(ci1["n"], ci2["n"]),
          Mean = round(c(ci1["mean"], ci2["mean"]), 3),
          `95% CI` = c(
            sprintf("(%0.3f, %0.3f)", ci1["lwr"], ci1["upr"]),
            sprintf("(%0.3f, %0.3f)", ci2["lwr"], ci2["upr"])
          ),
          check.names = FALSE
        )
      }, error = function(e) NULL)
    })

    ## labels + Shapiro–Wilk per group (optional check)
    output$mw_label_shapiro <- renderUI({
      df_loc <- try(mw_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 2)
      tags$h2("Normality check per group (Shapiro–Wilk)")
    })

    output$mw_shapiro <- renderTable({
      tryCatch({
        df_loc <- mw_local()
        lvs <- levels(df_loc$group)
        res <- lapply(lvs, function(L) {
          x <- df_loc$y[df_loc$group == L]
          if (sum(is.finite(x)) < 3) {
            c(W = NA_real_, `p-value` = NA_real_, Note = "Needs at least 3 observations")
          } else if (sum(is.finite(x)) > 5000) {
            c(W = NA_real_, `p-value` = NA_real_, Note = "n > 5000; test not computed")
          } else {
            s <- stats::shapiro.test(x)
            c(W = unname(round(s$statistic, 3)), `p-value` = signif(s$p.value, 4), Note = "")
          }
        })
        df <- do.call(rbind, res)
        data.frame(Group = lvs, df, check.names = FALSE, row.names = NULL)
      }, error = function(e) NULL)
    })

    ## labels + Effect size (Cliff's delta / rank-biserial)
    output$mw_label_effectsize <- renderUI({
      df_loc <- try(mw_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 1)
      tags$h2("Effect size (Cliff’s delta / rank-biserial)")
    })

    # ---- Effect size table (Cliff's δ) ----
    output$mw_effectsize <- renderTable({
      req(mw_ready(), cancelOutput = TRUE)

      df_loc <- mw_local()  # now safe to call (validations will pass)
      lvs <- levels(df_loc$group)
      x <- df_loc$y[df_loc$group == lvs[1]]
      y <- df_loc$y[df_loc$group == lvs[2]]

      delta <- cliffs_delta_vec(x, y)
      data.frame(
        `Cliff's delta (δ)` = round(delta, 3),
        `Magnitude` = describe_cliffs(delta),
        `Direction` = if (is.na(delta)) "–"
        else if (delta > 0) paste0(lvs[1], " > ", lvs[2])
        else if (delta < 0) paste0(lvs[1], " < ", lvs[2])
        else "no difference",
        check.names = FALSE
      )
    })

    ## label + Interpretation
    output$mw_label_interpretation <- renderUI({
      df_loc <- try(mw_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 1)
      tags$h2("Interpretation")
    })

    # Robust interpretation (blank until ready; no errors on ties/NAs)
    output$mw_interpretation <- renderUI({
      # 1) hard guard – render nothing until inputs/data truly ready
      if (!isTRUE(try(mw_ready(), silent = TRUE))) return(NULL)

      # 2) safely get local data without throwing validation messages
      df_loc <- try(mw_local(), silent = TRUE)
      if (inherits(df_loc, "try-error") || is.null(df_loc) || nrow(df_loc) < 2) return(NULL)

      lvs <- levels(df_loc$group)
      if (length(lvs) != 2) return(NULL)

      # 3) finite-only vectors (avoid NA issues)
      x <- df_loc$y[df_loc$group == lvs[1]]
      y <- df_loc$y[df_loc$group == lvs[2]]
      x <- x[is.finite(x)]; y <- y[is.finite(y)]
      if (length(x) == 0 || length(y) == 0) return(NULL)

      # 4) Wilcoxon test (approx; avoids exact-mode failures with ties)
      test <- try(suppressWarnings(stats::wilcox.test(
        x, y,
        alternative = input$mw_alt, mu = input$mw_mu,
        conf.int = TRUE, exact = FALSE
      )), silent = TRUE)
      if (inherits(test, "try-error")) return(NULL)

      # 5) Cliff's delta (robust via pooled ranks)
      cliffs_delta_vec <- function(x, y) {
        x <- x[is.finite(x)]; y <- y[is.finite(y)]
        n1 <- length(x); n2 <- length(y)
        if (n1 == 0 || n2 == 0) return(NA_real_)
        r <- rank(c(x, y), ties.method = "average")
        W1 <- sum(r[seq_len(n1)])
        U1 <- W1 - n1 * (n1 + 1) / 2
        2 * U1 / (n1 * n2) - 1
      }
      describe_cliffs <- function(delta) {
        a <- abs(delta)
        if (is.na(a)) return("unknown magnitude")
        if (a < 0.147) "negligible" else if (a < 0.33) "small" else if (a < 0.474) "medium" else "large"
      }

      delta <- cliffs_delta_vec(x, y)
      mag   <- describe_cliffs(delta)

      # Direction from HL estimate if available; fallback to median diff
      est <- if (!is.null(test$estimate) && is.finite(test$estimate)) unname(test$estimate)
      else stats::median(x) - stats::median(y)
      dir <- if (!is.finite(est) || is.na(est)) "no clear difference"
      else if (est > 0) paste0(lvs[1], " > ", lvs[2])
      else if (est < 0) paste0(lvs[1], " < ", lvs[2])
      else "no difference"

      pval <- suppressWarnings(test$p.value)
      sig <- if (is.null(pval) || is.na(pval)) "with unclear significance"
      else if (pval < 0.001) "and this difference is statistically significant (p < 0.001)"
      else if (pval < 0.05)  "and this difference is statistically significant (p < 0.05)"
      else "but this difference is not statistically significant (p ≥ 0.05)"

      # Safe formatting even if delta is NA
      delta_str <- if (is.na(delta)) "NA" else sprintf("%0.2f", delta)

      HTML(sprintf(
        "By ranks, %s tends to be %s (Cliff’s δ = %s, %s), %s.",
        input$mw_num_var, dir, delta_str, mag, sig
      ))
    })

    ## violin & density
    output$mw_violin <- renderPlot({
      tryCatch({
        df_loc <- mw_local()
        if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
        library(ggplot2)
        ggplot(df_loc, aes(x = group, y = y)) +
          geom_violin(trim = FALSE, alpha = 0.6) +
          geom_boxplot(width = 0.15, outlier.shape = NA) +
          geom_jitter(width = 0.08, height = 0, alpha = 0.5, size = 1.6) +
          labs(title = "Violin + boxplot with raw points",
               x = input$mw_group_var, y = input$mw_num_var)
      }, error = function(e) NULL)
    })

    output$mw_density <- renderPlot({
      tryCatch({
        df_loc <- mw_local()
        if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
        library(ggplot2)
        ggplot(df_loc, aes(x = y, fill = group)) +
          geom_density(alpha = 0.4, adjust = 1) +
          labs(title = "Kernel density by group", x = input$mw_num_var, y = "Density")
      }, error = function(e) NULL)
    })


    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------

    ## ===== Paired Wilcoxon (signed-rank) ====================================

    ## Populate selectors
    observe({
      df <- my_data(); req(df)
      isolate({
        numeric_vars <- names(df)[sapply(df, is.numeric)]
        updateSelectInput(session, "pwx_var1", choices = numeric_vars)
        updateSelectInput(session, "pwx_var2", choices = numeric_vars)
      })
    })

    ## Populate and keep selectors mutually exclusive
    observeEvent(my_data(), {
      df <- my_data(); req(df)
      numeric_vars <- names(df)[sapply(df, is.numeric)]

      sel1 <- isolate(if (!is.null(input$pwx_var1) && input$pwx_var1 %in% numeric_vars) input$pwx_var1 else head(numeric_vars, 1))
      updateSelectInput(session, "pwx_var1", choices = numeric_vars, selected = sel1)

      v2_choices <- setdiff(numeric_vars, sel1)
      sel2 <- isolate(if (!is.null(input$pwx_var2) && input$pwx_var2 %in% v2_choices) input$pwx_var2 else head(v2_choices, 1))
      updateSelectInput(session, "pwx_var2", choices = v2_choices, selected = sel2)
    }, ignoreInit = FALSE)

    observeEvent(input$pwx_var1, {
      df <- my_data(); req(df)
      numeric_vars <- names(df)[sapply(df, is.numeric)]
      v2_choices <- setdiff(numeric_vars, input$pwx_var1)
      sel2 <- if (!is.null(input$pwx_var2) && input$pwx_var2 %in% v2_choices) input$pwx_var2 else head(v2_choices, 1)
      updateSelectInput(session, "pwx_var2", choices = v2_choices, selected = sel2)
    }, ignoreInit = TRUE)

    observeEvent(input$pwx_var2, {
      df <- my_data(); req(df)
      numeric_vars <- names(df)[sapply(df, is.numeric)]
      v1_choices <- setdiff(numeric_vars, input$pwx_var2)
      sel1 <- if (!is.null(input$pwx_var1) && input$pwx_var1 %in% v1_choices) input$pwx_var1 else head(v1_choices, 1)
      updateSelectInput(session, "pwx_var1", choices = v1_choices, selected = sel1)
    }, ignoreInit = TRUE)

    ## ---- helpers (reuse if already defined elsewhere) --------------------------
    mean_ci <- function(x, conf.level = 0.95) {
      x <- x[is.finite(x)]
      n <- length(x); m <- mean(x); s <- stats::sd(x)
      se <- if (n > 0) s / sqrt(n) else NA_real_
      alpha <- 1 - conf.level
      tcrit <- if (n > 1) stats::qt(1 - alpha/2, df = n - 1) else NA_real_
      c(mean = m, lwr = m - tcrit * se, upr = m + tcrit * se, n = n)
    }

    describe_rb <- function(r) {
      a <- abs(r); if (is.na(a)) return("unknown magnitude")
      if (a < 0.147) "negligible" else if (a < 0.33) "small" else if (a < 0.474) "medium" else "large"
    }

    rank_biserial_paired <- function(x, y) {
      d <- x - y
      d <- d[is.finite(d) & d != 0]          # signed-rank ignores zeros
      n <- length(d)
      if (n == 0) return(list(r = NA_real_, n_used = 0))
      Rtot <- n * (n + 1) / 2
      rnk  <- rank(abs(d), ties.method = "average")
      Vpos <- sum(rnk[d > 0])                # sum of positive ranks
      r_rb <- 2 * Vpos / Rtot - 1            # matched-pairs rank-biserial
      list(r = as.numeric(r_rb), n_used = n)
    }

    ## ---- readiness guards ------------------------------------------------------
    pwx_ready <- reactive({
      df <- my_data()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(FALSE)
      if (is.null(input$pwx_var1) || is.null(input$pwx_var2)) return(FALSE)
      if (!(input$pwx_var1 %in% names(df)) || !(input$pwx_var2 %in% names(df))) return(FALSE)
      if (identical(input$pwx_var1, input$pwx_var2)) return(FALSE)
      x <- df[[input$pwx_var1]]; y <- df[[input$pwx_var2]]
      if (!is.numeric(x) || !is.numeric(y)) return(FALSE)
      idx <- is.finite(x) & is.finite(y)
      if (!any(idx)) return(FALSE)
      # need at least one non-zero difference for the signed-rank test
      sum(abs(x[idx] - y[idx]) > 0) >= 1
    })

    ## ---- local paired data -----------------------------------------------------
    pwx_local <- reactive({
      df <- my_data(); req(df, input$pwx_var1, input$pwx_var2)
      shiny::validate(
        need(input$pwx_var1 %in% names(df), "First variable not found."),
        need(input$pwx_var2 %in% names(df), "Second variable not found."),
        need(input$pwx_var1 != input$pwx_var2, "Pick two different variables.")
      )
      x <- df[[input$pwx_var1]]; y <- df[[input$pwx_var2]]
      shiny::validate(need(is.numeric(x) && is.numeric(y), "Both variables must be numeric."))
      idx <- is.finite(x) & is.finite(y)
      shiny::validate(need(sum(idx) >= 1, "Need at least 1 complete pair."))
      data.frame(x = x[idx], y = y[idx], id = seq_len(sum(idx)))
    })

    ## ---- H0/H1 statement -------------------------------------------------------
    output$pwx_h0_statement <- renderUI({
      req(input$pwx_var1, input$pwx_var2, input$pwx_mu, input$pwx_alt)
      v1 <- input$pwx_var1; v2 <- input$pwx_var2; mu <- input$pwx_mu
      h0 <- paste0("<b>Null Hypothesis</b> (H<sub>0</sub>): median(", v1, " − ", v2, ") = ", mu, ".")
      h1 <- switch(input$pwx_alt,
                   "two.sided" = paste0("<b>Alternative Hypothesis</b> (H<sub>1</sub>): median(", v1, " − ", v2, ") ≠ ", mu, "."),
                   "less"      = paste0("<b>Alternative Hypothesis</b> (H<sub>1</sub>): median(", v1, " − ", v2, ") < ",  mu, "."),
                   "greater"   = paste0("<b>Alternative Hypothesis</b> (H<sub>1</sub>): median(", v1, " − ", v2, ") > ",  mu, ".")
      )
      HTML(paste(h0, "<br>", h1, "<br><br>"))
    })

    ## ---- main test result ------------------------------------------------------
    pwx_result <- reactive({
      req(pwx_ready())
      df_loc <- pwx_local()
      test <- stats::wilcox.test(
        df_loc$x, df_loc$y,
        alternative = input$pwx_alt, mu = input$pwx_mu,
        paired = TRUE, conf.int = TRUE, exact = FALSE
      )

      d <- df_loc$x - df_loc$y
      n_total  <- nrow(df_loc)
      n_nonzero <- sum(is.finite(d) & d != 0)

      # dynamic means
      means_tbl <- setNames(
        data.frame(
          round(mean(df_loc$x), 3),
          round(mean(df_loc$y), 3),
          check.names = FALSE
        ),
        c(sprintf("mean (%s)", input$pwx_var1),
          sprintf("mean (%s)", input$pwx_var2))
      )

      out <- cbind(
        data.frame(
          `V statistic` = unname(test$statistic),
          `n pairs` = n_total,
          `n non-zero pairs` = n_nonzero,
          `p-value` = signif(test$p.value, 4),
          `Hodges–Lehmann Δ (v1−v2)` =
            if (!is.null(test$estimate)) round(unname(test$estimate), 3) else NA_real_,
          `95% CI for Δ` =
            if (!is.null(test$conf.int)) sprintf("(%0.3f, %0.3f)", test$conf.int[1], test$conf.int[2]) else NA_character_,
          check.names = FALSE
        ),
        means_tbl,
        check.names = FALSE
      )
      out
    })

    output$pwx_result <- renderTable({
      tryCatch(pwx_result(), error = function(e) NULL)
    })

    ## ---- labels + CI table for each mean ---------------------------------------
    output$pwx_label_means_ci <- renderUI({
      req(pwx_ready(), cancelOutput = TRUE)
      tags$h2("Confidence intervals for each mean")
    })

    output$pwx_means_ci <- renderTable({
      req(pwx_ready(), cancelOutput = TRUE)
      df_loc <- pwx_local()
      ci1 <- mean_ci(df_loc$x); ci2 <- mean_ci(df_loc$y)
      data.frame(
        Variable = c(input$pwx_var1, input$pwx_var2),
        n = c(ci1["n"], ci2["n"]),
        Mean = round(c(ci1["mean"], ci2["mean"]), 3),
        `95% CI` = c(
          sprintf("(%0.3f, %0.3f)", ci1["lwr"], ci1["upr"]),
          sprintf("(%0.3f, %0.3f)", ci2["lwr"], ci2["upr"])
        ),
        check.names = FALSE
      )
    })

    ## ---- labels + Shapiro–Wilk on differences ----------------------------------
    output$pwx_label_shapiro <- renderUI({
      # Only show if we’ll likely be able to compute (needs >= 3 non-zero ideally)
      if (!isTRUE(try(pwx_ready(), silent = TRUE))) return(NULL)
      tags$h2("Normality check on paired differences (Shapiro–Wilk)")
    })

    output$pwx_shapiro <- renderTable({
      df_loc <- try(pwx_local(), silent = TRUE)
      if (inherits(df_loc, "try-error") || is.null(df_loc)) return(NULL)
      d <- df_loc$x - df_loc$y
      d <- d[is.finite(d)]
      if (length(d) < 3) {
        return(data.frame(`W` = NA_real_, `p-value` = NA_real_, Note = "Needs at least 3 pairs"))
      }
      if (length(d) > 5000) {
        return(data.frame(`W` = NA_real_, `p-value` = NA_real_, Note = "n > 5000; test not computed"))
      }
      s <- stats::shapiro.test(d)
      data.frame(`W` = round(unname(s$statistic), 3),
                 `p-value` = signif(s$p.value, 4),
                 Note = "", check.names = FALSE)
    })

    ## ---- labels + Effect size (matched-pairs rank-biserial) --------------------
    output$pwx_label_effectsize <- renderUI({
      req(pwx_ready(), cancelOutput = TRUE)
      tags$h2("Effect size (matched-pairs rank-biserial)")
    })

    output$pwx_effectsize <- renderTable({
      req(pwx_ready(), cancelOutput = TRUE)
      df_loc <- pwx_local()
      es <- rank_biserial_paired(df_loc$x, df_loc$y)
      data.frame(
        `Rank-biserial r` = round(es$r, 3),
        `Magnitude` = describe_rb(es$r),
        check.names = FALSE
      )
    })

    ## ---- labels + Interpretation line ------------------------------------------
    output$pwx_label_interpretation <- renderUI({
      req(pwx_ready(), cancelOutput = TRUE)
      tags$h2("Interpretation")
    })

    output$pwx_interpretation <- renderUI({
      req(pwx_ready(), cancelOutput = TRUE)

      df_loc <- pwx_local()
      test <- try(suppressWarnings(stats::wilcox.test(
        df_loc$x, df_loc$y,
        alternative = input$pwx_alt, mu = input$pwx_mu,
        paired = TRUE, conf.int = TRUE, exact = FALSE
      )), silent = TRUE)
      if (inherits(test, "try-error")) return(NULL)

      es <- rank_biserial_paired(df_loc$x, df_loc$y)
      mag <- describe_rb(es$r)

      # Direction from HL estimate if available; fall back to median diff
      est <- if (!is.null(test$estimate) && is.finite(test$estimate)) unname(test$estimate)
      else stats::median(df_loc$x) - stats::median(df_loc$y)
      dir <- if (!is.finite(est) || is.na(est)) "no clear difference"
      else if (est > 0) paste0(input$pwx_var1, " > ", input$pwx_var2)
      else if (est < 0) paste0(input$pwx_var1, " < ", input$pwx_var2)
      else "no difference"

      pval <- suppressWarnings(test$p.value)
      sig <- if (is.null(pval) || is.na(pval)) "with unclear significance"
      else if (pval < 0.001) "and this paired difference is statistically significant (p < 0.001)"
      else if (pval < 0.05)  "and this paired difference is statistically significant (p < 0.05)"
      else "but this paired difference is not statistically significant (p ≥ 0.05)"

      r_str <- if (is.na(es$r)) "NA" else sprintf("%0.2f", es$r)

      HTML(sprintf(
        "By ranks, the paired difference %s − %s favors %s (rank-biserial r = %s, %s), %s.",
        input$pwx_var1, input$pwx_var2, dir, r_str, mag, sig
      ))
    })

    ## ---- Plots ------------------------------------------------------------------
    output$pwx_label_boxplot <- renderUI({
      df_loc <- try(pwx_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 1, cancelOutput = TRUE)
      tags$h2("Boxplot of the two paired variables")
    })

    output$pwx_boxplot <- renderPlot({
      df_loc <- try(pwx_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), !is.null(df_loc), nrow(df_loc) > 1, cancelOutput = TRUE)
      req(isTRUE(requireNamespace("ggplot2", quietly = TRUE)), cancelOutput = TRUE)
      library(ggplot2)
      long <- rbind(
        data.frame(variable = input$pwx_var1, value = df_loc$x),
        data.frame(variable = input$pwx_var2, value = df_loc$y)
      )
      ggplot(long, aes(x = variable, y = value)) +
        geom_boxplot(outlier.alpha = 0.35) +
        geom_jitter(width = 0.08, height = 0, alpha = 0.5, size = 1.6) +
        labs(title = "Boxplots of paired measurements", x = NULL, y = "Value")
    })

    output$pwx_label_boxplot_diff <- renderUI({
      df_loc <- try(pwx_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 1, cancelOutput = TRUE)
      tags$h2("Boxplot of paired differences")
    })

    output$pwx_boxplot_diff <- renderPlot({
      df_loc <- try(pwx_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), !is.null(df_loc), nrow(df_loc) > 1, cancelOutput = TRUE)
      req(isTRUE(requireNamespace("ggplot2", quietly = TRUE)), cancelOutput = TRUE)
      library(ggplot2)
      dd <- data.frame(diff = df_loc$x - df_loc$y)
      ggplot(dd, aes(x = "", y = diff)) +
        geom_boxplot(width = 0.25, outlier.alpha = 0.35) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(title = sprintf("Paired differences: %s − %s", input$pwx_var1, input$pwx_var2),
             x = NULL, y = "Difference") +
        coord_flip()
    })

    output$pwx_violin <- renderPlot({
      df_loc <- try(pwx_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), !is.null(df_loc), nrow(df_loc) > 1, cancelOutput = TRUE)
      req(isTRUE(requireNamespace("ggplot2", quietly = TRUE)), cancelOutput = TRUE)
      library(ggplot2)
      long <- rbind(
        data.frame(id = df_loc$id, variable = input$pwx_var1, value = df_loc$x),
        data.frame(id = df_loc$id, variable = input$pwx_var2, value = df_loc$y)
      )
      ggplot(long, aes(x = variable, y = value)) +
        geom_violin(trim = FALSE, alpha = 0.6) +
        geom_boxplot(width = 0.15, outlier.shape = NA) +
        geom_line(aes(group = id), position = position_dodge(width = 0.2), alpha = 0.25) +
        geom_jitter(width = 0.08, height = 0, alpha = 0.5, size = 1.6) +
        labs(title = "Paired data: violin + box + paired lines", x = NULL, y = "Value")
    })

    output$pwx_density <- renderPlot({
      df_loc <- try(pwx_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), !is.null(df_loc), nrow(df_loc) > 1, cancelOutput = TRUE)
      req(isTRUE(requireNamespace("ggplot2", quietly = TRUE)), cancelOutput = TRUE)
      library(ggplot2)
      long <- rbind(
        data.frame(variable = input$pwx_var1, value = df_loc$x),
        data.frame(variable = input$pwx_var2, value = df_loc$y)
      )
      ggplot(long, aes(x = value, fill = variable)) +
        geom_density(alpha = 0.4, adjust = 1) +
        labs(title = "Kernel density by variable", x = "Value", y = "Density")
    })


    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------

    ## ===== One-way ANOVA =========================================================

    ## Populate selectors
    observe({
      df <- my_data(); req(df); shiny::validate(need(ncol(df) > 0, "No columns in uploaded data."))
      isolate({
        numeric_vars <- names(df)[sapply(df, is.numeric)]
        updateSelectInput(session, "anova_num_var", choices = numeric_vars)

        updateSelectInput(session, "anova_group_var", choices = names(df))
      })
    })

    ## Group levels picker (allow 2+)
    output$anova_group_levels_ui <- renderUI({
      df <- my_data(); req(df, input$anova_group_var)
      g_raw <- df[[input$anova_group_var]]
      g <- try(as.factor(g_raw), silent = TRUE)
      shiny::validate(
        need(!inherits(g, "try-error"), "Cannot coerce grouping variable to factor."),
        need(nlevels(g) >= 2, "Grouping variable must have at least 2 levels.")
      )
      selectInput(
        "anova_selected_levels", "Select groups to compare (2 or more):",
        choices = levels(g),
        selected = levels(g),
        multiple = TRUE
      )
    })

    ## Helper: mean + t CI
    mean_ci <- function(x, conf.level = 0.95) {
      x <- x[is.finite(x)]; n <- length(x)
      m <- mean(x); s <- stats::sd(x); se <- s / sqrt(n)
      alpha <- 1 - conf.level
      tcrit <- if (n > 1) stats::qt(1 - alpha/2, df = n - 1) else NA_real_
      c(mean = m, lwr = m - tcrit * se, upr = m + tcrit * se, n = n)
    }

    describe_eta2 <- function(e) {
      a <- abs(e); if (is.na(a)) return("unknown magnitude")
      if (a < .01) "negligible" else if (a < .06) "small" else if (a < .14) "medium" else "large"
    }

    ## Ready check
    anova_ready <- reactive({
      df <- my_data()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(FALSE)
      if (is.null(input$anova_num_var) || is.null(input$anova_group_var) || is.null(input$anova_selected_levels)) return(FALSE)
      if (!(input$anova_num_var %in% names(df)) || !(input$anova_group_var %in% names(df))) return(FALSE)
      y <- df[[input$anova_num_var]]; g <- as.factor(df[[input$anova_group_var]])
      if (!is.numeric(y)) return(FALSE)
      if (length(input$anova_selected_levels) < 2) return(FALSE)
      if (!all(input$anova_selected_levels %in% levels(g))) return(FALSE)
      idx <- g %in% input$anova_selected_levels
      y <- y[idx]; g <- factor(g[idx], levels = input$anova_selected_levels)
      # need at least 2 total df: total N > number of groups
      if (sum(is.finite(y)) <= nlevels(g)) return(FALSE)
      TRUE
    })

    ## Local filtered data
    anova_local <- reactive({
      req(anova_ready())
      df <- my_data()
      y <- df[[input$anova_num_var]]
      g <- as.factor(df[[input$anova_group_var]])
      idx <- g %in% input$anova_selected_levels
      y <- y[idx]; g <- factor(g[idx], levels = input$anova_selected_levels)
      data.frame(y = y, group = g)
    })

    ## Notice if invalid selection
    output$anova_selection_notice <- renderUI({
      # show a banner if selection is missing OR has < 2 groups
      sels <- input$anova_selected_levels
      if (is.null(sels) || length(sels) < 2) {
        tags$div(
          class = "alert alert-warning",
          tags$b("Select at least two groups"),
          tags$span(" – please choose two or more groups in the sidebar to run ANOVA.")
        )
      } else {
        NULL
      }
    })

    ## H0/H1 statement
    output$anova_h0_statement <- renderUI({
      req(input$anova_num_var, input$anova_group_var)
      HTML(paste0(
        "<b>Null Hypothesis</b> (H<sub>0</sub>): All group means of '",
        input$anova_num_var, "' are equal.<br>",
        "<b>Alternative</b> (H<sub>1</sub>): At least one group mean differs.<br><br>"
      ))
    })

    ## ANOVA fit and table
    anova_fit <- reactive({
      df_loc <- anova_local()
      stats::aov(y ~ group, data = df_loc)
    })

    output$anova_label_table <- renderUI({ req(anova_ready(), cancelOutput = TRUE); tags$h2("ANOVA table") })

    output$anova_table <- renderTable({
      req(anova_ready(), cancelOutput = TRUE)
      sm <- summary(anova_fit())[[1]]
      # Convert to data.frame with nice names
      out <- data.frame(
        Term = rownames(sm),
        Df = sm[,"Df"],
        `Sum Sq` = round(sm[,"Sum Sq"], 4),
        `Mean Sq` = round(sm[,"Mean Sq"], 4),
        `F value` = round(sm[,"F value"], 3),
        `Pr(>F)` = signif(sm[,"Pr(>F)"], 4),
        check.names = FALSE, row.names = NULL
      )
      out
    })

    ## Boxplot
    output$anova_label_boxplot <- renderUI({ req(anova_ready(), cancelOutput = TRUE); tags$h2("Boxplot of selected groups") })
    output$anova_boxplot <- renderPlot({
      req(anova_ready(), cancelOutput = TRUE)
      df_loc <- anova_local()
      if (!requireNamespace("ggplot2", quietly = TRUE)) return(invisible(NULL))
      library(ggplot2)
      counts <- table(df_loc$group)
      labs <- paste0(names(counts), " (n = ", as.integer(counts), ")")
      names(labs) <- names(counts)
      ggplot(df_loc, aes(x = group, y = y)) +
        geom_boxplot(outlier.alpha = 0.35) +
        geom_jitter(width = 0.08, height = 0, alpha = 0.5, size = 1.6) +
        scale_x_discrete(labels = labs) +
        labs(title = "Boxplots", x = input$anova_group_var, y = input$anova_num_var)
    })

    ## Group means + CIs
    output$anova_label_means_ci <- renderUI({ req(anova_ready(), cancelOutput = TRUE); tags$h2("Confidence intervals for group means") })
    output$anova_means_ci <- renderTable({
      req(anova_ready(), cancelOutput = TRUE)
      df_loc <- anova_local()
      lvs <- levels(df_loc$group)
      rows <- lapply(lvs, function(L) {
        ci <- mean_ci(df_loc$y[df_loc$group == L])
        data.frame(
          Group = L,
          n = ci["n"],
          Mean = round(ci["mean"], 3),
          `95% CI` = sprintf("(%0.3f, %0.3f)", ci["lwr"], ci["upr"]),
          check.names = FALSE
        )
      })
      do.call(rbind, rows)
    })

    ## Assumption checks
    output$anova_label_assumptions <- renderUI({ req(anova_ready(), cancelOutput = TRUE); tags$h2("Assumption checks") })

    # Shapiro–Wilk on residuals
    output$anova_shapiro_resid <- renderTable({
      req(anova_ready(), cancelOutput = TRUE)
      fit <- anova_fit()
      res <- residuals(fit)
      res <- res[is.finite(res)]
      if (length(res) < 3) {
        return(data.frame(`Shapiro–Wilk (residuals) W` = NA_real_, `p-value` = NA_real_, Note = "Needs at least 3 residuals", check.names = FALSE))
      }
      if (length(res) > 5000) {
        return(data.frame(`Shapiro–Wilk (residuals) W` = NA_real_, `p-value` = NA_real_, Note = "n > 5000; not computed", check.names = FALSE))
      }
      s <- stats::shapiro.test(res)
      data.frame(`Shapiro–Wilk (residuals) W` = round(unname(s$statistic), 3),
                 `p-value` = signif(s$p.value, 4),
                 Note = "", check.names = FALSE)
    })

    # Optional: Shapiro per group (exploratory)
    output$anova_shapiro_groups <- renderTable({
      req(anova_ready(), cancelOutput = TRUE)
      df_loc <- anova_local()
      lvs <- levels(df_loc$group)
      res <- lapply(lvs, function(L) {
        x <- df_loc$y[df_loc$group == L]
        if (sum(is.finite(x)) < 3) c(W = NA_real_, `p-value` = NA_real_, Note = "n<3")
        else if (sum(is.finite(x)) > 5000) c(W = NA_real_, `p-value` = NA_real_, Note = "n>5000")
        else { s <- stats::shapiro.test(x); c(W = round(unname(s$statistic),3), `p-value` = signif(s$p.value,4), Note = "") }
      })
      df <- do.call(rbind, res)
      data.frame(Group = lvs, df, check.names = FALSE, row.names = NULL)
    })

    # Homogeneity: Bartlett (base)
    output$anova_bartlett <- renderTable({
      req(anova_ready(), cancelOutput = TRUE)
      df_loc <- anova_local()
      bt <- try(stats::bartlett.test(y ~ group, data = df_loc), silent = TRUE)
      if (inherits(bt, "try-error")) return(NULL)
      data.frame(
        `Bartlett K-squared` = round(unname(bt$statistic), 3),
        df = unname(bt$parameter),
        `p-value` = signif(bt$p.value, 4),
        check.names = FALSE
      )
    })

    # Homogeneity: Levene (if car is available)
    output$anova_levene <- renderTable({
      req(anova_ready(), cancelOutput = TRUE)
      if (!requireNamespace("car", quietly = TRUE)) return(NULL)
      df_loc <- anova_local()
      lv <- car::leveneTest(y ~ group, data = df_loc, center = median)
      as.data.frame(lv, stringsAsFactors = FALSE, check.names = FALSE)
    })

    ## Effect size (eta-squared)
    output$anova_label_effectsize <- renderUI({ req(anova_ready(), cancelOutput = TRUE); tags$h2("Effect size (eta-squared)") })
    output$anova_effectsize <- renderTable({
      req(anova_ready(), cancelOutput = TRUE)
      sm <- summary(anova_fit())[[1]]
      ss_between <- sm["group","Sum Sq"]
      ss_within  <- sm["Residuals","Sum Sq"]
      eta2 <- ss_between / (ss_between + ss_within)  # same as SS_between / SS_total in one-way
      data.frame(
        `η²` = round(eta2, 3),
        Magnitude = describe_eta2(eta2),
        check.names = FALSE
      )
    })

    ## Tukey HSD (only when ≥3 groups)
    output$anova_label_tukey <- renderUI({
      req(anova_ready(), cancelOutput = TRUE)
      df_loc <- anova_local()
      if (nlevels(df_loc$group) < 3) return(NULL)
      tags$h2("Post-hoc pairwise comparisons (Tukey HSD)")
    })

    output$anova_tukey <- renderTable({
      req(anova_ready(), cancelOutput = TRUE)
      df_loc <- anova_local()
      if (nlevels(df_loc$group) < 3) return(NULL)

      tk <- try(TukeyHSD(anova_fit()), silent = TRUE)
      if (inherits(tk, "try-error") || length(tk) == 0) return(NULL)

      # The term name may be "group" (since model is y ~ group); take the first element robustly.
      tk_df <- as.data.frame(tk[[1]])
      if (nrow(tk_df) == 0) return(NULL)

      tk_df$Comparison <- rownames(tk_df); rownames(tk_df) <- NULL

      # Normalize p-value col name
      cn <- names(tk_df)
      if ("p adj" %in% cn) names(tk_df)[cn == "p adj"] <- "p_adj"
      if ("p.adj" %in% cn) names(tk_df)[cn == "p.adj"] <- "p_adj"

      # Keep/rename display columns
      keep <- c("Comparison", "diff", "lwr", "upr", "p_adj")
      tk_df <- tk_df[, keep, drop = FALSE]
      names(tk_df) <- c("Comparison", "Diff", "Lower", "Upper", "Adj p-value")

      tk_df$Diff  <- round(tk_df$Diff, 3)
      tk_df$Lower <- round(tk_df$Lower, 3)
      tk_df$Upper <- round(tk_df$Upper, 3)
      tk_df$`Adj p-value` <- signif(tk_df$`Adj p-value`, 4)

      tk_df
    })

    ## Interpretation
    output$anova_label_interpretation <- renderUI({ req(anova_ready(), cancelOutput = TRUE); tags$h2("Interpretation") })
    output$anova_interpretation <- renderUI({
      req(anova_ready(), cancelOutput = TRUE)
      df_loc <- anova_local()
      sm <- summary(anova_fit())[[1]]
      df1 <- sm["group","Df"]; df2 <- sm["Residuals","Df"]
      Fv  <- sm["group","F value"]; p  <- sm["group","Pr(>F)"]
      ss_between <- sm["group","Sum Sq"]; ss_within <- sm["Residuals","Sum Sq"]
      eta2 <- ss_between / (ss_between + ss_within)
      mag  <- describe_eta2(eta2)

      # Which group has highest/lowest mean
      grp_means <- tapply(df_loc$y, df_loc$group, mean, na.rm = TRUE)
      ord <- order(grp_means, decreasing = TRUE)
      top <- names(grp_means)[ord[1]]
      bot <- names(grp_means)[ord[length(ord)]]

      # If Tukey available with ≥3 groups, count significant pairs
      sig_txt <- ""
      if (nlevels(df_loc$group) >= 3) {
        tk <- try(TukeyHSD(anova_fit()), silent = TRUE)
        if (!inherits(tk, "try-error") && length(tk) > 0) {
          df_tk <- as.data.frame(tk[[1]])
          # p-value column may be "p adj" or "p.adj"
          pvals <- df_tk[["p adj"]]
          if (is.null(pvals)) pvals <- df_tk[["p.adj"]]
          if (!is.null(pvals)) {
            n_sig <- sum(pvals < 0.05, na.rm = TRUE)
            sig_txt <- if (n_sig > 0)
              paste0(" Tukey HSD found ", n_sig, " significant pair(s) at α = 0.05.")
            else
              " Tukey HSD found no significant pairwise differences at α = 0.05."
          }
        }
      }

      HTML(sprintf(
        "The one-way ANOVA for <i>%s</i> across %d group(s) yields F(%d, %d) = %0.2f, p = %s (η² = %0.2f, %s). The highest mean is in <b>%s</b>, and the lowest in <b>%s</b>.%s",
        input$anova_num_var, nlevels(df_loc$group), df1, df2, Fv,
        ifelse(p < 0.001, "< 0.001", sprintf("%0.3f", p)),
        eta2, mag, top, bot, sig_txt
      ))
    })

    ## Violin & density
    output$anova_violin <- renderPlot({
      req(anova_ready(), cancelOutput = TRUE)
      df_loc <- anova_local()
      if (!requireNamespace("ggplot2", quietly = TRUE)) return(invisible(NULL))
      library(ggplot2)
      ggplot(df_loc, aes(x = group, y = y)) +
        geom_violin(trim = FALSE, alpha = 0.6) +
        geom_boxplot(width = 0.15, outlier.shape = NA) +
        geom_jitter(width = 0.08, height = 0, alpha = 0.5, size = 1.6) +
        labs(title = "Violin + boxplot with raw points", x = input$anova_group_var, y = input$anova_num_var)
    })

    output$anova_density <- renderPlot({
      req(anova_ready(), cancelOutput = TRUE)
      df_loc <- anova_local()
      if (!requireNamespace("ggplot2", quietly = TRUE)) return(invisible(NULL))
      library(ggplot2)
      ggplot(df_loc, aes(x = y, fill = group)) +
        geom_density(alpha = 0.4, adjust = 1) +
        labs(title = "Kernel density by group", x = input$anova_num_var, y = "Density")
    })


    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------

}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################


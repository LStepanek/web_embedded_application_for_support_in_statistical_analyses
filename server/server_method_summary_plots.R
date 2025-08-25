###############################################################################
###############################################################################
###############################################################################

## ===== Server module: Summary Plots tab (server) ============================

method_summaryPlotsServer <- function(input, output, session, my_data) {

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

}

## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

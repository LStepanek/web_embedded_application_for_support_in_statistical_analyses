###############################################################################
###############################################################################
###############################################################################

## ===== Server module: Paired t-test (server) ================================

method_pairedTTestServer <- function(input, output, session, my_data) {

  ## Paired t-test
  output$ptt_intro <- renderUI({
    intro_box(
      HTML("Paired <em>t</em>-test (within-subject / matched)"),
      "Tests whether the mean of within-pair differences equals µ (typically 0). This is for repeated measures on the same units.",
      bullets = c(
        "Use when you have two numeric measurements per subject (before/after, left/right, etc.).",
        "Assumption: differences are approximately normal; pairs are independent.",
        "Outputs: CI for mean diff, Cohen’s dz, Shapiro–Wilk on differences, boxplots (incl. differences), violin/density, and interpretation."
      )
    )
  })


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

}

## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

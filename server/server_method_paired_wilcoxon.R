###############################################################################
###############################################################################
###############################################################################

## ===== Server module: Paired Wilcoxon (signed-rank) =========================

method_pairedWilcoxonServer <- function(input, output, session, my_data) {

  ## Paired Wilcoxon (signed-rank)
  output$pwx_intro <- renderUI({
    intro_box(
      "Paired Wilcoxon signed-rank",
      "Nonparametric paired test; checks if the median of within-pair differences equals µ (typically 0).",
      bullets = c(
        "Use when you have paired/repeated measures and want a rank-based alternative to the paired t-test.",
        "Assumptions: paired differences are symmetrically distributed around the median; ignores zero differences.",
        "Outputs: Hodges–Lehmann shift + CI, matched-pairs rank-biserial effect size, Shapiro on differences (diagnostic), and plots."
      )
    )
  })


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

}

## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

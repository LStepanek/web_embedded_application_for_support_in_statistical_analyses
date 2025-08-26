###############################################################################
###############################################################################
###############################################################################

## ===== Server module: Mann–Whitney (Wilcoxon rank-sum) ======================

method_mannWhitneyServer <- function(input, output, session, my_data) {

  ## Mann–Whitney (Wilcoxon rank-sum)
  output$mw_intro <- renderUI({
    intro_box(
      "Mann–Whitney U (Wilcoxon rank-sum)",
      "Nonparametric alternative to the two-sample t-test; assesses whether values in one group tend to be larger than the other.",
      bullets = c(
        "Use for two independent groups when normality is doubtful or you prefer rank-based inference.",
        "Assumptions: independent samples; similar distribution shapes if interpreting as a location shift.",
        "Outputs: Hodges–Lehmann location shift + CI, Cliff’s ? effect size, Shapiro per group (optional), and plots."
      ),
      note = "Tip: If shapes differ strongly, interpret results as stochastic dominance rather than a pure median shift."
    )
  })


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
  # Populate selectors with mutual exclusion (numeric != grouping)
    observe({
      df <- my_data(); req(df)
      shiny::validate(need(ncol(df) > 0, "No columns in uploaded data."))

      isolate({
        numeric_vars <- names(df)[sapply(df, is.numeric)]

        # pick/keep a valid numeric selection
        sel_num <- input$mw_num_var
        if (is.null(sel_num) || !(sel_num %in% numeric_vars)) {
          sel_num <- if (length(numeric_vars)) numeric_vars[1] else NULL
        }
        updateSelectInput(session, "mw_num_var", choices = numeric_vars, selected = sel_num)

        # grouping choices exclude the selected numeric variable
        group_candidates <- setdiff(names(df), sel_num)
        sel_group <- input$mw_group_var
        if (is.null(sel_group) || !(sel_group %in% group_candidates)) {
          sel_group <- if (length(group_candidates)) group_candidates[1] else NULL
        }
        updateSelectInput(session, "mw_group_var", choices = group_candidates, selected = sel_group)
      })
    })
    
    # If numeric changes, drop it from grouping choices
    observeEvent(input$mw_num_var, {
      df <- my_data(); req(df)
      group_choices <- setdiff(names(df), input$mw_num_var)
      sel_group <- if (!is.null(input$mw_group_var) && input$mw_group_var %in% group_choices) {
        input$mw_group_var
      } else {
        if (length(group_choices)) group_choices[1] else NULL
      }
      updateSelectInput(session, "mw_group_var", choices = group_choices, selected = sel_group)
    }, ignoreInit = TRUE, priority = 100)

    # If grouping changes, exclude it from numeric choices (when that column is numeric)
    observeEvent(input$mw_group_var, {
      df <- my_data(); req(df)
      numeric_vars <- names(df)[sapply(df, is.numeric)]
      num_choices  <- setdiff(numeric_vars, input$mw_group_var)
      sel_num <- if (!is.null(input$mw_num_var) && input$mw_num_var %in% num_choices) {
        input$mw_num_var
      } else {
        if (length(num_choices)) num_choices[1] else NULL
      }
      updateSelectInput(session, "mw_num_var", choices = num_choices, selected = sel_num)
    }, ignoreInit = TRUE, priority = 100)

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

}

## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

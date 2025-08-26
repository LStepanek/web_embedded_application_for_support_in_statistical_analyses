###############################################################################
###############################################################################
###############################################################################

## ===== Server module: One-way ANOVA =========================================

method_anovaServer <- function(input, output, session, my_data) {

  ## One-way ANOVA
  output$anova_intro <- renderUI({
    intro_box(
      "One-way ANOVA",
      "Tests whether at least one group mean differs among 2+ groups using an F-test on between- vs within-group variability.",
      bullets = c(
        "Use for a numeric outcome and a factor with 2 or more levels (typically 3+).",
        "Assumptions: independent observations; approximately normal residuals; homogeneous variances across groups.",
        "Outputs: ANOVA table, group means + CIs, residual & per-group Shapiro, Bartlett/Levene, ?2 effect size, Tukey HSD (?3 groups), and plots."
      ),
      note = "If assumptions look poor (e.g., strong non-normality or heteroscedasticity), consider transformations or a nonparametric alternative (e.g., Kruskal–Wallis)."
    )
  })


  ## Populate selectors (mutually exclusive: numeric != grouping)
    observe({
      df <- my_data(); req(df)
      shiny::validate(need(ncol(df) > 0, "No columns in uploaded data."))

      isolate({
        numeric_vars <- names(df)[sapply(df, is.numeric)]

        # keep/choose a valid numeric
        sel_num <- input$anova_num_var
        if (is.null(sel_num) || !(sel_num %in% numeric_vars)) {
          sel_num <- if (length(numeric_vars)) numeric_vars[1] else NULL
        }
        updateSelectInput(session, "anova_num_var", choices = numeric_vars, selected = sel_num)

        # group choices exclude the selected numeric
        group_candidates <- setdiff(names(df), sel_num)
        sel_group <- input$anova_group_var
        if (is.null(sel_group) || !(sel_group %in% group_candidates)) {
          sel_group <- if (length(group_candidates)) group_candidates[1] else NULL
        }
        updateSelectInput(session, "anova_group_var", choices = group_candidates, selected = sel_group)
      })
    })
    
  # If numeric changes, remove it from grouping choices
    observeEvent(input$anova_num_var, {
      df <- my_data(); req(df)
      group_choices <- setdiff(names(df), input$anova_num_var)
      sel_group <- if (!is.null(input$anova_group_var) && input$anova_group_var %in% group_choices) {
        input$anova_group_var
      } else {
        if (length(group_choices)) group_choices[1] else NULL
      }
      updateSelectInput(session, "anova_group_var", choices = group_choices, selected = sel_group)
    }, ignoreInit = TRUE, priority = 100)

    # If grouping changes, exclude it from numeric choices (when that column is numeric)
    observeEvent(input$anova_group_var, {
      df <- my_data(); req(df)
      numeric_vars <- names(df)[sapply(df, is.numeric)]
      num_choices  <- setdiff(numeric_vars, input$anova_group_var)
      sel_num <- if (!is.null(input$anova_num_var) && input$anova_num_var %in% num_choices) {
        input$anova_num_var
      } else {
        if (length(num_choices)) num_choices[1] else NULL
      }
      updateSelectInput(session, "anova_num_var", choices = num_choices, selected = sel_num)
    }, ignoreInit = TRUE, priority = 100)

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

      # NEW: numeric and grouping cannot be the same column
      if (identical(input$anova_num_var, input$anova_group_var)) return(FALSE)

      y <- df[[input$anova_num_var]]; g <- as.factor(df[[input$anova_group_var]])
      if (!is.numeric(y)) return(FALSE)
      if (length(input$anova_selected_levels) < 2) return(FALSE)
      if (!all(input$anova_selected_levels %in% levels(g))) return(FALSE)
      idx <- g %in% input$anova_selected_levels
      y <- y[idx]; g <- factor(g[idx], levels = input$anova_selected_levels)
      if (sum(is.finite(y)) <= nlevels(g)) return(FALSE)  # residual df > 0
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

    # Optional: Shapiro per group (exploratory) — silent until applicable
    # output$anova_shapiro_groups <- renderTable({
      # tryCatch({
        # req(anova_ready(), cancelOutput = TRUE)

        # df_loc <- anova_local()
        # # keep complete cases only
        # df_loc <- df_loc[is.finite(df_loc$y) & !is.na(df_loc$group), , drop = FALSE]
        # if (nrow(df_loc) < 3) return(NULL)

        # g <- droplevels(df_loc$group)
        # if (nlevels(g) < 1) return(NULL)
        # lvs <- levels(g)

        # rows <- lapply(lvs, function(L) {
          # x <- df_loc$y[g == L]
          # x <- x[is.finite(x)]
          # n <- length(x)

          # if (n < 3) {
            # return(data.frame(Group = L, W = NA_real_, `p-value` = NA_real_, Note = "n < 3",
                              # check.names = FALSE))
          # }
          # if (n > 5000) {
            # return(data.frame(Group = L, W = NA_real_, `p-value` = NA_real_, Note = "n > 5000",
                              # check.names = FALSE))
          # }
          # # constant values -> Shapiro is undefined
          # rng <- range(x)
          # if (!is.finite(rng[1]) || !is.finite(rng[2]) || (rng[2] - rng[1] == 0)) {
            # return(data.frame(Group = L, W = NA_real_, `p-value` = NA_real_, Note = "constant values",
                              # check.names = FALSE))
          # }

          # s <- try(stats::shapiro.test(x), silent = TRUE)
          # if (inherits(s, "try-error")) {
            # return(data.frame(Group = L, W = NA_real_, `p-value` = NA_real_, Note = "not computed",
                              # check.names = FALSE))
          # }

          # data.frame(
            # Group    = L,
            # W        = round(unname(s$statistic), 3),
            # `p-value`= signif(s$p.value, 4),
            # Note     = "",
            # check.names = FALSE
          # )
        # })

        # out <- do.call(rbind, rows)

        # # If no group produced a computable result, hide the table
        # computable <- is.finite(out$W) & is.finite(out$`p-value`)
        # if (!any(computable, na.rm = TRUE)) return(NULL)

        # # Otherwise show the table (including rows with notes for transparency)
        # out
      # }, error = function(e) {
        # NULL  # hide on any unexpected error
      # })
    # })

  # Homogeneity: Bartlett (base) — silent until truly applicable
    output$anova_bartlett <- renderTable({
      tryCatch({
        req(anova_ready(), cancelOutput = TRUE)

        df_loc <- anova_local()
        # keep complete rows
        df_loc <- df_loc[is.finite(df_loc$y) & !is.na(df_loc$group), , drop = FALSE]
        if (nrow(df_loc) < 3) return(NULL)

        # need ≥2 groups with n ≥ 2 AND non-zero variance
        by_stats <- tapply(df_loc$y, droplevels(df_loc$group), function(x) {
          c(n = sum(is.finite(x)), var = stats::var(x, na.rm = TRUE))
        })
        by_stats <- do.call(rbind, by_stats)

        if (is.null(by_stats)) return(NULL)
        if (sum(by_stats[, "n"] >= 2, na.rm = TRUE) < 2) return(NULL)
        if (sum(by_stats[, "var"] > 0,  na.rm = TRUE) < 2) return(NULL)

        bt <- suppressWarnings(stats::bartlett.test(y ~ group, data = df_loc))
        if (inherits(bt, "htest")) {
          data.frame(
            `Bartlett K-squared` = round(unname(bt$statistic), 3),
            df                   = unname(bt$parameter),
            `p-value`            = signif(bt$p.value, 4),
            check.names = FALSE
          )
        } else {
          NULL
        }
      }, error = function(e) {
        # Hide output on any error
        NULL
      })
    })

  # Homogeneity: Levene (if car is available) — silent until truly applicable
    output$anova_levene <- renderTable({
      tryCatch({
        req(anova_ready(), cancelOutput = TRUE)
        if (!requireNamespace("car", quietly = TRUE)) return(NULL)

        df_loc <- anova_local()
        # keep complete rows only
        df_loc <- df_loc[is.finite(df_loc$y) & !is.na(df_loc$group), , drop = FALSE]
        if (nrow(df_loc) < 3) return(NULL)

        g <- droplevels(df_loc$group)
        if (nlevels(g) < 2) return(NULL)

        # need residual df > 0: total non-missing > number of groups
        if (sum(is.finite(df_loc$y)) <= nlevels(g)) return(NULL)

        # check that deviations are not all zero (otherwise F is undefined)
        med_by_group <- tapply(df_loc$y, g, stats::median, na.rm = TRUE)
        dev <- abs(df_loc$y - med_by_group[as.integer(g)])
        if (all(!is.finite(dev)) || all(dev == 0, na.rm = TRUE)) return(NULL)

        # run Levene (center = median), suppress harmless warnings
        lv <- suppressWarnings(car::leveneTest(y ~ g,
          data = data.frame(y = df_loc$y, g = g),
          center = median
        ))

        dflv <- as.data.frame(lv, stringsAsFactors = FALSE, check.names = FALSE)
        # add a 'Term' column for readability
        if (!is.null(rownames(dflv))) {
          dflv <- cbind(Term = rownames(dflv), dflv, row.names = NULL, check.names = FALSE)
        }
        dflv
      }, error = function(e) {
        # Hide output on any error
        NULL
      })
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
  
  ## Notice if invalid selection
  output$anova_selection_notice <- renderUI({
      # 1) Wait until data & inputs are available (no banner before that)
      df <- my_data()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      if (is.null(input$anova_num_var) || is.null(input$anova_group_var)) return(NULL)

      # 2) If the group-levels selector hasn't been rendered yet, don't show anything
      if (is.null(input$anova_selected_levels)) return(NULL)

      # 3) Make sure inputs point to real columns and levels exist
      if (!(input$anova_num_var %in% names(df)) || !(input$anova_group_var %in% names(df))) return(NULL)
      g <- try(as.factor(df[[input$anova_group_var]]), silent = TRUE)
      if (inherits(g, "try-error")) return(NULL)
      if (!all(input$anova_selected_levels %in% levels(g))) return(NULL)

      # 4) Now, and only now, show the notice if too few groups are selected
      if (length(input$anova_selected_levels) < 2) {
        tags$div(
          class = "alert alert-warning",
          tags$b("Select at least two groups"),
          tags$span(" – please choose two or more groups in the sidebar to run ANOVA.")
        )
      } else {
        NULL
      }
    })

}

## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

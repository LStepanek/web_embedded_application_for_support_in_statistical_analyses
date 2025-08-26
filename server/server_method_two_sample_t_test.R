###############################################################################
###############################################################################
###############################################################################

## ===== Server module: Two-sample t-test =====================================

method_twoSampleTTestServer <- function(input, output, session, my_data) {

  output$ttest_intro <- renderUI({
    intro_box(
      HTML("Two-sample <em>t</em>-test (independent groups)"),
      "Tests whether two independent group means differ (H0: mean difference = µ, typically 0). Uses Welch’s t by default (no equal-variance assumption).",
      bullets = c(
        "Use for a numeric outcome and a 2-level grouping variable.",
        "Assumptions: independent observations; roughly normal within groups (or large n).",
        "Outputs here include: CI for mean diff, Cohen’s d, Shapiro–Wilk per group, box/violin/density plots, and a one-line interpretation."
      )
    )
  })

  ## logic of the two-sample t-test -----------------------------------------

  output$data_ready <- reactive({
      !is.null(my_data()) && nrow(my_data()) > 0 && ncol(my_data()) > 0
  })
  outputOptions(output, "data_ready", suspendWhenHidden = FALSE)

  # Populate selectors with mutual exclusion (numeric != grouping)
    observe({
      df <- my_data()
      req(df)
      shiny::validate(need(ncol(df) > 0, "No columns in uploaded data."))

      isolate({
        numeric_vars <- names(df)[sapply(df, is.numeric)]

        # choose/keep a valid numeric selection
        sel_num <- input$ttest_num_var
        if (is.null(sel_num) || !(sel_num %in% numeric_vars)) {
          sel_num <- if (length(numeric_vars)) numeric_vars[1] else NULL
        }
        updateSelectInput(session, "ttest_num_var", choices = numeric_vars, selected = sel_num)

        # grouping choices exclude the selected numeric var
        group_candidates <- setdiff(names(df), sel_num)
        sel_group <- input$ttest_group_var
        if (is.null(sel_group) || !(sel_group %in% group_candidates)) {
          sel_group <- if (length(group_candidates)) group_candidates[1] else NULL
        }
        updateSelectInput(session, "ttest_group_var", choices = group_candidates, selected = sel_group)
      })
    })
    
    # If numeric changes, drop it from grouping choices
    observeEvent(input$ttest_num_var, {
      df <- my_data(); req(df)
      group_choices <- setdiff(names(df), input$ttest_num_var)
      sel_group <- if (!is.null(input$ttest_group_var) && input$ttest_group_var %in% group_choices) {
        input$ttest_group_var
      } else {
        if (length(group_choices)) group_choices[1] else NULL
      }
      updateSelectInput(session, "ttest_group_var", choices = group_choices, selected = sel_group)
    }, ignoreInit = TRUE, priority = 100)

    # If grouping changes, exclude it from numeric choices (if that col is numeric)
    observeEvent(input$ttest_group_var, {
      df <- my_data(); req(df)
      numeric_vars <- names(df)[sapply(df, is.numeric)]
      num_choices  <- setdiff(numeric_vars, input$ttest_group_var)
      sel_num <- if (!is.null(input$ttest_num_var) && input$ttest_num_var %in% num_choices) {
        input$ttest_num_var
      } else {
        if (length(num_choices)) num_choices[1] else NULL
      }
      updateSelectInput(session, "ttest_num_var", choices = num_choices, selected = sel_num)
    }, ignoreInit = TRUE, priority = 100)

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

}

## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

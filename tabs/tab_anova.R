###############################################################################
###############################################################################
###############################################################################

## ANOVA tab ------------------------------------------------------------------

tab_anova <- tabPanel(
  title = "Analysis of variance (ANOVA)",
  value = "tab_anova",
  HTML("<h1>Analysis of variance (ANOVA)</h1>"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "output.data_ready == true",
        selectInput("anova_num_var",   "Select numeric variable:", choices = NULL),
        selectInput("anova_group_var", "Select grouping variable:", choices = NULL),
        uiOutput("anova_group_levels_ui"),
        selectInput(
          "anova_alt", "Alternative hypothesis:",
          choices = c("two.sided"), selected = "two.sided"  # ANOVA uses two-sided F
        )
      )
    ),
    mainPanel(
	  uiOutput("anova_intro"),

      uiOutput("anova_selection_notice"),
      uiOutput("anova_h0_statement"),

      uiOutput("anova_label_table"),
      tableOutput("anova_table"),

      uiOutput("anova_label_boxplot"),
      plotOutput("anova_boxplot", height = "320px"),

      uiOutput("anova_label_means_ci"),
      tableOutput("anova_means_ci"),

      uiOutput("anova_label_assumptions"),
      tableOutput("anova_shapiro_resid"),
      tableOutput("anova_shapiro_groups"),
      tableOutput("anova_bartlett"),
      tableOutput("anova_levene"),

      uiOutput("anova_label_effectsize"),
      tableOutput("anova_effectsize"),

      uiOutput("anova_label_tukey"),
      tableOutput("anova_tukey"),

      uiOutput("anova_label_interpretation"),
      uiOutput("anova_interpretation"),

      HTML("<br><br>"),
      plotOutput("anova_violin",  height = "320px"),
      plotOutput("anova_density", height = "320px")
    )
  )
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

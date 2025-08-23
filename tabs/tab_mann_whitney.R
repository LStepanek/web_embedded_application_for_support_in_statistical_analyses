###############################################################################
###############################################################################
###############################################################################

## Mann-Whitney test tab ------------------------------------------------------

tab_mann_whitney <- tabPanel(
  title = "Mann–Whitney test",
  value = "tab_mann_whitney",
  HTML("<h1>Mann–Whitney test</h1>"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "output.data_ready == true",
        selectInput("mw_num_var",   "Select numeric variable:", choices = NULL),
        selectInput("mw_group_var", "Select grouping variable:", choices = NULL),
        uiOutput("mw_group_levels_ui"),
        selectInput(
          "mw_alt", "Alternative hypothesis:",
          choices = c("two.sided", "less", "greater"), selected = "two.sided"
        ),
        numericInput(
          "mw_mu",
          HTML("Null hypothesis location shift (&#956;):"),
          value = 0, step = 0.01
        )
      )
    ),
    mainPanel(
      uiOutput("mw_selection_notice"),
      
      uiOutput("mw_h0_statement"),
      tableOutput("mw_result"),

      uiOutput("mw_label_boxplot"),
      plotOutput("mw_boxplot", height = "320px"),

      uiOutput("mw_label_means_ci"),
      tableOutput("mw_means_ci"),

      uiOutput("mw_label_shapiro"),
      tableOutput("mw_shapiro"),

      uiOutput("mw_label_effectsize"),
      tableOutput("mw_effectsize"),
      
      uiOutput("mw_label_interpretation"),
      uiOutput("mw_interpretation"),

      HTML("<br><br>"),
      plotOutput("mw_violin",  height = "320px"),
      plotOutput("mw_density", height = "320px")
    )
  )
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

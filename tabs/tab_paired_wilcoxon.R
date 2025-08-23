###############################################################################
###############################################################################
###############################################################################

## paired Wilcoxon test tab ---------------------------------------------------

tab_paired_wilcoxon <- tabPanel(
  title = "Paired Wilcoxon test",
  value = "tab_paired_wilcoxon",
  HTML("<h1>Paired Wilcoxon test</h1>"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "output.data_ready == true",
        selectInput("pwx_var1", "Select first numeric variable:",  choices = NULL),
        selectInput("pwx_var2", "Select second numeric variable:", choices = NULL),
        selectInput(
          "pwx_alt", "Alternative hypothesis:",
          choices = c("two.sided", "less", "greater"), selected = "two.sided"
        ),
        numericInput(
          "pwx_mu",
          HTML("Null hypothesis location shift (&#956;):"),
          value = 0, step = 0.01
        )
      )
    ),
    mainPanel(
	  uiOutput("pwx_intro"),
	  
      uiOutput("pwx_h0_statement"),
      tableOutput("pwx_result"),

      uiOutput("pwx_label_boxplot"),
      plotOutput("pwx_boxplot", height = "320px"),

      uiOutput("pwx_label_boxplot_diff"),
      plotOutput("pwx_boxplot_diff", height = "280px"),

      uiOutput("pwx_label_means_ci"),
      tableOutput("pwx_means_ci"),

      uiOutput("pwx_label_shapiro"),
      tableOutput("pwx_shapiro"),

      uiOutput("pwx_label_effectsize"),
      tableOutput("pwx_effectsize"),
      
      uiOutput("pwx_label_interpretation"),
      uiOutput("pwx_interpretation"),

      HTML("<br><br>"),
      plotOutput("pwx_violin",  height = "320px"),
      plotOutput("pwx_density", height = "320px")
    )
  )
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

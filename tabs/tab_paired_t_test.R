###############################################################################
###############################################################################
###############################################################################

## paired t-test tab ----------------------------------------------------------

tab_paired_t_test <- tabPanel(
  title = HTML("Paired <i>t</i>-test"),
  value = "tab_paired_t_test",
  HTML("<h1>Paired <i>t</i>-test</h1>"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "output.data_ready == true",
        selectInput("ptt_var1", "Select first numeric variable:", choices = NULL),
        selectInput("ptt_var2", "Select second numeric variable:", choices = NULL),
        selectInput(
          "ptt_alt", "Alternative hypothesis:",
          choices = c("two.sided", "less", "greater"),
          selected = "two.sided"
        ),
        numericInput(
          "ptt_mu",
          HTML("Null hypothesis mean difference (&#956;):"),
          value = 0, step = 0.01
        )
      )
    ),
    mainPanel(
	  uiOutput("ptt_intro"),
      uiOutput("ptt_h0_statement"),
      tableOutput("ptt_result"),
      uiOutput("ptt_label_boxplot"),
      plotOutput("ptt_boxplot", height = "320px"),
      uiOutput("ptt_label_boxplot_diff"),
      plotOutput("ptt_boxplot_diff", height = "280px"),
      uiOutput("ptt_label_means_ci"),
      tableOutput("ptt_means_ci"),
      uiOutput("ptt_label_shapiro"),
      tableOutput("ptt_shapiro"),
      uiOutput("ptt_label_effectsize"),
      tableOutput("ptt_effectsize"),
      uiOutput("ptt_label_interpretation"),
      uiOutput("ptt_interpretation"),
      HTML("<br><br>"),
      plotOutput("ptt_violin",  height = "320px"),
      plotOutput("ptt_density", height = "320px")
    )
  )
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

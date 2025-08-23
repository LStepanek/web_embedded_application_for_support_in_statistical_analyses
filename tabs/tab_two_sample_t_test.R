###############################################################################
###############################################################################
###############################################################################

## method 1 tab ---------------------------------------------------------------

tab_two_sample_t_test <- tabPanel(
    
    title = "Two-sample t-test",
    
    value = "tab_two_sample_t_test",
    
    HTML("<h1>Two-sample <i>t</i>-test</h1>"),
    
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(
                condition = "output.data_ready == true",
                selectInput(
                    "ttest_num_var",
                    "Select numeric variable:",
                    choices = NULL
                ),
                selectInput(
                    "ttest_group_var",
                    "Select grouping variable:",
                    choices = NULL
                ),
                uiOutput("ttest_group_levels_ui"),
                selectInput(
                    "ttest_alt",
                    "Alternative hypothesis:",
                    choices = c("two.sided", "less", "greater"),
                    selected = "two.sided"
                ),
                numericInput(
                    "ttest_mu",
                    HTML("Null hypothesis mean difference (&#956;):"),
                    value = 0,
                    step = 0.01
                )
            )
        ),
        mainPanel(
            uiOutput("ttest_h0_statement"),
            tableOutput("ttest_result"),
            plotOutput("ttest_boxplot"),
            tableOutput("ttest_means_ci"),
            tableOutput("ttest_shapiro"),
            tableOutput("ttest_effectsize"),
            HTML("<br>"),
            HTML("<br>"),
            uiOutput("ttest_interpretation"),
            HTML("<br>"),
            HTML("<br>"),
            plotOutput("ttest_violin", height = "320px")
        )
    )
    
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

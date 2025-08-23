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
            uiOutput("ttest_intro"),
            uiOutput("ttest_h0_statement"),
            tableOutput("ttest_result"),
            plotOutput("ttest_boxplot"),
            HTML("<br>"),
            uiOutput("ttest_label_means_ci"),
            tableOutput("ttest_means_ci"),
            HTML("<br>"),
            uiOutput("ttest_label_shapiro"),
            tableOutput("ttest_shapiro"),
            HTML("<br>"),
            uiOutput("ttest_label_effectsize"),
            tableOutput("ttest_effectsize"),
            HTML("<br>"),
            uiOutput("ttest_label_interpretation"),
            uiOutput("ttest_interpretation"),
            HTML("<br>"),
            HTML("<br>"),
            HTML("<br>"),
            plotOutput("ttest_violin", height = "320px"),
            plotOutput("ttest_density", height = "320px")
        )
    )
    
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

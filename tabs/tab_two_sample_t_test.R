###############################################################################
###############################################################################
###############################################################################

## method 1 tab ---------------------------------------------------------------

tab_two_sample_t_test <- tabPanel(
    
    title = "Two-sample t-test",
    
    value = "tab_two_sample_t_test",
    
    h1(HTML("Two-sample <i>t</i>-test")),
    
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
                    "Select grouping variable (2 levels):",
                    choices = NULL
                ),
                selectInput(
                    "ttest_alt",
                    "Alternative hypothesis:",
                    choices = c(
                        "two.sided",
                        "less",
                        "greater"
                    ),
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
            tableOutput("ttest_result"),
            plotOutput("ttest_boxplot")
        )
    ),
    
    
    ## ------------------------------------------------------------------------
    
    HTML("<br />"),
    HTML("<br />"),
    HTML("<br />"),
    HTML("<br />"),
    HTML("<br />"),
    HTML("<br />")
    
    
    ## ------------------------------------------------------------------------
    
    
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################






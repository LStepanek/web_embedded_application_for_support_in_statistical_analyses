###############################################################################
###############################################################################
###############################################################################

## method 1 tab ---------------------------------------------------------------

tab_summary_plots <- tabPanel(
    
    title = "Summary plots",
    
    value = "tab_summary_plots",
    
    h1("Summary plots"),
    
    ## centered plots section
    fluidRow(
        column(width = 6, offset = 3, uiOutput("plot_section_label"))
    ),
    
    ## centered plots
    fluidRow(
        column(width = 6, offset = 3, uiOutput("variable_plots"))
    ),
        
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

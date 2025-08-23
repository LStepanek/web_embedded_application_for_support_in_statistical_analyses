###############################################################################
###############################################################################
###############################################################################

## method 1 tab ---------------------------------------------------------------

tab_summary_plots <- tabPanel(
    
    title = "Summary plots",
    
    value = "tab_summary_plots",
    
    h1("Summary plots"),
    
    uiOutput("summary_intro"),
    
    # Center helpers (add once in the tab)
    tags$head(tags$style(HTML("
      .centered-table { display:flex; justify-content:center; }
      .dl-row { display:flex; justify-content:center; margin-top:8px; }
    "))),
    
    uiOutput("summary_numeric_block"),
    uiOutput("summary_categorical_block"),
    uiOutput("summary_date_block"),

    ## centered plots section
    fluidRow(
        column(width = 6, offset = 3, uiOutput("plot_section_label"))
    ),
    
    ## centered plots
    fluidRow(
        column(width = 6, offset = 3, uiOutput("variable_plots"))
    )
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

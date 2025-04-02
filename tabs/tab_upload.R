###############################################################################
###############################################################################
###############################################################################

## upload tab -----------------------------------------------------------------

tab_upload <- tabPanel(
    
    title = "Data upload",
    
    value = "tab_upload",
    
    h1("Data upload"),
    
    fluidRow(
        
        column(
            width = 3,
            fileInput(
                "file_upload",
                "Choose a .csv file",
                accept = c(".csv")
            )
        ),
        
        column(
            width = 2,
            checkboxInput(
                "header",
                "Does it contain a header?",
                TRUE
            ),
            checkboxInput(
                "use_first_col_as_rownames",
                "Use first column as row names!",
                FALSE
            )
        ),
        
        column(
            width = 2,
            radioButtons(
                "separator",
                "Separator", 
                choices = c(
                    "Comma" = ",",
                    "Semicolon" = ";",
                    "Tab" = "\t"
                ),
                selected = ","
            )
        ),
        
        column(
            width = 3,
            textInput(
                "col_types",
                "Column data types",
                placeholder = "e.g., NSSD"
            )
        ),
        
        column(
            width = 2,
            checkboxInput(
                "use_builtin",
                "Use built-in dataset?",
                FALSE
            ),
            conditionalPanel(
                condition = "input.use_builtin == true",
                selectInput(
                    "builtin_dataset",
                    "Select dataset",
                    choices = c(
                        "mtcars",
                        "airquality",
                        "npk"
                    )
                )
            )
        )
        
    ),
    
    
    ## dynamically render labels & tables -------------------------------------
    
    fluidRow(
        column(
            width = 12,
            uiOutput("data_preview_label"),
            DTOutput("data_table")
        )
    ),
    
    ## upload summary table ---------------------------------------------------
    
    fluidRow(
        column(
            width = 6,
            offset = 3,
            uiOutput("upload_summary_label"),
            DTOutput("upload_summary_table")
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






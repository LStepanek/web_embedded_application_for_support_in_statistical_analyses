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
                label = HTML("Choose a data file<br/><small>Supported formats: ", paste(ALLOWED_DATA_FILES, collapse = ", "), "</small>"),
                accept = paste(ALLOWED_DATA_FILES, collapse = ", ")
            ),

            checkboxInput(
                "use_builtin",
                label = HTML("<b>Or use built-in dataset?</b>"),
                FALSE
            ),
            conditionalPanel(
                condition = "input.use_builtin == true",
                selectInput(
                    "builtin_dataset",
                    "Select dataset",
                    choices = NULL
                )
            )
        ),

        column(
            width = 3,
            div(id = "data_options",
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
            )
        ),
        
        column(
            width = 3,
            radioButtons(
                "col_separator",
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
                placeholder = "e.g., NSSLD"
            )
        ),
        
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
    
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

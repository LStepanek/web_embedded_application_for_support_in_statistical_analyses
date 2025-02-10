###############################################################################
###############################################################################
###############################################################################

## I am loading packages ------------------------------------------------------

for(
    my_package in c(
        
        "shiny",
        "shinyjs",
        "DT"
        
    )
){
    
    if(
        !(
            my_package %in% rownames(installed.packages())
        )
    ){
        
        install.packages(
            my_package,
            dependencies = TRUE,
            repos = "http://cran.us.r-project.org"
        )
        
    }
    
    library(
        my_package,
        character.only = TRUE
    )
    
}


## I am loading a module with user-defined functions --------------------------

source(
    file = "functions.R",
    echo = FALSE,
    encoding = "UTF-8"
)


## I am loading a module with global options ----------------------------------

source(
    file = "global.R",
    echo = FALSE,
    encoding = "UTF-8"
)


## ----------------------------------------------------------------------------

###############################################################################

## I am creating a backend of the application ---------------------------------

my_server <- function(
    
    input,
    output,
    session
    
){
    
    ## I am introducing a counter ---------------------------------------------
    
    output$my_counter <- renderText({
        
        if(
            ! file.exists("my_counter.Rdata")
        ){
            
            my_counter <- 0
            
        }else{
            
            load(file = "my_counter.Rdata")
            
        }
        
        my_counter <- my_counter + 1
        
        save(
            my_counter,
            file = "my_counter.Rdata"
        )
        
        paste(
            "Count of visits: ",
            my_counter,
            sep = ""
        )
        
    })
    
    
    ## logic of user of inbuilt data upload -----------------------------------
    
    # reactive dataset function -----------------------------------------------
    
    my_data <- reactive({
        
        # use built-in dataset if selected ------------------------------------
        
        if(
            input$use_builtin
        ){
            
            dataset_path <- file.path(
                "datasets",
                paste(
                input$builtin_dataset,
                ".csv",
                sep = ""
                )
            )
            
            if(
                file.exists(dataset_path)
            ){
                
                return(
                    read.csv(
                        dataset_path,
                        header = TRUE
                    )
                )
                
            }else{
                
                showNotification(
                    "Dataset not found!",
                    type = "error"
                )
                return(NULL)
                
            }
            
        }
        
        # check if file is uploaded -------------------------------------------
        
        req(input$file_upload)
        
        data <- read.csv(
            input$file_upload$datapath,
            header = input$header,
            sep = input$separator
        )
        
        # convert column data types based on user input -----------------------
        
        col_types <- unlist(
            strsplit(
                input$col_types,
                ""
            )
        )
        
        if(
            length(col_types) == ncol(data)
        ){
            
            data <- mapply(
                function(col, type){
                    switch(
                        type,
                        "N" = as.numeric(col),
                        "S" = as.character(col),
                        "D" = as.Date(col),
                        "L" = as.logical(col),
                        col
                    )
                },
                data,
                col_types,
                SIMPLIFY = FALSE
            )
            data <- as.data.frame(data)
            
        }
        
        return(data)
        
    })

    # render Data Table only if dataset exists --------------------------------
    
    output$data_table <- renderDT({
        
        req(my_data())
        
        datatable(
            my_data(), 
            options = list(
                pageLength = 10, 
                autoWidth = TRUE, 
                searchHighlight = TRUE, 
                dom = 'Bfrtip'
            )
        )
        
    })
    
    # dynamically show "data preview" label only if a dataset is available ----
    
    output$data_preview_label <- renderUI({
        
        req(my_data())
        h4("Data preview")
        
    })
    
    
    ## ------------------------------------------------------------------------
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################






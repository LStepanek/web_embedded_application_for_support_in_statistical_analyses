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
                
                data <- read.csv(
                    dataset_path,
                    header = input$header,
                    sep = input$separator
                )
                
                # if row names checkbox is checked, use first column
                # as row names ------------------------------------------------
                
                if(
                    input$use_first_col_as_rownames & ncol(data) > 1
                ){
                    rownames(data) <- data[, 1]
                    data <- data[, -1, drop = FALSE]
                }
                
                # convert column data types based on user input ---------------
                
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
                
                return(
                    data
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
        
        # if row names checkbox is checked, use first column as row names -----
        
        if(
            input$use_first_col_as_rownames & ncol(data) > 1
        ){
            rownames(data) <- data[, 1]       # set first column as row names
            data <- data[, -1, drop = FALSE]  # remove first column
        }
        
        
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
    
    # reactive function to get upload summary ---------------------------------
    
    upload_summary <- reactive({
        
        req(my_data())
        data <- my_data()
        
        col_info <- data.frame(
            "variable" = colnames(data),
            "data type" = sapply(data, function(col) class(col)[1]),
            stringsAsFactors = FALSE
        )
        
        col_info
        
    })
    
    # render upload summary table ---------------------------------------------
    
    output$upload_summary_table <- renderDT({
        
        req(upload_summary())
        datatable(
            upload_summary(),
            options = list(
                pageLength = 10, 
                autoWidth = TRUE, 
                searchHighlight = TRUE, 
                dom = 'Bfrtip'
            ),
            rownames = FALSE,
            colnames = c("variable", "data type")
        )
        
    })
    
    # dynamically show "data preview" label only if a dataset is available ----
    
    output$data_preview_label <- renderUI({
        
        req(my_data())
        h4("Data preview")
        
    })
    
    output$upload_summary_label <- renderUI({
        
        req(upload_summary())
        h4("Variable data types")
        
    })
    
    output$plot_section_label <- renderUI({
        
        req(my_data())
        h4("Variable distributions")
        
    })
    
    
    # reactive function to get variable types ---------------------------------
    
    variable_types <- reactive({
        
        req(my_data())
        data <- my_data()
        data.frame(
            Variable = colnames(data),
            Data_Type = sapply(data, function(col) class(col)[1]),
            stringsAsFactors = FALSE
        )
        
    })
    
    
    # Generate plots and download buttons for each variable dynamically -------
    
    output$variable_plots <- renderUI({
        
        req(my_data())
        data <- my_data()
        var_types <- variable_types()
        
        plot_list <- lapply(
            seq_along(var_types$Variable),
            function(i){
                var_name <- var_types$Variable[i]
                plot_output_id <- paste0("plot_", var_name)
                download_button_id <- paste0("download_", var_name)
                
                tagList(
                    div(
                        style = "margin-top: 20px;", 
                        h5(var_name),
                        plotOutput(
                            outputId = plot_output_id,
                            height = "300px"
                        ),
                        downloadButton(
                            download_button_id,
                            "Download Plot",
                            class = "btn btn-primary"
                        )
                    )
                )
            }
        )
        
        do.call(tagList, plot_list)
        
    })
    
    # generate individual plots and download handlers -------------------------
    
    observe({
        
        req(my_data())
        data <- my_data()
        var_types <- variable_types()
        
        for(
            i in seq_along(var_types$Variable)
        ){
            local({
                var_name <- var_types$Variable[i]
                var_type <- var_types$Data_Type[i]
                plot_output_id <- paste0("plot_", var_name)
                download_button_id <- paste0("download_", var_name)
                
                output[[plot_output_id]] <- renderPlot({
                    
                    req(data[[var_name]])
                    
                    if(
                        var_type %in% c("integer", "numeric", "Date")
                    ){
                        
                        # histogram for numeric and date variables
                        hist(
                            data[[var_name]], 
                            col = "lightblue", 
                            border = "black", 
                            main = paste("histogram of", var_name), 
                            xlab = var_name, 
                            ylab = "frequency"
                        )
                        
                    }else if(
                        var_type %in% c("character", "logical", "factor")
                    ){
                        
                        # barplot for categorical variables
                        counts <- table(data[[var_name]])
                        
                        barplot(
                            counts, 
                            col = "orange", 
                            border = "black", 
                            main = paste("barplot of", var_name), 
                            xlab = var_name, 
                            ylab = "count",
                            las = 2
                        )
                        
                    }
                    
                })
                
                # download handler for each plot                
                output[[download_button_id]] <- downloadHandler(
                    
                    filename = function(){
                        paste("plot_", var_name, ".png", sep = "")
                    },
                    content = function(file){
                        png(file, width = 800, height = 600)
                        if(
                            var_type %in% c("integer", "numeric", "Date")
                        ){
                            
                            hist(
                                data[[var_name]], 
                                col = "lightblue", 
                                border = "black", 
                                main = paste("histogram of", var_name), 
                                xlab = var_name, 
                                ylab = "frequency"
                            )
                            
                        }else if(
                            var_type %in% c("character", "logical", "factor")
                        ){
                            
                            counts <- table(data[[var_name]])
                            
                            barplot(
                                counts, 
                                col = "orange", 
                                border = "black", 
                                main = paste("barplot of", var_name), 
                                xlab = var_name, 
                                ylab = "count",
                                las = 2
                            )
                            
                        }
                        
                        dev.off()
                        
                    }
                    
                )
                
            })
            
        }
        
    })
    
    
    ## ------------------------------------------------------------------------
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################






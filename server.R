###############################################################################
###############################################################################
###############################################################################


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


## I am loading packages ------------------------------------------------------

# List of packages to be installed and loaded
packages_to_install <- c("shiny", "shinyjs", "DT", "readxl")

# Call the function to install and load the packages
install_and_load_packages(packages_to_install)


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
    my_data <- reactive({
        
        # built-in dataset
        if (input$use_builtin) {
            dataset_path <- file.path(
                SAMPLE_DATASETS_DIR,
                paste(input$builtin_dataset)
            )
            
            tryCatch({
                if (file.exists(dataset_path)) {
                    file_ext <- tools::file_ext(dataset_path)
                    
                    if (file_ext %in% c("xls", "xlsx")) {
                        data <- readxl::read_excel(dataset_path)
                    } else {
                        data <- read.csv(dataset_path, header = input$header, sep = input$col_separator)
                    }

                    # rownames
                    if (input$use_first_col_as_rownames && ncol(data) > 1) {
                        rownames(data) <- data[[1]]
                        data <- data[, -1, drop = FALSE]
                    }

                    # column types
                    col_types <- unlist(strsplit(input$col_types, ""))
                    if (length(col_types) == ncol(data)) {
                        data <- mapply(function(col, type) {
                            switch(type,
                                   "N" = as.numeric(col),
                                   "S" = as.character(col),
                                   "D" = as.Date(col),
                                   "L" = as.logical(col),
                                   col)
                        }, data, col_types, SIMPLIFY = FALSE)
                        data <- as.data.frame(data)
                    }

                    toastr_success(paste("Built-in dataset", sQuote(input$builtin_dataset), "loaded successfully!"))
                    return(data)
                } else {
                    toastr_error(paste("Built-in dataset", sQuote(input$builtin_dataset), "not found!"))
                    showNotification(paste("Built-in dataset", sQuote(input$builtin_dataset), "not found!"), type = "error")
                    return(NULL)
                }
            }, error = function(e) {
                toastr_error(paste("Error reading built-in dataset:", e$message))
                showNotification(paste("Error reading built-in dataset: ", e$message), type = "error")
                return(NULL)
            })
        }

        # uploaded file
        req(input$file_upload)

        tryCatch({
            file_ext <- tools::file_ext(input$file_upload$name)
            
            if (file_ext %in% c("xls", "xlsx")) {
                data <- readxl::read_excel(input$file_upload$datapath)
            } else {
                data <- read.csv(input$file_upload$datapath, header = input$header, sep = input$col_separator)
            }

            num_rows <- nrow(data)
            if (num_rows <= 1) {
                toastr_warning("The file appears to be empty or contains only header data. Please check the content of the file.")
            }

            if (input$use_first_col_as_rownames && ncol(data) > 1) {
                rownames(data) <- data[[1]]
                data <- data[, -1, drop = FALSE]
            }

            col_types <- unlist(strsplit(input$col_types, ""))
            if (length(col_types) == ncol(data)) {
                data <- mapply(function(col, type) {
                    switch(type,
                           "N" = as.numeric(col),
                           "S" = as.character(col),
                           "D" = as.Date(col),
                           "L" = as.logical(col),
                           col)
                }, data, col_types, SIMPLIFY = FALSE)
                data <- as.data.frame(data)
            }

            toastr_success(paste("File", sQuote(input$file_upload$name), "loaded successfully!"))
            return(data)
        }, error = function(e) {
            toastr_error(paste("Error reading file:", e$message))
            showNotification(paste("Error reading file: ", e$message), type = "error")
            return(NULL)
        })
    })


    # render Data Table only if dataset exists --------------------------------

    output$data_table <- renderDT({
        req(my_data())

        # Nahradíme NA hodnoty řetězcem "N/A"
        data_display <- my_data()
        data_display[is.na(data_display)] <- "N/A"

        datatable(
            data_display,
            options = list(
                pageLength = 10,
                lengthMenu = c(10, 25, 50, 100, 250),
                autoWidth = TRUE,
                searchHighlight = TRUE,
                dom = 'Blfrtip',
                rowCallback = JS("highlightMissingCells")
            )
        )
    })


    observeEvent(input$use_builtin, {
      if (input$use_builtin) {
        # Load all files from the directory
        all_files <- list.files(SAMPLE_DATASETS_DIR, full.names = FALSE)
        
        # Filter files based on the allowed extensions in ALLOWED_DATA_FILES
        file_names <- all_files[sapply(all_files, function(f) {
          any(sapply(ALLOWED_DATA_FILES, function(ext) grepl(paste0(ext, "$"), f)))
        })]
        
        # Remove extensions for display purposes (only for display, not value)
        display_names <- sub(paste0("(", paste(ALLOWED_DATA_FILES, collapse = "|"), ")$"), "", file_names)
        
        # Create named vector: display_name = value_with_extension
        named_choices <- setNames(file_names, display_names)
        
        # Update the selectInput with display names and actual file names as values
        updateSelectInput(session, "builtin_dataset", choices = named_choices)
      } else {
        # If checkbox is not selected, clear the selectInput
        updateSelectInput(session, "builtin_dataset", choices = NULL)
      }
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
        h2("Data preview")
        
    })
    
    output$upload_summary_label <- renderUI({
        
        req(upload_summary())
        h2("Variable data types")
        
    })
    
    output$plot_section_label <- renderUI({
        
        req(my_data())
        h2("Variable distributions")
        
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
    
    ## logic of the two-sample t-test -----------------------------------------
    
    output$data_ready <- reactive({
        ready <- !is.null(my_data()) && nrow(my_data()) > 0 && ncol(my_data()) > 0
        session$sendCustomMessage("setNavbarStatisticMethodsState", ready)
        ready
    })
    outputOptions(output, "data_ready", suspendWhenHidden = FALSE)
    
    observe({
        df <- my_data()
        req(df)
        validate(need(ncol(df) > 0, "No columns in uploaded data."))
        
        isolate({
            numeric_vars <- names(df)[sapply(df, is.numeric)]
            updateSelectInput(
                session,
                "ttest_num_var",
                choices = numeric_vars
            )
            
            group_var_candidates <- names(df)
            updateSelectInput(
                session, "ttest_group_var",
                choices = group_var_candidates
            )
        })
    })
    
    output$ttest_group_levels_ui <- renderUI({
        df <- my_data()
        req(df)
        req(input$ttest_group_var)
        
        group_raw <- df[[input$ttest_group_var]]
        group <- try(as.factor(group_raw), silent = TRUE)
        validate(
            need(
                !inherits(group, "try-error"),
                "Cannot coerce grouping variable to factor."
            ),
            need(
                nlevels(group) >= 2,
                "Grouping variable must have at least 2 levels."
            )
        )
        
        selectInput(
            "ttest_selected_levels",
            "Select two groups to compare:",
            choices = levels(group),
            selected = levels(group)[1:2],
            multiple = TRUE
        )
    })
    
    ttest_result <- reactive({
        df <- my_data()
        req(df)
        req(
            input$ttest_num_var,
            input$ttest_group_var,
            input$ttest_selected_levels
        )
        validate(
            need(
                length(input$ttest_selected_levels) == 2,
                "Select exactly two groups."
            )
        )
        
        num_var_name <- isolate(input$ttest_num_var)
        group_var_name <- isolate(input$ttest_group_var)
        
        validate(
            need(
                num_var_name %in% names(df),
                "Numeric variable not found."
            ),
            need(
                group_var_name %in% names(df),
                "Grouping variable not found."
            )
        )
        
        y <- df[[num_var_name]]
        group_raw <- df[[group_var_name]]
        
        # checks
        validate(
            need(
                is.numeric(y),
                "Selected outcome variable must be numeric."
            )#,
            # need(
                # !is.numeric(group_raw),
                # "Grouping variable should not be numeric."
            # )   # allow characters, factors, logicals
        )
        
        group <- as.factor(group_raw)
        
        # Subset only the selected two groups
        subset_idx <- group %in% input$ttest_selected_levels
        y <- y[subset_idx]
        group <- factor(
            group[subset_idx],
            levels = input$ttest_selected_levels
        )   # set correct order
        
        validate(
            need(nlevels(group) == 2,
            "After subsetting, must have exactly 2 groups.")
        )
        
        # get selected group names
        group_names <- levels(group)
        
        # local data frame for t-test
        local_df <- data.frame(y = y, group = group)
        
        test <- t.test(
            y ~ group,
            data = local_df,
            alternative = input$ttest_alt,
            mu = input$ttest_mu,
            var.equal = FALSE
        )
        
        group_levels <- levels(group)
        
        df_out <- data.frame(
            "statistic" = round(test$statistic, 3),
            "degrees of freedom" = round(test$parameter, 2),
            "n_group1" = sum(group == group_names[1], na.rm = TRUE),
            "n_group2" = sum(group == group_names[2], na.rm = TRUE),
            "p-value" = signif(test$p.value, 4),
            "confidence interval" = paste(
                "(",
                round(test$conf.int[1], 3),
                ", ",
                round(test$conf.int[2], 3),
                ")",
                sep = ""
            ),
            "mean_group1" = round(
                mean(y[group == group_names[1]], na.rm = TRUE),
                3
            ),
            "mean_group2" = round(
                mean(y[group == group_names[2]], na.rm = TRUE),
                3
            ),
            check.names = FALSE
        )
        
        names(df_out)[3:4] <- paste0("n (", group_names, ")")
        names(df_out)[7:8] <- paste0("mean (", group_names, ")")
        
        df_out
        
    })
    
    output$ttest_result <- renderTable({
        tryCatch({
            ttest_result()
        }, error = function(e) {
            NULL
        })
    })
    
    output$ttest_boxplot <- renderPlot({
        tryCatch(
            {
                df <- my_data()
                req(df)
                req(
                    input$ttest_num_var,
                    input$ttest_group_var,
                    input$ttest_selected_levels
                )
                validate(
                    need(
                        length(input$ttest_selected_levels) == 2,
                        "Select exactly two groups."
                    )
                )
                
                y <- df[[input$ttest_num_var]]
                group_raw <- df[[input$ttest_group_var]]
                
                group <- as.factor(group_raw)
                subset_idx <- group %in% input$ttest_selected_levels
                y <- y[subset_idx]
                group <- group[subset_idx]
                
                # set the factor levels explicitly according to the user selection order
                group <- factor(group, levels = input$ttest_selected_levels)
                
                # salculate sample sizes
                group_counts <- table(group)
                group_labels <- paste(
                    names(group_counts),
                    " (n = ",
                    as.numeric(group_counts),
                    ")",
                    sep = ""
                )
                
                boxplot(
                    y ~ group,
                    main = "Boxplot of Selected Groups",
                    xlab = input$ttest_group_var,
                    ylab = input$ttest_num_var,
                    names = group_labels,
                    col = "lightblue",
                    border = "darkblue"
                )
            },
            error = function(e) {
                # do nothing (catch the error silently)
                NULL
            }
        )
    })
    
    output$ttest_h0_statement <- renderUI({
        req(
            input$ttest_num_var,
            input$ttest_group_var,
            input$ttest_selected_levels
        )
        validate(
            need(
                length(input$ttest_selected_levels) == 2,
                "Select two groups."
            )
        )
        
        group1 <- input$ttest_selected_levels[1]
        group2 <- input$ttest_selected_levels[2]
        var <- input$ttest_num_var
        mu <- input$ttest_mu
        
        h0 <- paste(
            "<b>Null Hypothesis</b> (H<sub>0</sub>): The mean of '",
            var,
            "' for group '",
            group1,
            "' minus group '",
            group2,
            "' equals ",
            mu,
            ".",
            sep = ""
        )
        
        # Based on selected alternative
        h1 <- switch(input$ttest_alt,
            "two.sided" = paste(
                "<b>Alternative Hypothesis</b> (H<sub>1</sub>): The mean difference is not equal to ",
                mu,
                ".",
                sep = ""
            ),
            "less" = paste(
                "<b>Alternative Hypothesis</b> (H<sub>1</sub>): The mean of '",
                var,
                "' for group '",
                group1,
                "' is less than that for group '",
                group2,
                "'.",
                sep = ""
            ),
            "greater" = paste(
                "<b>Alternative Hypothesis</b> (H<sub>1</sub>): The mean of '",
                var,
                "' for group '",
                group1,
                "' is greater than that for group '",
                group2,
                "'.",
                sep = ""
            )
        )
        
        HTML(paste(h0, "<br>", h1, "<br><br>", sep = ""))
        
    })
    
    ## ------------------------------------------------------------------------
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################






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
required_packages <- c("shiny", "shinyjs", "DT", "readxl")

# Call the function to install and load the packages
install_and_load_packages(required_packages)


## ----------------------------------------------------------------------------

###############################################################################

## I am creating a backend of the application ---------------------------------

my_server <- function(
    
    input,
    output,
    session

){
    
    ## Register all system information variables for about tab
    register_sysinfo_outputs(output)
  
    ## I am introducing a counter ---------------------------------------------
    output$my_counter <- renderText({
        if(
            ! file.exists("my_counter.Rdata")
        ) {
            my_counter <- 0
        } else {
            load(file = "my_counter.Rdata")
        }
        
        my_counter <- my_counter + 1
        save(my_counter, file = "my_counter.Rdata" )
        paste("Count of visits: ", my_counter, sep = "")
    })

    
    ## logic of user of inbuilt data upload -----------------------------------
    my_data <- reactiveVal(NULL)

    builtin_file <- reactiveVal(NULL)
    uploaded_file <- reactiveVal(NULL)  # potřebuji, protože fileinput si i po reset drží název souboru


    # Add all supported built-in datasets to selectInput
    updateSelectInput(session, "builtin_dataset", choices = get_builtin_named_datasets())



    load_dataset <- function(session, fullpath, name, header_included, col_separator, use_first_col_as_rownames, col_types, builtin = TRUE, quiet = FALSE) {
      source_type_str <- paste(if (builtin) "built-in dataset" else "file", sQuote(name))

      if (!quiet && !is.null(builtin_file())) {
        toastr_info(sprintf("Built-in dataset %s was successfully unloaded.", sQuote(builtin_file())))
        builtin_file(NULL)
      }
      if (!quiet && !is.null(uploaded_file())) {
        toastr_info(sprintf("File %s was successfully unloaded.", sQuote(uploaded_file())))
        uploaded_file(NULL)
      }

      if (!file.exists(fullpath)) {
        msg_error <- sprintf("The %s not found!", source_type_str)
        toastr_error(msg_error)
        showNotification(msg_error, type = "error")

        session$sendCustomMessage("setNavbarStatisticMethodsState", FALSE)
        return(NULL)
      }

      tryCatch({
        file_ext <- tools::file_ext(fullpath)

        if (file_ext %in% c("xls", "xlsx")) {
          disable(selector = "input[name='col_separator']")

          sheet_names <- readxl::excel_sheets(fullpath)
          if (length(sheet_names) > 1) {
            msg_multiple_sheets <- sprintf(
              "Multiple sheets detected (%d) in the %s file. Due to application limitations, only the first sheet will be loaded.",
              length(sheet_names), file_ext
            )

            toastr_info(msg_multiple_sheets)
            #showNotification(msg_multiple_sheets, type = "message")
          }

          data <- readxl::read_excel(fullpath, col_names = header_included)
        } else {
          enable(selector = "input[name='col_separator']")
          data <- read.csv(fullpath, header = header_included, sep = col_separator)
        }

        if (nrow(data) <= 1) {
          toastr_warning(sprintf("The %s appears to be empty or contains only header data.", source_type_str))
        }

        if (use_first_col_as_rownames && ncol(data) > 1) {
          rownames(data) <- data[[1]]
          data <- data[, -1, drop = FALSE]
        }

        num_missings <- sum(is.na(data))
        if (num_missings > 0) {
          if (!quiet) toastr_warning(sprintf("The %s contains %s missing value(s).", source_type_str, num_missings))
          # enablovat volbu pro řešení missingů
        } else {
          # disablovat volbu pro řešení missingů
        }

        col_types_vec <- unlist(strsplit(col_types, ""))
        if (length(col_types_vec) == ncol(data)) {
          data <- mapply(function(col, type) {
            switch(type,
                   "N" = as.numeric(col),
                   "S" = as.character(col),
                   "D" = as.Date(col),
                   "L" = as.logical(col),
                   col)
          }, data, col_types_vec, SIMPLIFY = FALSE)
          data <- as.data.frame(data)
        }

        if (!quiet) toastr_success(sprintf("The %s loaded successfully!", source_type_str))
        session$sendCustomMessage("setNavbarStatisticMethodsState", TRUE)

        # vše OK, uložíme si název souboru pro případný upload
        if (builtin) {
            builtin_file(name)
        } else {
            uploaded_file(name)
        }

        return(data)

      }, error = function(e) {
        msg_error <- sprintf("Error reading %s %s.", source_type_str, e$message)
        toastr_error(paste(msg_error))
        showNotification(msg_error, type = "error")
        session$sendCustomMessage("setNavbarStatisticMethodsState", FALSE)

        return(NULL)
      })
    }


    # Reagujeme na nahrání souboru
    observeEvent(input$file_upload, ignoreInit = TRUE, {
      req(input$file_upload)

      if (input$use_builtin) {
        # odškrtneme případný nahraný vestavěný dataset
        updateCheckboxInput(session, "use_builtin", value = FALSE)
      }

      data <- load_dataset(
        session = session,
        fullpath = input$file_upload$datapath,
        name = input$file_upload$name,
        header_included = input$header,
        col_separator = input$col_separator,
        use_first_col_as_rownames = input$use_first_col_as_rownames,
        col_types = input$col_types,
        builtin = FALSE,
        quiet = FALSE
      )

      my_data(data)
    })


    # Reagujeme na vybrání vestavěného datasetu
    observeEvent(list(input$use_builtin, input$builtin_dataset), ignoreInit = TRUE, {
      if (input$use_builtin) {
        if (!is.null(uploaded_file())) {
          reset("file_upload")
        }

        data <- load_dataset(
          session = session,
          fullpath = file.path(SAMPLE_DATASETS_DIR, paste(input$builtin_dataset)),
          name = input$builtin_dataset,
          header_included = input$header,
          col_separator = input$col_separator,
          use_first_col_as_rownames = input$use_first_col_as_rownames,
          col_types = input$col_types,
          builtin = TRUE,
          quiet = FALSE
        )

        my_data(data)

      } else {
        # odškrtnutí vestavěného datasetu
        if (!is.null(builtin_file())) {
          toastr_info(paste("Built-in dataset", sQuote(builtin_file()), "was successfully unloaded."))
          session$sendCustomMessage("setNavbarStatisticMethodsState", FALSE)
          builtin_file(NULL)
          my_data(NULL)
        }
      }
    })


    observeEvent(list(input$header, input$use_first_col_as_rownames, input$col_separator, input$col_types), ignoreInit = TRUE, {
        req(my_data())  # pokud už máme nějaká data, budeme dělat jen reload s jinými parametry

        if (input$use_builtin) {
          name = input$builtin_dataset
          header_included = input$header
        } else {
          name = input$file_upload$name
          header_included = input$header
        }

        data <- load_dataset(
          session = session,
          fullpath = file.path(SAMPLE_DATASETS_DIR, paste(input$builtin_dataset)),
          name = name,
          header_included = header_included,
          col_separator = input$col_separator,
          use_first_col_as_rownames = input$use_first_col_as_rownames,
          col_types = input$col_types,
          builtin = FALSE,
          quiet = TRUE
        )

        my_data(data)
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




    # nikde se nevyužívá
    observeEvent(my_data(), {
      is_data_valid <- !is.null(my_data()) && nrow(my_data()) > 0 && ncol(my_data()) > 0
      #toastr_info(paste(!is.null(my_data())))
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

        select_options <- c("integer", "numeric", "logical", "string", "character", "date", "POSIXct")

        datatable(
            upload_summary(),
            escape = FALSE,  
            options = list(
                pageLength = 10,
                autoWidth = TRUE,
                dom = 'Bfrtip',
                ordering = FALSE,
                stateSave = TRUE,  # Save the table state, but for visible rows only
                columnDefs = list(list(
                    targets = 1,
                    render = JS(sprintf(
                        "function(data, type, row, meta) {
                           var options = ['%s'];
                           return renderDataTypeSelector(data, type, row, options);
                         }",
                        paste(select_options, collapse = "','")
                    ))
                ))
            ),
            rownames = FALSE,
            colnames = c("Variable", "Data type"),
            selection = "none"
        )        
    }, server = FALSE)      

    
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
        !is.null(my_data()) && nrow(my_data()) > 0 && ncol(my_data()) > 0
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


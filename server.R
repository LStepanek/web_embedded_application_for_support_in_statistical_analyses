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
required_packages <- c(
    "shiny",
    "shinyjs",
    "DT",
    "readxl",
    "ggplot2"
)

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

        session$sendCustomMessage("setNavbarItemsState", FALSE)
        return(NULL)
      }

      # enable all data settings (checkboxes and radiobuttons)
      enable(selector = "div#data_options")
      enable(selector = "input[name='col_separator']")

      tryCatch({
        file_ext <- tools::file_ext(fullpath)

        if (file_ext %in% c("xls", "xlsx")) {
          # XLS/XLSX data
          disable(selector = "input[name='col_separator']")

          sheet_names <- readxl::excel_sheets(fullpath)
          if (!quiet && length(sheet_names) > 1) {
            msg_multiple_sheets <- sprintf(
              "Multiple sheets detected (%d) in the %s file. Due to application limitations, only the first sheet will be loaded.",
              length(sheet_names), file_ext
            )

            toastr_info(msg_multiple_sheets)
            #showNotification(msg_multiple_sheets, type = "message")
          }

          data <- readxl::read_excel(fullpath, col_names = header_included)

        } else if (file_ext %in% c("json")) {
          # JSON data (newline-delimited)
          #disable(selector = "div#data_options")
          disable(selector = "#data_options input#header")
          disable(selector = "input[name='col_separator']")

          json_lines <- readLines(fullpath, warn = FALSE)
          data_list <- lapply(json_lines, function(line) jsonlite::fromJSON(line))
          data <- as.data.frame(do.call(rbind, data_list), stringsAsFactors = FALSE)

        } else {
          # CSV/TXT/TSV
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
        session$sendCustomMessage("setNavbarItemsState", TRUE)

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
        session$sendCustomMessage("setNavbarItemsState", FALSE)

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
          session$sendCustomMessage("setNavbarItemsState", FALSE)
          builtin_file(NULL)
          my_data(NULL)
        }
      }
    })


    # Reagujeme na změnu voleb checkboxů a radiobuttonů
    observeEvent(list(input$header, input$use_first_col_as_rownames, input$col_separator, input$col_types), ignoreInit = TRUE, {
        req(my_data())  # pokud už máme nějaká data, budeme dělat jen tichý reload s jinými parametry

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
                        paste(DATA_TYPE_OPTIONS, collapse = "','")
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
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    
    ## logic of the two-sample t-test -----------------------------------------
    
    output$data_ready <- reactive({
        !is.null(my_data()) && nrow(my_data()) > 0 && ncol(my_data()) > 0
    })
    outputOptions(output, "data_ready", suspendWhenHidden = FALSE)
    
    observe({
        df <- my_data()
        req(df)
        shiny::validate(need(ncol(df) > 0, "No columns in uploaded data."))
        
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
        shiny::validate(
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
        shiny::validate(
            need(
                length(input$ttest_selected_levels) == 2,
                "Select exactly two groups."
            )
        )
        
        num_var_name <- isolate(input$ttest_num_var)
        group_var_name <- isolate(input$ttest_group_var)
        
        shiny::validate(
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
        shiny::validate(
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
        
        shiny::validate(
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
                shiny::validate(
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
        shiny::validate(
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
    
    ## --- helpers ---------------------------------------------------------------
    
    mean_ci <- function(x, conf.level = 0.95) {
      x <- x[is.finite(x)]
      n <- length(x)
      m <- mean(x)
      s <- stats::sd(x)
      se <- s / sqrt(n)
      alpha <- 1 - conf.level
      tcrit <- stats::qt(1 - alpha/2, df = n - 1)
      c(mean = m, lwr = m - tcrit * se, upr = m + tcrit * se, n = n)
    }
    
    cohens_d_pooled <- function(x, g, order = NULL) {
      # x numeric, g factor with 2 levels
      stopifnot(length(levels(g)) == 2)
      if (!is.null(order)) g <- factor(g, levels = order)
      lvs <- levels(g)
      x1 <- x[g == lvs[1]]
      x2 <- x[g == lvs[2]]
      n1 <- sum(is.finite(x1)); n2 <- sum(is.finite(x2))
      m1 <- mean(x1, na.rm = TRUE); m2 <- mean(x2, na.rm = TRUE)
      s1 <- stats::sd(x1, na.rm = TRUE); s2 <- stats::sd(x2, na.rm = TRUE)
      s_pooled <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
      d <- (m1 - m2) / s_pooled
      list(d = d, n1 = n1, n2 = n2, s_pooled = s_pooled, levels = lvs, m1 = m1, m2 = m2)
    }
    
    describe_d <- function(d) {
      ad <- abs(d)
      if (is.na(ad)) return("unknown magnitude")
      if (ad < 0.2) "negligible"
      else if (ad < 0.5) "small"
      else if (ad < 0.8) "medium"
      else "large"
    }
    
    ## --- common reactive to reuse cleaned data ---------------------------------
    
    ttest_local <- reactive({
      df <- my_data()
      req(df, input$ttest_num_var, input$ttest_group_var, input$ttest_selected_levels)
      shiny::validate(need(length(input$ttest_selected_levels) == 2, "Select exactly two groups."))
      y <- df[[input$ttest_num_var]]
      g_raw <- df[[input$ttest_group_var]]
      shiny::validate(need(is.numeric(y), "Selected outcome variable must be numeric."))
      g <- as.factor(g_raw)
      idx <- g %in% input$ttest_selected_levels
      y <- y[idx]
      g <- factor(g[idx], levels = input$ttest_selected_levels) # respect user order
      shiny::validate(need(nlevels(g) == 2, "After subsetting, must have exactly 2 groups."))
      data.frame(y = y, group = g)
    })
    
    # Labels that show only when data exists
    output$ttest_label_means_ci <- renderUI({
      df_loc <- try(ttest_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 0)
      tags$h4("Confidence intervals for group means")
    })

    output$ttest_label_shapiro <- renderUI({
      df_loc <- try(ttest_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 2)  # need at least 3 per group for Shapiro
      tags$h4("Normality check (Shapiro–Wilk test)")
    })

    output$ttest_label_effectsize <- renderUI({
      df_loc <- try(ttest_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 1)
      tags$h4("Effect size (Cohen's d)")
    })
    
    output$ttest_label_interpretation <- renderUI({
      df_loc <- try(ttest_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 1)
      tags$h4("Interpretation")
    })
    
    
    ## --- confidence intervals for group means ----------------------------------
    
    output$ttest_means_ci <- renderTable({
      tryCatch({
        df_loc <- ttest_local()
        lvs <- levels(df_loc$group)
        
        ci1 <- mean_ci(df_loc$y[df_loc$group == lvs[1]])
        ci2 <- mean_ci(df_loc$y[df_loc$group == lvs[2]])
        
        out <- data.frame(
          Group = lvs,
          n = c(ci1["n"], ci2["n"]),
          Mean = round(c(ci1["mean"], ci2["mean"]), 3),
          `95% CI` = c(
            sprintf("(%0.3f, %0.3f)", ci1["lwr"], ci1["upr"]),
            sprintf("(%0.3f, %0.3f)", ci2["lwr"], ci2["upr"])
          ),
          check.names = FALSE
        )
        out
      }, error = function(e) NULL)
    })
    
    ## --- Shapiro–Wilk normality per group --------------------------------------
    
    output$ttest_shapiro <- renderTable({
      tryCatch({
        df_loc <- ttest_local()
        lvs <- levels(df_loc$group)
        res <- lapply(lvs, function(L) {
          x <- df_loc$y[df_loc$group == L]
          if (sum(is.finite(x)) < 3) {
            c(W = NA_real_, `p-value` = NA_real_, Note = "Needs at least 3 observations")
          } else if (sum(is.finite(x)) > 5000) {
            # Shapiro is defined for n <= 5000
            c(W = NA_real_, `p-value` = NA_real_, Note = "n > 5000; test not computed")
          } else {
            s <- stats::shapiro.test(x)
            c(W = unname(round(s$statistic, 3)), `p-value` = signif(s$p.value, 4), Note = "")
          }
        })
        df <- do.call(rbind, res)
        data.frame(Group = lvs, df, check.names = FALSE, row.names = NULL)
      }, error = function(e) NULL)
    })
    
    ## --- Cohen's d -------------------------------------------------------------
    
    output$ttest_effectsize <- renderTable({
      tryCatch({
        df_loc <- ttest_local()
        cd <- cohens_d_pooled(df_loc$y, df_loc$group, order = levels(df_loc$group))
        data.frame(
          `Cohen's d (pooled SD)` = round(cd$d, 3),
          `Magnitude` = describe_d(cd$d),
          `Group order` = paste(cd$levels[1], "vs", cd$levels[2]),
          check.names = FALSE
        )
      }, error = function(e) NULL)
    })
    
    ## --- One-sentence interpretation -------------------------------------------
    
    output$ttest_interpretation <- renderUI({
      tryCatch({
        df_loc <- ttest_local()
        test <- stats::t.test(
          y ~ group, data = df_loc,
          alternative = input$ttest_alt, mu = input$ttest_mu, var.equal = FALSE
        )
        cd <- cohens_d_pooled(df_loc$y, df_loc$group, order = levels(df_loc$group))
        lvs <- levels(df_loc$group)
        
        direction <- if (is.na(cd$d)) "no clear difference" else if (cd$d > 0) paste0(lvs[1], " > ", lvs[2]) else paste0(lvs[1], " < ", lvs[2])
        mag <- describe_d(cd$d)
        sig <- if (is.na(test$p.value)) "with unclear significance" else if (test$p.value < 0.001) "and this difference is statistically significant (p < 0.001)"
        else if (test$p.value < 0.05) "and this difference is statistically significant (p < 0.05)"
        else "but this difference is not statistically significant (p ≥ 0.05)"
        
        sentence <- sprintf(
          "On average, %s shows a %s effect (Cohen's d = %0.2f) in the direction %s, %s.",
          input$ttest_num_var, mag, cd$d, direction, sig
        )
        HTML(paste("<b>Interpretation:</b> ", sentence))
      }, error = function(e) NULL)
    })
    
    
    ## --- Violin plot ------------------------------------------------------------
    
    output$ttest_violin <- renderPlot({
      tryCatch({
        df_loc <- ttest_local()
        # lazy load ggplot2 if available
        if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
        library(ggplot2)
        ggplot(df_loc, aes(x = group, y = y)) +
          geom_violin(trim = FALSE, alpha = 0.6) +
          geom_boxplot(width = 0.15, outlier.shape = NA) +
          geom_jitter(width = 0.08, height = 0, alpha = 0.5, size = 1.6) +
          labs(
            title = "Violin + boxplot with raw points",
            x = input$ttest_group_var,
            y = input$ttest_num_var
          )
      }, error = function(e) NULL)
    })
    
    
    ## --- Density plot -----------------------------------------------------------
    
    output$ttest_density <- renderPlot({
      tryCatch({
        df_loc <- ttest_local()
        if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
        library(ggplot2)
        ggplot(df_loc, aes(x = y, fill = group)) +
          geom_density(alpha = 0.4, adjust = 1) +
          labs(
            title = "Kernel density by group",
            x = input$ttest_num_var,
            y = "Density"
          )
      }, error = function(e) NULL)
    })
    
    
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    
    ## ===== Paired t-test (server) ===========================================
    
    ## populate selectors (assuming output$data_ready already exists)
    observe({
      df <- my_data(); req(df)
      isolate({
        numeric_vars <- names(df)[sapply(df, is.numeric)]
        updateSelectInput(session, "ptt_var1", choices = numeric_vars)
        updateSelectInput(session, "ptt_var2", choices = numeric_vars)
      })
    })
    
    ## --- helpers ----------------------------------------------------------------
    mean_ci <- function(x, conf.level = 0.95) {
      x <- x[is.finite(x)]
      n <- length(x); m <- mean(x); s <- stats::sd(x)
      se <- s / sqrt(n); alpha <- 1 - conf.level
      tcrit <- stats::qt(1 - alpha/2, df = n - 1)
      c(mean = m, lwr = m - tcrit * se, upr = m + tcrit * se, n = n)
    }
    
    describe_d <- function(d) {
      ad <- abs(d); if (is.na(ad)) return("unknown magnitude")
      if (ad < 0.2) "negligible" else if (ad < 0.5) "small" else if (ad < 0.8) "medium" else "large"
    }
    
    cohens_d_paired <- function(x, y) {  # dz = mean(diff)/sd(diff)
      d <- x - y; d_mean <- mean(d, na.rm = TRUE); d_sd <- stats::sd(d, na.rm = TRUE)
      d_z <- d_mean / d_sd
      list(d = d_z, diff_mean = d_mean, diff_sd = d_sd)
    }
    
    ## --- paired data reactive (pairwise complete) --------------------------------
    ptt_local <- reactive({
      df <- my_data()
      req(df, input$ptt_var1, input$ptt_var2)
      shiny::validate(
        need(input$ptt_var1 %in% names(df), "First variable not found."),
        need(input$ptt_var2 %in% names(df), "Second variable not found."),
        need(input$ptt_var1 != input$ptt_var2, "Pick two different variables.")
      )
      x <- df[[input$ptt_var1]]
      y <- df[[input$ptt_var2]]
      shiny::validate(need(is.numeric(x) && is.numeric(y), "Both variables must be numeric."))
      idx <- is.finite(x) & is.finite(y)
      shiny::validate(need(sum(idx) >= 2, "Need at least 2 complete pairs."))
      data.frame(x = x[idx], y = y[idx], id = seq_len(sum(idx)))
    })
    
    ## --- H0/H1 statement ---------------------------------------------------------
    output$ptt_h0_statement <- renderUI({
      req(input$ptt_var1, input$ptt_var2, input$ptt_mu, input$ptt_alt)
      v1 <- input$ptt_var1; v2 <- input$ptt_var2; mu <- input$ptt_mu
      h0 <- paste0("<b>Null Hypothesis</b> (H<sub>0</sub>): mean(", v1, " − ", v2, ") = ", mu, ".")
      h1 <- switch(input$ptt_alt,
                   "two.sided" = paste0("<b>Alternative Hypothesis</b> (H<sub>1</sub>): mean(", v1, " − ", v2, ") ≠ ", mu, "."),
                   "less"      = paste0("<b>Alternative Hypothesis</b> (H<sub>1</sub>): mean(", v1, " − ", v2, ") < ",  mu, "."),
                   "greater"   = paste0("<b>Alternative Hypothesis</b> (H<sub>1</sub>): mean(", v1, " − ", v2, ") > ",  mu, ".")
      )
      HTML(paste(h0, "<br>", h1, "<br><br>"))
    })
    
    ## --- main test result --------------------------------------------------------
    ptt_result <- reactive({
      df_loc <- ptt_local()
      test <- stats::t.test(
        df_loc$x, df_loc$y,
        alternative = input$ptt_alt,
        mu = input$ptt_mu,
        paired = TRUE
      )
      
      # dynamic mean columns
      means_tbl <- setNames(
        data.frame(
          round(mean(df_loc$x), 3),
          round(mean(df_loc$y), 3),
          check.names = FALSE
        ),
        c(sprintf("mean (%s)", input$ptt_var1),
          sprintf("mean (%s)", input$ptt_var2))
      )
      
      out <- cbind(
        data.frame(
          statistic = round(unname(test$statistic), 3),
          `degrees of freedom` = round(unname(test$parameter), 2),
          `n pairs` = nrow(df_loc),
          `p-value` = signif(test$p.value, 4),
          `CI (diff)` = sprintf("(%0.3f, %0.3f)", test$conf.int[1], test$conf.int[2]),
          check.names = FALSE
        ),
        means_tbl,
        check.names = FALSE
      )
      
      out
    })
    
    output$ptt_result <- renderTable({
      tryCatch(ptt_result(), error = function(e) NULL)
    })
    
    
    # ---- Labels ----
    output$ptt_label_boxplot <- renderUI({
      df_loc <- try(ptt_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 1)
      tags$h4("Boxplot of the two paired variables")
    })
    
    output$ptt_label_boxplot_diff <- renderUI({
      df_loc <- try(ptt_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 1)
      tags$h4("Boxplot of paired differences")
    })
    
    # ---- Boxplot: variables side-by-side ----
    output$ptt_boxplot <- renderPlot({
      tryCatch({
        df_loc <- ptt_local()
        if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
        library(ggplot2)
        
        long <- rbind(
          data.frame(variable = input$ptt_var1, value = df_loc$x),
          data.frame(variable = input$ptt_var2, value = df_loc$y)
        )
        
        ggplot(long, aes(x = variable, y = value)) +
          geom_boxplot(outlier.alpha = 0.35) +
          geom_jitter(width = 0.08, height = 0, alpha = 0.5, size = 1.6) +
          labs(title = "Boxplots of paired measurements",
               x = NULL, y = "Value")
      }, error = function(e) NULL)
    })
    
    # ---- Boxplot: paired differences (x - y) ----
    output$ptt_boxplot_diff <- renderPlot({
      tryCatch({
        df_loc <- ptt_local()
        if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
        library(ggplot2)
        
        dd <- data.frame(diff = df_loc$x - df_loc$y)
        ggplot(dd, aes(x = "", y = diff)) +
          geom_boxplot(width = 0.25, outlier.alpha = 0.35) +
          geom_hline(yintercept = 0, linetype = "dashed") +
          labs(title = sprintf("Paired differences: %s − %s", input$ptt_var1, input$ptt_var2),
               x = NULL, y = "Difference") +
          coord_flip()  # optional: horizontal boxplot; remove if you prefer vertical
      }, error = function(e) NULL)
    })
    
    
    ## --- labels + tables: CIs per variable --------------------------------------
    output$ptt_label_means_ci <- renderUI({
      df_loc <- try(ptt_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 0)
      tags$h4("Confidence intervals for each mean")
    })
    
    output$ptt_means_ci <- renderTable({
      tryCatch({
        df_loc <- ptt_local()
        ci1 <- mean_ci(df_loc$x); ci2 <- mean_ci(df_loc$y)
        data.frame(
          Variable = c(input$ptt_var1, input$ptt_var2),
          n = c(ci1["n"], ci2["n"]),
          Mean = round(c(ci1["mean"], ci2["mean"]), 3),
          `95% CI` = c(
            sprintf("(%0.3f, %0.3f)", ci1["lwr"], ci1["upr"]),
            sprintf("(%0.3f, %0.3f)", ci2["lwr"], ci2["upr"])
          ),
          check.names = FALSE
        )
      }, error = function(e) NULL)
    })
    
    ## --- labels + tables: Shapiro–Wilk on differences ---------------------------
    output$ptt_label_shapiro <- renderUI({
      df_loc <- try(ptt_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) >= 3)  # Shapiro needs >= 3
      tags$h4("Normality check on paired differences (Shapiro–Wilk)")
    })
    
    output$ptt_shapiro <- renderTable({
      tryCatch({
        df_loc <- ptt_local()
        d <- df_loc$x - df_loc$y
        n <- sum(is.finite(d))
        if (n < 3) {
          data.frame(`W` = NA_real_, `p-value` = NA_real_, Note = "Needs at least 3 pairs")
        } else if (n > 5000) {
          data.frame(`W` = NA_real_, `p-value` = NA_real_, Note = "n > 5000; test not computed")
        } else {
          s <- stats::shapiro.test(d)
          data.frame(`W` = round(unname(s$statistic), 3),
                     `p-value` = signif(s$p.value, 4),
                     Note = "", check.names = FALSE)
        }
      }, error = function(e) NULL)
    })
    
    ## --- labels + tables: Effect size (paired dz) --------------------------------
    output$ptt_label_effectsize <- renderUI({
      df_loc <- try(ptt_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 1)
      tags$h4("Effect size (Cohen’s d for paired data, dz)")
    })
    
    output$ptt_effectsize <- renderTable({
      tryCatch({
        df_loc <- ptt_local()
        cd <- cohens_d_paired(df_loc$x, df_loc$y)
        data.frame(
          `Cohen's d (paired, dz)` = round(cd$d, 3),
          `Magnitude` = describe_d(cd$d),
          check.names = FALSE
        )
      }, error = function(e) NULL)
    })
    
    ## --- label + interpretation line --------------------------------------------
    output$ptt_label_interpretation <- renderUI({
      df_loc <- try(ptt_local(), silent = TRUE)
      req(!inherits(df_loc, "try-error"), nrow(df_loc) > 1)
      tags$h4("Interpretation")
    })
    
    output$ptt_interpretation <- renderUI({
      tryCatch({
        df_loc <- ptt_local()
        test <- stats::t.test(df_loc$x, df_loc$y,
                              alternative = input$ptt_alt,
                              mu = input$ptt_mu,
                              paired = TRUE)
        d_vec <- df_loc$x - df_loc$y
        mdiff <- mean(d_vec)
        cd <- cohens_d_paired(df_loc$x, df_loc$y)
        dir <- if (is.na(mdiff)) "no clear difference"
        else if (mdiff > 0) paste0(input$ptt_var1, " > ", input$ptt_var2)
        else if (mdiff < 0) paste0(input$ptt_var1, " < ", input$ptt_var2)
        else "no difference"
        mag <- describe_d(cd$d)
        sig <- if (is.na(test$p.value)) "with unclear significance"
        else if (test$p.value < 0.001) "and this paired difference is statistically significant (p < 0.001)"
        else if (test$p.value < 0.05)  "and this paired difference is statistically significant (p < 0.05)"
        else "but this paired difference is not statistically significant (p ≥ 0.05)"
        sentence <- sprintf(
          "On average, the paired difference %s − %s is %0.3f, a %s effect (Cohen's dz = %0.2f), %s.",
          input$ptt_var1, input$ptt_var2, mdiff, mag, cd$d, sig
        )
        HTML(sentence)
      }, error = function(e) NULL)
    })
    
    ## --- optional plots (unchanged) ---------------------------------------------
    output$ptt_violin <- renderPlot({
      tryCatch({
        df_loc <- ptt_local()
        if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
        library(ggplot2)
        long <- rbind(
          data.frame(id = df_loc$id, variable = input$ptt_var1, value = df_loc$x),
          data.frame(id = df_loc$id, variable = input$ptt_var2, value = df_loc$y)
        )
        ggplot(long, aes(x = variable, y = value)) +
          geom_violin(trim = FALSE, alpha = 0.6) +
          geom_boxplot(width = 0.15, outlier.shape = NA) +
          geom_line(aes(group = id), position = position_dodge(width = 0.2), alpha = 0.25) +
          geom_jitter(width = 0.08, height = 0, alpha = 0.5, size = 1.6) +
          labs(title = "Paired data: violin + box + paired lines",
               x = NULL, y = "Value")
      }, error = function(e) NULL)
    })
    
    output$ptt_density <- renderPlot({
      tryCatch({
        df_loc <- ptt_local()
        if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
        library(ggplot2)
        long <- rbind(
          data.frame(variable = input$ptt_var1, value = df_loc$x),
          data.frame(variable = input$ptt_var2, value = df_loc$y)
        )
        ggplot(long, aes(x = value, fill = variable)) +
          geom_density(alpha = 0.4, adjust = 1) +
          labs(title = "Kernel density by variable", x = "Value", y = "Density")
      }, error = function(e) NULL)
    })
    
    
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    ## ------------------------------------------------------------------------
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################


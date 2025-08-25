###############################################################################
###############################################################################
###############################################################################

## ===== Server module: Data upload ===========================================

dataUploadServer <- function(input, output, session, my_data) {

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

        json_file <- file(fullpath, "r")
        data <- jsonlite::stream_in(json_file, verbose = FALSE)
        close(json_file)

        # Additional type conversion: attempt to automatically cast character columns
        # to more appropriate types (numeric, integer, logical, etc.)
        data[] <- lapply(data, function(col) type.convert(col, as.is = TRUE))

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


      # Convert columns in a data frame according to specified type codes. Each character in 
      # 'col_types'  corresponds to a column in 'data' and determines the conversion applied
      if (length(strsplit(col_types, "")[[1]]) == ncol(data)) {
        data <- mapply(function(col, type) {
          switch(type,
                 "I" = as.integer(col),
                 "N" = as.numeric(col),
                 "L" = as.logical(col),
                 "C" = as.character(col),
                 "D" = as.Date(col),
                 "P" = as.POSIXct(col),
                 col)
        }, data, strsplit(col_types, "")[[1]], SIMPLIFY = FALSE)
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

      # buď první načtení dat, nebo změna, chci odstranit odpověď od AI (pokud už byla)
      output$ai_response <- renderUI({
        NULL
      })

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


  # nikde se nevyužívá, volá se jen s prvním načtením dat (ne při změně datasetu)
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

}

## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

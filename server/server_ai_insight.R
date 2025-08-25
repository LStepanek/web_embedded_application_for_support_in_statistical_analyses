###############################################################################
###############################################################################
###############################################################################

## ===== Server module: AI insight ============================================

ai_insightServer <- function(input, output, session, my_data) {

  observeEvent(input$my_navbar_page_set, ignoreInit = TRUE, {
    req(my_data())

    # pokud nejsme na AI Insight tab, ukončíme
    if (input$my_navbar_page_set != "tab_ai_insight") return()

    data_df <- my_data()
    sample_rows_to_ai <- 25

    prompt <- paste0(
      "Analyze this dataset and provide your response in structured sections. ",
      "Use Markdown formatting with headings (## Section Title) for each major topic, and bullet points where appropriate. ",
      "When referring to column names, always format them as inline code using backticks in Markdown (e.g., `column_name`).",
      "Include sections such as: Overview, Key Statistics, Missing Values, Suggested Actions, Exploration Ideas.\n\n",
      "Dataset summary (only first ", sample_rows_to_ai, " rows):\n",
      "Columns: ", paste(colnames(data_df), collapse = ","), "\n",
      "Sample rows:\n", paste(capture.output(head(data_df, sample_rows_to_ai)), collapse = "\n"), "\n"
    )

    # vypíše celý prompt do konzole (kontrola/debug)
    cat(prompt)

    # volání OpenAI API synchronně
    response <- tryCatch({
      create_chat_completion(
        model = "gpt-4",
        messages = list(
          list(role = "system", content = "You are a helpful assistant for data analysis."),
          list(role = "user", content = prompt)
        )
      )
    }, error = function(e) {
      list(error = if (!is.null(e$message) && nchar(e$message) > 0) e$message
                 else "Something went wrong, no details...")
    })

    md_text <- NULL
    error_msg <- paste0(
      "<p class='text-danger'><strong>We couldn't process your request right now. ",
      "Please try again later or contact the application developer.</strong></p>\n\n"
    )

    if (!is.null(response$error)) {
      # chyba při volání API
      md_text <- paste0(
        error_msg,
        "**Technical details (for debugging):**\n\n",
        "```text\n", response$error, "\n```"
      )
    } else if (is.list(response) && "choices" %in% names(response) &&
               is.data.frame(response$choices) &&
               "message.content" %in% colnames(response$choices)) {
      # úspěšná odpověď
      md_text <- response$choices$`message.content`[1]
    } else {
      # neočekávaný formát
      md_text <- paste0(
        error_msg,
        "**Technical details (for debugging):**\n\n",
        "```text\nUnexpected response format\n```"
      )
    }

    # markdown konvertuji a vrátím HTML
    output$ai_response <- renderUI({
      HTML(commonmark::markdown_html(md_text))
    })
  })

}

## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

###############################################################################
###############################################################################
###############################################################################

## introduction tab -----------------------------------------------------------

tab_introduction <- tabPanel(
    
    title = "Introduction",
    
    value = "tab_introduction",
    
    # Purpose section
    tags$h1("A purpose of this application"),

    tags$p("Hello and welcome to the application for support in statistical analyses! This tool helps you explore datasets, perform statistical tests, and get AI-powered insights."),

    tags$p("This application was developed as a study project to assist users in exploring, analyzing, and understanding datasets with minimal manual effort. It combines traditional statistical analyses with AI-powered insights, helping users identify patterns, trends, and anomalies quickly. The goal is to support data-driven decision-making, educational purposes, and self-service data analysis for students, researchers, and analysts."),
    
    # Key features section
    tags$h2("Key features"),
    
tags$ul(
  tags$li(tags$b("Supported dataset file formats:"),
          tags$ul(
            tags$li("Text files (CSV, TXT, TSV, etc.)"),
            tags$li(HTML("XLSX/XLS (older Excel 97-2003 format)<em><small>*</small></em>")),
            tags$li("Newline Delimited JSON (NDJSON)")
          )
  ),
  tags$li(tags$b("Data exploration:"),
          tags$div("Summary statistics, missing value detection, visualization of numerical and categorical variables, and column type adjustments.",
                   class = "no-bullet-indent")
  ),
  tags$li(tags$b("Implemented statistical methods:"),
          tags$ul(
            tags$li("Summary plots"),
            tags$li("Two-sample t-test"),
            tags$li("Paired t-test"),
            tags$li("Mann–Whitney test"),
            tags$li("Paired Wilcoxon test"),
            tags$li("Analysis of variance (ANOVA)")
          )
  ),
  tags$li(tags$b("AI-powered insights (Experimental):"),
          tags$div("Automatic assessment of a dataset sample with structured recommendations, suggested actions, and exploration ideas.",
                   class = "no-bullet-indent")
  ),
  tags$li(tags$b("Interactive UI:"),
          tags$div("Sortable/filterable tables, dynamic visualizations, and download options for results.",
                   class = "no-bullet-indent")
  ),
  tags$li(tags$b("User feedback:"),
          tags$div("Clear status indicators, loading spinners, and error messages to ensure a smooth experience.",
                   class = "no-bullet-indent")
  ),
  tags$li(tags$b("Print-friendly design:"),
          tags$div("All pages are adapted for clean and readable printing.",
                   class = "no-bullet-indent")
  )
),


    
    # Footnote or limitation note
    tags$p(tags$em(tags$small("* Currently, only the first sheet of Excel files is supported due to application limitations.")))
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

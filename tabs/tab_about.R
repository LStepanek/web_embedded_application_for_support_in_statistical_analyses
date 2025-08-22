###############################################################################
###############################################################################
###############################################################################

## about tab ------------------------------------------------------------------

tab_about <- tabPanel(
    
    title = "About application",
    
    value = "tab_about",
    
    h1("About Web-embedded application for support in statistical analyses"),
    
    p("This Shiny application is designed to support data analysts, researchers, and students by providing an interactive web-based environment for statistical analysis. It enables users to upload datasets, explore and visualize data, perform statistical tests, and apply basic machine learning techniques – all through an intuitive interface without requiring advanced programming skills. The application is particularly useful for academic projects, data exploration, and preliminary statistical analyses, and aims to make data analysis more accessible, efficient, and user-friendly."),
    
    p(HTML('This project was developed as part of the KIP XRPR1/2 Team Project course. A developer version of the application is available at <a href="http://92.62.235.37:81/">http://92.62.235.37:81/</a>.')),
    
    h2("Technical Information"),
    tags$ul(
      tags$li(strong("R version:"), textOutput("r_version", inline = TRUE)),
      tags$li(strong("Shiny version:"), textOutput("shiny_ver", inline = TRUE)),
      tags$li(strong("Operating System:"), textOutput("os", inline = TRUE)),
      tags$li(strong("CPU:"), textOutput("cpu", inline = TRUE)),
      tags$li(strong("Cores:"), textOutput("cores", inline = TRUE)),
      tags$li(strong("Total RAM:"), textOutput("ram", inline = TRUE))
    ),
    
    h2("Key Packages Used"),
    tags$ul(
      tags$li(tags$em("shiny"), " – main framework for interactive web apps"),
      tags$li(tags$em("shinythemes"), " – provides ready-to-use Bootstrap themes for Shiny apps"),
      tags$li(tags$em("shinyjs"), " – JavaScript enhancements for Shiny"),
      tags$li(tags$em("shinytoastr"), " – non-blocking toast notifications in Shiny apps"),
      tags$li(tags$em("DT"), " – interactive tables"),
      tags$li(tags$em("readxl"), " – reading Excel files"),
      tags$li(tags$em("jsonlite"), " – reading and writing JSON files"),
      tags$li(tags$em("openai"), " – interface to OpenAI API for AI features"),
      tags$li(tags$em("commonmark"), " – Markdown parsing and rendering")
    ),
    
    h2("Authors and Contributors"),
    p("Lead Developer: Lubomir Stepanek"),
    tags$ul(
      tags$li("since November 23, 2024: Lubomir Stepanek and Michal Seda"),
      tags$li("since February 22, 2025: Lubomir Stepanek, Michal Seda, and Milan Cizek")
    ),
    
    h2("Project repository"),
    p("The repository contains the source code, documentation, example datasets and other artefacts."),
    
    p("GitHub: ",
      a("https://github.com/LStepanek/web_embedded_application_for_support_in_statistical_analyses", 
        href = "https://github.com/LStepanek/web_embedded_application_for_support_in_statistical_analyses")),
    p("Wiki: ",
      a("https://github.com/LStepanek/web_embedded_application_for_support_in_statistical_analyses/wiki",
        href = "https://github.com/LStepanek/web_embedded_application_for_support_in_statistical_analyses/wiki")),
    
    h2("Acknowledgements"),
    p(HTML("We gratefully acknowledge the authors and contributors of the open-source R packages used in this project, including <em>shiny</em>, <em>shinythemes</em>, <em>shinyjs</em>, <em>shinytoastr</em>, <em>DT</em>, and <em>readxl</em>.")),
    
    p("Special thanks to Jaroslav Zacek (University of Ostrava) for his guidance and mentorship throughout the project."),
    
    h2("License and Acknowledgements"),
    p("This application is distributed under the Creative Commons Attribution-NonCommercial-NoDerivatives 3.0 Czech Republic (CC BY-NC-ND 3.0 CZ) license."),
    p("We gratefully acknowledge the R and Shiny communities and all authors of the open-source packages used in this project.")
)

## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

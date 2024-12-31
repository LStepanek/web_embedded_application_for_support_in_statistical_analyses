###############################################################################
###############################################################################
###############################################################################

## I am loading packages ------------------------------------------------------

for(
    my_package in c(
        
        "shiny",
        "shinyjs"
        
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


## ----------------------------------------------------------------------------

###############################################################################

## I am creating a user interface / frontend of the application ---------------

my_ui <- shinyUI(
    
    fluidPage(
        
        tagList(
        
            ## ----------------------------------------------------------------
            
            ###################################################################
            
            ## CSS settings ---------------------------------------------------
            
            tags$head(
                
                tags$style(
                    type = "text/css",
                    paste(
                        ".panel-footer {position:",
                        "fixed; right: 0;",
                        "bottom: 0;",
                        "left: 0;}",
                        sep = ""
                    )
                )
                
            ),
            
            
            ## busy indicator will be here ------------------------------------
            
            
            
            ## I am calling shinyjs functionalities ---------------------------
            
            shinyjs::useShinyjs(),
            
            
            ## user interface of the application ------------------------------
            
            navbarPage(
                
                ## header -----------------------------------------------------
            
                title = div(
                    HTML(
                        "<font size = '5'>Statistical tools</font>"
                    )
                ),
                
                id = "my_navbar_page_set",
                
                windowTitle = "Statistical tools",
                
                position = "fixed-top",
                
                selected = "introduction",
                
                collapsible = TRUE,
                
                
                ## footer -----------------------------------------------------
                
                footer = list(
                   
                    div(class = "clear"),
                    div(
                        class = "panel-footer",
                        HTML(
                            '
                                <!-- application name -->
                                <font size = "3">
                                    Statistical tools
                                </font>
                                
                                <!-- application version -->
                                <font size = "2">
                                    &nbsp; | &nbsp;
                                    version 0.0.1
                                    &nbsp; | &nbsp;
                                </font>
                                
                                <!-- licence and its logo -->
                                <a
                                    href =
                                    "https://creativecommons.org/
                                    licenses/by-nc-nd/3.0/cz/"
                                    id = "tooltip_cc"
                                    target = "_blank"
                                >
                                    <img
                                        src = "cc_by_nc_nd.png",
                                        style = "height: 20px;"
                                    >
                                </a>
                                
                                <!-- licence abbreviation -->
                                &nbsp;
                                | &nbsp; CC BY-NC-ND 3.0 CZ &nbsp;
                                | &nbsp; 2024 &nbsp;
                                | &nbsp;
                                
                                <!-- technical suppport -->
                                A project within 
                                <a
                                    href = "https://portal.osu.cz/stag?urlid=prohlizeni-predmet-sylabus&predmetZkrPrac=KIP&predmetZkrPred=XRPR1&predmetRok=2024"
                                    target = "_blank"
                                >
                                    XRPR1 Team project 1
                                </a>
                            ',
                            '
                            <!-- visit counter -->
							<span style = "float:right">
                            ',
                            paste(
                                "<font size = '2'><b>",
                                textOutput(
                                    "my_counter",
                                    inline = TRUE
                                ),
                                "&emsp; </b></font>",
                                sep = ""
                            ),
                            '
                            <!-- logos of faculties and universities -->
                            <a
                                href = "https://prf.osu.eu/"
                                target = "_blank"
                            >
                                <img
                                    src = "logo_univesity_of_ostrava.png",
                                    style = "height: 30px;"
                                >
                            </a>
                            <a
                                href = "https://fis.vse.cz/english/"
                                target = "_blank"
                            >
                                <img
                                    src = "logo_faculty_of_informatics_and_statistics.png",
                                    style = "height: 20px;"
                                >
                            </a>
                            </span>
                            ',
                            '
                            <!-- javascript functionalities in HTML -->
                            '
                        ),
                        style = "opacity: 1.00; z-index: 1000;"
                    )
                    
                ),
                
                
                # CSS bootstrap will be here
                theme = "bootstrap.css",
                
                
                ## the first tab ----------------------------------------------
                
                tabPanel(
                    
                    title = "Introduction",
                    
                    value = "introduction",
                    
                    HTML("<br>"),
                    HTML("<br>"),
                    HTML("<br>"),
                    
                    h4("A purpose of this application"),
                    
                    p(
                        "(i) ..."
                    ),
                    
                    p(HTML(
                        "(ii) ..."
                    )),
                    
                    HTML("<hr>")
                    
                ),
                
                
                ## the second tab ---------------------------------------------
                
                tabPanel(
                    
                    title = "Decision-making flowchart",
                    
                    value = "flowchart_tab",
                    
                    HTML("<br>"),
                    HTML("<br>"),
                    HTML("<br>"),
                    
                    h4("Decision-making flowchart"),
                    
                    p(HTML(
                        "The flowchart for decision making will be here (...)"
                    )),
                    
                    HTML("<hr>")
                    
                ),
                
                
                ## the third tab ----------------------------------------------
                
                navbarMenu(
                    
                    title = "Some methods (...)",
                    
                    ###########################################################
                    
                    ## method 1 ------------------------------------------------
                    
                    tabPanel(
                        
                        title = "Method 1",
                        
                        value = "method_1_tab",
                        
                        HTML("<br>"),
                        HTML("<br>"),
                        HTML("<br>"),
                        
                        h4("Method 1"),
                        
                        p(HTML(
                          "This is just the first statistical method (...)"
                        )),
                        
                        HTML("<hr>")
                        
                    ),
                    
                    
                    ## method 2 ------------------------------------------------
                    
                    tabPanel(
                        
                        title = "Method 2",
                        
                        value = "method_2_tab",
                        
                        HTML("<br>"),
                        HTML("<br>"),
                        HTML("<br>"),
                        
                        h4("Method 2"),
                        
                        p(HTML(
                          "This is just the second statistical method (...)"
                        )),
                        
                        HTML("<hr>")
                        
                    ),
                    
                    
                    ## --------------------------------------------------------
                    
                    ###########################################################
                    
                    
                ),
                
                
                ## the fourth tab ---------------------------------------------
                
                tabPanel(
                    
                    title = "About application",
                    
                    value = "about_tab",
                    
                    HTML("<br>"),
                    HTML("<br>"),
                    HTML("<br>"),
                    
                    h4("About application"),
                    
                    p(HTML(
                        "Something about the application ..."
                    )),
                    
                    HTML("<hr>")
                    
                )
                
            )
            
        )
        
    )
    
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################






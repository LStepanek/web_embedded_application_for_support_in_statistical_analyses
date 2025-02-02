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


## I am loading all individual tabs -------------------------------------------

my_global_working_directory <- getwd()

setwd(
    paste(
        my_global_working_directory,
        "tabs",
        sep = "/"
    )
)

for(
    my_tab in dir()
){
    
    source(
        file = my_tab,
        echo = FALSE,
        encoding = "utf-8"
    )
    
}

setwd(
    my_global_working_directory
)


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
                
                selected = "tab_introduction",
                
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
                                id = "tooltip_univesity_of_ostrava"
                                target = "_blank"
                            >
                                <img
                                    src = "logo_univesity_of_ostrava.png",
                                    style = "height: 30px;"
                                >
                            </a>
                            <a
                                href = "https://fis.vse.cz/english/"
                                id = "tooltip_faculty_of_informatics_and_statistics"
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
                            <script>
                                $("#tooltip_cc").attr(
                                    "title",
                                    "CC BY-NC-ND 3.0 CZ"
                                );
                                $("#tooltip_univesity_of_ostrava").attr(
                                    "title",
                                    "Faculty of Science, University of Ostrava"
                                );
                                $("#tooltip_faculty_of_informatics_and_statistics").attr(
                                    "title",
                                    "Facuty of Informatics and Statistics, Prague University of Economics and Business"
                                );
                            </script>
                            '
                        ),
                        style = "opacity: 1.00; z-index: 1000;"
                    )
                    
                ),
                
                # CSS bootstrap will be here
                theme = "bootstrap.css",
                
                
                ## the first tab ----------------------------------------------
                
                tab_introduction,
                
                
                ## the second tab ---------------------------------------------
                
                tab_flowchart,
                
                
                ## the third tab ----------------------------------------------
                
                navbarMenu(
                    
                    title = "Some methods (...)",
                    
                    ###########################################################
                    
                    ## method 1 ------------------------------------------------
                    
                    tab_method_1,
                    
                    
                    ## method 2 ------------------------------------------------
                    
                    tab_method_2,
                    
                    
                    ## --------------------------------------------------------
                    
                    ###########################################################
                    
                    
                ),
                
                
                ## the fourth tab ---------------------------------------------
                
                tab_about
                
            )
            
        )
        
    )
    
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################






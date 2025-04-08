###############################################################################
###############################################################################
###############################################################################

## I am loading packages ------------------------------------------------------

for(
    my_package in c(
        
        "shiny",
        "shinyjs",
        "shinythemes",
        "shinytoastr",
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

        # CSS bootstrap
        theme = shinytheme("spacelab"),

        tagList(
        
            ## ----------------------------------------------------------------
            
            ###################################################################
            
            ## CSS & JS settings ---------------------------------------------------

            tags$head(
                tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
                tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
                tags$script(src = "functions.js")
            ),
            
            
            ## busy indicator will be here ------------------------------------
            
            
            
            ## I am calling shinyjs functionalities ---------------------------
            
            useShinyjs(),
            useToastr(),
            
            
            ## user interface of the application ------------------------------
            
            navbarPage(
                
                ## header -----------------------------------------------------
            
                title = tags$span(class = "project-title", "Statistical tools"),
                
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
                                &nbsp; | &nbsp;
                                version 0.0.1
                                &nbsp; | &nbsp;
                                
                                <!-- licence and its logo -->
                                <a
                                    href =
                                    "https://creativecommons.org/
                                    licenses/by-nc-nd/3.0/cz/"
                                    id = "tooltip_cc"
                                    title = "CC BY-NC-ND 3.0 CZ"
                                    target = "_blank"
                                >
                                    <img
                                        src = "images/cc_by_nc_nd.png",
                                        style = "height: 20px;"
                                    />
                                </a>
                                
                                <!-- licence abbreviation -->
                                &nbsp;
                                | &nbsp; CC BY-NC-ND 3.0 CZ &nbsp;
                                | &nbsp; 2025 &nbsp;
                                | &nbsp;
                                
                                <!-- technical suppport -->
                                A project within 
                                <a
                                    href = "https://portal.osu.cz/stag?urlid=prohlizeni-predmet-sylabus&predmetZkrPrac=KIP&predmetZkrPred=XRPR2&predmetRok=2025"
                                    target = "_blank"
                                >
                                    XRPR1/2 Team project
                                </a>
                            ',
                            '
                            <!-- visit counter -->
                            <span style = "float:right">
                            ',
                            paste(
                                "<b>",
                                textOutput(
                                    "my_counter",
                                    inline = TRUE
                                ),
                                "&emsp; </b>",
                                sep = ""
                            ),
                            '
                            <!-- logos of faculties and universities -->
                            <a
                                href = "https://prf.osu.eu/"
                                target = "_blank"
                            >
                                <img
                                    src = "images/logo_univesity_of_ostrava.png",
                                    title = "Faculty of Science, University of Ostrava"
                                    style = "height: 30px;"
                                />
                            </a>
                            <a
                                href = "https://fis.vse.cz/english/"
                                title = "Facuty of Informatics and Statistics, Prague University of Economics and Business"
                                target = "_blank"
                            >
                                <img
                                    src = "images/logo_faculty_of_informatics_and_statistics.png",
                                    style = "height: 20px;"
                                />
                            </a>
                            </span>
                            '
                        ),
                        style = "opacity: 1.00; z-index: 1000;"
                    )
                    
                ),
                
                
                ## the first tab ----------------------------------------------
                
                tab_introduction,
                
                
                ## the second tab ---------------------------------------------
                
                tab_flowchart,
                
                
                ## the third tab ---------------------------------------------
                
                tab_upload,
                
                
                ## the fourth tab --------------------------------------------
                
                navbarMenu(
                    
                    title = "Some methods (...)",

                    menuName = "statistic-methods",  
                    
                    ###########################################################
                    
                    ## method 1 ------------------------------------------------
                    
                    tab_summary_plots,
                    
                    
                    ## method 2 ------------------------------------------------
                    
                    tab_method_2,
					
					
                    ## method 2 ------------------------------------------------
                    
                    tab_two_sample_t_test
					
                    
                    ## --------------------------------------------------------
                    
                    ###########################################################
                    
                    
                ),
                
                
                ## the fifth tab ----------------------------------------------
                
                tab_about
                
            )
            
        )
        
    )
    
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

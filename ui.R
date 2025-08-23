###############################################################################
###############################################################################
###############################################################################


## I am loading a module with user-defined functions --------------------------

source(
    file = "functions.R",
    echo = FALSE,
    encoding = "UTF-8"
)


## I am loading packages ------------------------------------------------------

# List of packages to be installed and loaded
required_packages <- c("shiny", "shinyjs", "shinycssloaders", "shinythemes", "shinytoastr", "DT")

# Call the function to install and load the packages
install_and_load_packages(required_packages)


## I am loading all individual tabs -------------------------------------------

for (my_tab in list.files("tabs", pattern = "^tab_.*\\.R$", full.names = TRUE)) {
    source(file = my_tab, encoding = "utf-8")
}


## ----------------------------------------------------------------------------

###############################################################################

## I am creating a user interface / frontend of the application ---------------

my_ui <- shinyUI(
    
    fluidPage(

        # CSS bootstrap
        theme = shinytheme("cerulean"),

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
                      id = "panel-footer",
                      HTML(
                          '
                          <!-- application name -->
                          <font size="3">Statistical tools</font>
                          
                          <!-- application version -->
                          &nbsp; | &nbsp; version 0.0.1 &nbsp; | &nbsp;
                          
                          <!-- licence and its logo -->
                          <a
                              href="https://creativecommons.org/licenses/by-nc-nd/3.0/cz/"
                              id="tooltip_cc"
                              title="CC BY-NC-ND 3.0 CZ"
                              target="_blank"
                          >
                              <img src="images/cc_by_nc_nd.png" />
                          </a>
                          
                          <!-- licence abbreviation -->
                          &nbsp; | &nbsp; CC BY-NC-ND 3.0 CZ &nbsp; | &nbsp; 2025 &nbsp; | &nbsp;
                          
                          <!-- technical support -->
                          A project within
                          <a
                              href="https://portal.osu.cz/stag?urlid=prohlizeni-predmet-sylabus&predmetZkrPrac=KIP&predmetZkrPred=XRPR2&predmetRok=2025"
                              target="_blank"
                          >
                              XRPR1/2 Team project
                          </a>
                          
                          <!-- visit counter -->
                          <span class="right-content">
                              <b>', paste(textOutput("my_counter", inline = TRUE)), '</b>&emsp;
                              
                              <!-- logos of faculties and universities -->
                              <a href="https://prf.osu.eu/" target="_blank">
                                  <img
                                      src="images/logo_univesity_of_ostrava.png"
                                      title="Faculty of Science, University of Ostrava"
                                  />
                              </a>
                              <a
                                  href="https://fis.vse.cz/english/"
                                  title="Faculty of Informatics and Statistics, Prague University of Economics and Business"
                                  target="_blank"
                              >
                                  <img src="images/logo_faculty_of_informatics_and_statistics.png" />
                              </a>
                          </span>
                          '
                      )
                  )

                ),
              
                
                ## the first tab ----------------------------------------------
                tab_introduction,
                
                ## the second tab ---------------------------------------------
                tab_flowchart,
                
                ## the third tab ---------------------------------------------
                tab_upload,
                
                ## the fourth tab --------------------------------------------
                tab_ai_insight,

                ## the fourth tab --------------------------------------------
                navbarMenu(
                    
                    title = "Statistical methods",
                    menuName = "statistical-methods",  
                    
                    ###########################################################
                    
                    ## method 1 -----------------------------------------------
                    tab_summary_plots,
                    
                    ## method 2 -----------------------------------------------
                    #tab_method_2,
                    
                    ## two-sample t-test --------------------------------------
                    tab_two_sample_t_test,
                    
                    ## paired t-test ------------------------------------------
                    tab_paired_t_test,
                    
                    ## Mann-Whitney test --------------------------------------
                    tab_mann_whitney,
                    
                    ## Mann-Whitney test --------------------------------------
                    tab_paired_wilcoxon,
                    
                    ## Mann-Whitney test --------------------------------------
                    tab_anova
                    
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

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


## ----------------------------------------------------------------------------

###############################################################################

## I am creating a backend of the application ---------------------------------

my_server <- function(
    
    input,
    output,
    session
    
){
    
    ## I am introducing a counter ---------------------------------------------
    
    output$my_counter <- renderText({
        
        if(
            ! file.exists("my_counter.Rdata")
        ){
            
            my_counter <- 0
            
        }else{
            
            load(file = "my_counter.Rdata")
            
        }
        
        my_counter <- my_counter + 1
        
        save(
            my_counter,
            file = "my_counter.Rdata"
        )
        
        paste(
            "Count of visits: ",
            my_counter,
            sep = ""
        )
        
    })
    
    
    ## ------------------------------------------------------------------------
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################






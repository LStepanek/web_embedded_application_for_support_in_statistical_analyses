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
  "ggplot2",
  "readxl",
  "jsonlite",
  "openai",
  "commonmark"
)

# Call the function to install and load the packages
install_and_load_packages(required_packages)


## I am loading all server modules --------------------------------------------

for (f in list.files("server", pattern = "^server_.*\\.R$", full.names = TRUE)) {
  source(f, encoding = "UTF-8")
}


## ----------------------------------------------------------------------------

###############################################################################

## I am creating a backend of the application ---------------------------------

my_server <- function(

    input,
    output,
    session

){

  ## I am introducing a counter -----------------------------------------------
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


  ## Global (shared) variables  -----------------------------------------------
  # shared reactive dataframe used by tabs that perform calculations
  my_data <- reactiveVal(NULL)  


  ## Register all system information variables for about tab ------------------
  register_sysinfo_outputs(output)


  # Call server logic for each tab (server modules) ---------------------------
  # Each function handles the reactive and output logic for a specific tab or statistical method

  aboutServer(input, output, session)
  ai_insightServer(input, output, session, my_data)
  dataUploadServer(input, output, session, my_data)
  flowchartServer(input, output, session)
  method_anovaServer(input, output, session, my_data)
  method_mannWhitneyServer(input, output, session, my_data)
  method_pairedTTestServer(input, output, session, my_data)
  method_pairedWilcoxonServer(input, output, session, my_data)
  method_summaryPlotsServer(input, output, session, my_data)
  method_twoSampleTTestServer(input, output, session, my_data)
  introductionServer(input, output, session)

}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################


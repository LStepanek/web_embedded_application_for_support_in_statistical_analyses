###############################################################################
###############################################################################
###############################################################################

SAMPLE_DATASETS_DIR <- "datasets"
ALLOWED_DATA_FILES  <- c(".csv", ".txt", ".tsv", ".xls", ".xlsx")


## error and warning handling -------------------------------------------------

# options(
    # shiny.error = function(){
        # stop("Processing.")
    # }
# )                       # I am turning off the messaging of benign errors

# options(warn = -1)      # I am turning off the messaging of benign warnings

# this globally suppress the temporary visual red errors in shiny interface
options(
    shiny.sanitize.errors = TRUE
)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

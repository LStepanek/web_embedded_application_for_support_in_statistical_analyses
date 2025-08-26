###############################################################################
###############################################################################
###############################################################################

APP_VERSION <- "0.0.9"

SAMPLE_DATASETS_DIR <- "datasets"
ALLOWED_DATA_FILES  <- c(".csv", ".json", ".txt", ".tsv", ".xls", ".xlsx")
DATA_TYPE_OPTIONS <- c("integer", "numeric", "logical", "character", "date", "POSIXct")


## error and warning handling -------------------------------------------------

# options(
#   shiny.error = function() {
#     stop("Processing.")
#   }
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

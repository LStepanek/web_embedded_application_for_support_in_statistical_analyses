###############################################################################
###############################################################################
###############################################################################

## user-defined functions -----------------------------------------------------

# Installs and loads specified packages if they are not already installed.
install_and_load_packages <- function(packages) {
    for (my_package in packages) {
        if (!(my_package %in% rownames(installed.packages()))) {
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
}


# Get a named list of all supported data files from a directory
get_builtin_named_datasets <- function(dir_path = SAMPLE_DATASETS_DIR, allowed_exts = ALLOWED_DATA_FILES) {
  all_files <- list.files(dir_path, full.names = FALSE)

  file_names <- all_files[sapply(all_files, function(f) {
    any(sapply(allowed_exts, function(ext) grepl(paste0(ext, "$"), f, ignore.case = TRUE)))
  })]

  display_names <- sub(paste0("(", paste(allowed_exts, collapse = "|"), ")$"), "", file_names)
  named_choices <- setNames(file_names, display_names)

  return(named_choices)
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################






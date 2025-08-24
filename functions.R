###############################################################################
###############################################################################
###############################################################################

## user-defined functions -----------------------------------------------------

# Installs and loads specified packages if they are not already installed
install_and_load_packages <- function(packages) {
    for (my_package in packages) {
        if (!(my_package %in% rownames(installed.packages()))) {
            install.packages(
                my_package,
                dependencies = TRUE,
                repos = "https://cloud.r-project.org"
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


# Register system information (R/Shiny versions, OS, CPU name, number of cores, and total RAM) outputs
register_sysinfo_outputs <- function(output) {

    # Helper function to detect CPU name depending on the operating system
    get_cpu_name <- function() {
      sys <- Sys.info()[["sysname"]]
      if (sys == "Linux") {
        system("cat /proc/cpuinfo | grep 'model name' | head -1 | cut -d: -f2", intern = TRUE)
      } else if (sys == "Windows") {
        system("wmic cpu get name", intern = TRUE)[2]
      } else if (sys == "Darwin") {
        system("sysctl -n machdep.cpu.brand_string", intern = TRUE)
      } else {
        "Unknown CPU"
      }
    }
    
    # Detect logical and physical cores
    cores_logical <- parallel::detectCores(logical = TRUE)
    
    get_physical_cores <- function() {
      sys <- Sys.info()[["sysname"]]
      if (sys == "Linux") {
        uniq <- system("grep 'core id' /proc/cpuinfo | sort -u | wc -l", intern = TRUE)
        as.integer(uniq)
      } else {
        parallel::detectCores(logical = FALSE)
      }
    }
    
    cores_physical <- get_physical_cores()
    
    # Collect system information into a named list
    sysinfo <- list(
      r_version = paste(R.version$major, R.version$minor, sep = "."),
      shiny_ver = as.character(packageVersion("shiny")),
      os        = paste(Sys.info()[["sysname"]], Sys.info()[["release"]]),
      cpu       = get_cpu_name(),
      cores     = paste0(cores_physical, " physical / ", cores_logical, " logical"),
      ram_gb    = round(ps::ps_system_memory()$total / 1024^3, 1)
    )
    
    # Assign each piece of system information to the corresponding output
    output$r_version <- renderText({ sysinfo$r_version })
    output$shiny_ver <- renderText({ sysinfo$shiny_ver })
    output$os        <- renderText({ sysinfo$os })
    output$cpu       <- renderText({ sysinfo$cpu })
    output$cores     <- renderText({ sysinfo$cores })
    output$ram       <- renderText({ paste(sysinfo$ram_gb, "GB") })
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

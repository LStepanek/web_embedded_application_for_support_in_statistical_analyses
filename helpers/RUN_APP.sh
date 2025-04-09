#!/bin/bash

if ! command -v Rscript &> /dev/null
then
    echo "Error: Rscript not found. Please install R."
    exit 1
fi

Rscript -e "shiny::runApp('../', launch.browser = TRUE)"

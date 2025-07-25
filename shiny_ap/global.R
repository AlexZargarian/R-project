
required_packages <- c(
  "shiny",
  "dplyr",       # For data manipulation
  "ggplot2",     # For plotting
  "leaflet",     # For interactive maps
  "readr"        # For robust reading of CSV files (optional, but good practice)

)

check_and_load_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    message("Installing missing packages: ", paste(new_packages, collapse = ", "))
    install.packages(new_packages, dependencies = TRUE)
  }
  message("Loading required packages...")
  lapply(packages, library, character.only = TRUE, quietly = TRUE)
}

check_and_load_packages(required_packages)

message("All required packages are loaded.")


#Change this line to make the wd the one with the "master.R" file
setwd("~/Desktop/ITAM/9no Semestre/Inferencia Causal/Talleres/Taller1/")


libraries_to_check <- c("ggplot2", "gridExtra", "latex2exp", "REAT", "ggtext",
                        "readr", "dplyr", "scales", "DescTools", "stringr")

# Create a function to check if a library is installed
is_installed <- function(package) {
  return(package %in% installed.packages()[,"Package"])
}

# Install and load libraries that are not installed
for (library_name in libraries_to_check) {
  if (!is_installed(library_name)) {
    install.packages(library_name)
  }
  library(library_name, character.only = TRUE)
}

source("03_Scripts/Problema1.R")
source("03_Scripts/Problema2.R")
source("03_Scripts/Problema3.R")
source("03_Scripts/Problema4.R")


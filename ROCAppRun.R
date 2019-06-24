# list all packages required for app
pkgs <- c("adehabitatHR", 
          "data.table", 
          "fasterize",
          "ggplot2",
          "leaflet", 
          "maptools",
          "pacman",
          "raster", 
          "rasterVis", 
          "rgdal", 
          "rgeos",
          "sf", 
          "shiny",
          "shinyFiles")

# check for installation gaps and rectify by installing missing
pkgs_to_install <- setdiff(pkgs, installed.packages())
#install.packages(pkgs_to_install)
#install.packages("BHSdataPNF_1.0.tar.gz", repos = NULL, type = "source")
#install.packages("BHSforay_1.1.1.tar.gz", repos = NULL, type = "source")
library(BHSforay)
#library(devtools)NF
#install_github('wleepang/shiny-directory-input')
library(shinyDirectoryInput)
# load all packages

lapply(pkgs, require, character.only = TRUE)

# TO DO: figure out how to source in local functions here.

source("./ROCApp/spatFull.R")

runApp("./ROCApp/")

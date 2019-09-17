# This script is where packages are installed and parameters are defined for 
# the other scripts within this workflow. 

# About neonUtilities: https://www.neonscience.org/neonDataStackR
# About geoNEON: https://github.com/NEONScience/NEON-geolocation/tree/master/geoNEON

# Load necessary packages. 
# If R says "Error in loadNamespace(name) : there is no package called 'packageName',
# Use install.packages("package_name") unless otherwise specified. 
library(neonUtilities) 
library(geoNEON)
#install.packages("devtools")
#library(devtools)
#install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE)
library(dplyr)
library(stringr)
library(sf)
# following packages needed for the clip_overlap custom function
library(sp) 
library(raster)
library(rgeos)

# Load custom supporting functions 
source("00-supporting_functions.R")

# Create folders to store the raw and processed data
check_create_dir("data")
dir_data_raw <- file.path("data","data_raw")
dir_data_out <- file.path("data","data_output")
check_create_dir(dir_data_raw)
check_create_dir(dir_data_out)

# Define parameters for the in-situ woody plant vegetation data to download


# Define parameters for the AOP remote sensing data to download
site_code <- "NIWO"       # four-digit NEON site code 
aop_data_year <- "2017"   # four-digit year in character string "YYYY" for AOP imagery
buffer_val <- 5 #[m]       # integer buffer size around tree coordinates

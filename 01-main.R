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
library(tools)
library(randomForest) 


# Load custom supporting functions 
source("00-supporting_functions.R")

# Create folders to store the raw and processed data
check_create_dir("data")
dir_data_raw <- file.path("data","data_raw")
dir_data_out <- file.path("data","data_output")
check_create_dir(dir_data_raw)
check_create_dir(dir_data_out)
# Create a folder to store outputs for analysis 
dir_analysis <- file.path("analysis")
check_create_dir(dir_analysis)


# Download in-situ woody vegetation data ----------------------------------

# USER-DEFINED-INPUTS
site_code <- "NIWO"         # Four-digit NEON site code, character string type.
veg_start_date <- "2016-01" # Starting date and ending dates of woody veg data 
veg_end_date <- NA          # to download, using the "YYYY-MM" format. 
                            # Set to NA for earliest/latest data possible.

# Define parameters for the in-situ woody plant vegetation data to download.
# Let's load the in-situ Woody Vegetation Structure data straight into R.
# Specify the NEON site and starting/ending date(s) for the data. 
# A message in the console will display the total file size to be downloaded.
# Proceed by typing "y" and pressing Enter. 
# The woody vegetation structure tables are 
veg_raw <- neonUtilities::loadByProduct(dpID = "DP1.10098.001"   
                                        ,site = site_code              
                                        ,startdate = veg_start_date      
                                        ,enddate = veg_end_date
                                        ,package = "basic"          
                                        ,check.size = T)


# Create features (points or polygons) for each tree ----------------------

# Points, polygons with the maximum crown diameter, and polygons with 
# half the maximum crown diameter per tree are created and written
# to .shp files in the data/data_output directory. 

source("02-create_tree_features.R")


# Process tree features ---------------------------------------------------

# USER-DEFINED INPUTS
px_thresh <- 2 # area threshold, integer count of of 1m by 1m pixels, 
               # to filter out small tree crowns in the processing workflow

source("03-process_tree_features.R")


# Download airborne remote sensing data -----------------------------------

# Define parameters for the AOP remote sensing data to download
aop_data_year <- "2017"   # four-digit year in character string "YYYY" for AOP imagery
buffer_val <- 5 #[m]       # integer buffer size around tree coordinates

source("04-download_aop_imagery.R")


# Prepare AOP imagery for training set creation ---------------------------

# This script combines all airborne remote sensing data layers per square-km 
# tile into a single multi-layer raster, saved as an .rds file. 
source("05-prep_aop_imagery.R")

# read the text file with wavelength data for subsequent steps
wavelengths = as.numeric(unlist(read.table(file.path(dir_data_out,
                                                     "wavelengths.txt"),
                                           skip = 1,
                                           col.names = 'wavelength')))


# Plot AOP imagery --------------------------------------------------------

# USER-DEFINED-INPUT
# Specify which tile to plot. 452000_4432000 has a visible road. 
east_north_string <- "452000_4432000"

source("06-plot_aop_imagery.R")


# Create training data for species classification -------------------------

# USER-DEFINED INPUT 
# shapefile containing tree points or polygons
shapefile_filename <- file.path(dir_data_out, "veg_points_all.shp")

source("07-extract_training_features.R")

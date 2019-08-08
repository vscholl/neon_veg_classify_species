# This script downloads NEON woody vegetation data from the API and loads 
# it directly into R. 
# 
# About neonUtilities: https://www.neonscience.org/neonDataStackR
# About geoNEON: https://github.com/NEONScience/NEON-geolocation/tree/master/geoNEON

# Load necessary packages
library(neonUtilities)
library(geoNEON)
library(dplyr)

# Load custom supporting functions 
source("00-supporting_functions.R")

# Create folders to store the raw and processed data
check_create_dir("data")
check_create_dir("data/data_raw")
check_create_dir("data/data_output")


# ------------------------------------------------------------------------
# # Download the files locally from NEON API using the zipsByProduct function. 
# # They will be stored within "data/data_raw/filesToStack#####" where "#####" 
# # is the middle portion of the data product ID string. 
# # Since the data product ID is "DP1.10098.001", files will be saved 
# # in "filesToStack10098". 
# neonUtilities::zipsByProduct(dpID = "DP1.10098.001"   
#                              ,site = "NIWO"              
#                              ,startdate = "2016-01"      
#                              #,enddate = "YYYY-MM"
#                              ,package = "basic"          
#                              ,savepath = "data/data_raw"
#                              ,check.size = T)
# 
# # The downloaded files can now be passed to stackByTable() to be stacked. 
# # They are saved within a folder called "stackedFiles" within the "filesToStack10098) directory. 
# neonUtilities::stackByTable(filepath = "data/data_raw/filesToStack10098" 
#                             ,folder = T)
# ------------------------------------------------------------------------




# Let's load the in-situ data straight into R
veg_raw <- neonUtilities::loadByProduct(dpID = "DP1.10098.001"   
                                              ,site = "NIWO"              
                                              ,startdate = "2016-01"      
                                              #,enddate = "YYYY-MM"
                                              ,package = "basic"          
                                              ,check.size = T)


# Remove duplicate IndividualID entries in mappingandtagging; keep most recent
veg_mapping_unique <- veg_raw$vst_mappingandtagging %>% 
  dplyr::group_by(individualID) %>%
  dplyr::slice(which.max(as.Date(date)))

# Create a data frame to keep track of the number of trees after each step.
# Count how many trees there are in the input mapping_and_tagging file. 
tree_counts <- data.frame(count = c(nrow(veg_mapping_unique))
                         ,description = c("unique IDs in mappingandtagging"))


# Calculate mapped UTM locations of stems based on distance and azimuth.
veg_loc <- geoNEON::def.calc.geo.os(data = veg_raw$vst_mappingandtagging,
                                        dataProd = "vst_mappingandtagging")

# Remove all rows with NA coordinates (they had missing inputs for easting, 
# northing, or UTM zone and were not converted)
veg_utm <- veg_loc[complete.cases(veg_loc[, c("adjNorthing"
                                          ,"adjEasting")]),]


# Count how many trees have UTM coordinates
tree_counts <- rbind(tree_counts,
                     data.frame(count = c(nrow(veg_utm))
                ,description = c("entries w calculated UTM coordinates")))


# Get a list of unique easting, northing coordinates of mapped stems.
# These can be used to pull AOP imagery that cover the same area. 
veg_coordinates <- data.frame(eastings = veg_utm$adjEasting,
                              northings = veg_utm$adjNorthing) %>%
  dplyr::distinct()


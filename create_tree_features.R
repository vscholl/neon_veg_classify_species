# This script downloads NEON woody vegetation data from the API and loads 
# it directly into R. 
# 
# About neonUtilities: https://www.neonscience.org/neonDataStackR
# About geoNEON: https://github.com/NEONScience/NEON-geolocation/tree/master/geoNEON

# Load necessary packages
library(neonUtilities)
library(geoNEON)
library(dplyr)
library(stringr)
library(sf)

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



# mappingandtagging -----------------------------------------------------------
# This data table contains mapped tree stems using distance and azimuth from
# plot-specific reference points. 

# Remove duplicate IndividualID entries in mappingandtagging; keep most recent
veg_mapping_unique <- veg_raw$vst_mappingandtagging %>% 
                        dplyr::group_by(individualID) %>%
                          dplyr::slice(which.max(as.Date(date)))

# Create a data frame to keep track of the number of trees after each step.
# Count how many trees there are in the input mapping_and_tagging file. 
tree_counts <- data.frame(count = c(nrow(veg_mapping_unique))
                         ,description = c("mappingandtagging unique IDs"))


# Calculate mapped UTM locations of stems based on distance and azimuth.
veg_loc <- geoNEON::def.calc.geo.os(data = veg_mapping_unique
                                   ,dataProd = "vst_mappingandtagging") %>% 
  # Rename the adjEasting and adjNorthing columns 
  dplyr::rename(easting = adjEasting, northing = adjNorthing)

# Remove all rows with NA coordinates (they had missing inputs for easting, 
# northing, or UTM zone and were not converted)
veg_utm <- veg_loc[complete.cases(veg_loc[, c("northing"
                                          ,"easting")]),]


# Count how many trees have UTM coordinates
tree_counts <- rbind(tree_counts, data.frame(count = c(nrow(veg_utm))
  ,description = c("mappingandtagging entries w calculated UTM coordinates")))



# apparentindividual ------------------------------------------------------
# This data table contains height & crown diameter data per individual veggie. 

# Remove any duplicate individalID entries; keep most recent 
veg_individual <- veg_raw$vst_apparentindividual %>% 
                    dplyr::group_by(individualID) %>%
                      dplyr::slice(which.max(as.Date(date)))

# Keep only the entries with crown diameter and tree height
# since these measurements are needed to create and compare polygons.
# FUTURE-WORK: include ninetyCrownDiameter? 
veg_ind <- veg_individual[complete.cases(veg_individual$height) & 
                          complete.cases(veg_individual$maxCrownDiameter),]

# Count how many trees have height and maximum crown diameter measurements
tree_counts <- rbind(tree_counts, data.frame(count = c(nrow(veg_ind))
      ,description = c("apparentindividual entries w height & maxCrownDiam")))


# Merge stem locations with structural measurements -----------------------

# Match structural measurements from apparent_individual with 
# mapping_and_tagging entries based on individualID.
veg_merged <- merge(veg_utm
                    ,veg_ind
                    , by = "individualID") 

# Count how many entries have stem locations, height, & crown diam
tree_counts <- rbind(tree_counts, data.frame(count = c(nrow(veg_merged))
      ,description = c("entries w mapped location, height, & maxCrownDiam")))


# Write merged mappingandtagging and apparentindividual entries to csv
write.csv(veg_merged, file = "data/data_output/vst_merged.csv")


# Get a list of unique easting, northing coordinates of mapped stems. 
# These can be used to pull AOP imagery that cover the same area using 
# the neonUtilities::byTileAOP function.
veg_coordinates <- data.frame(eastings = veg_merged$easting,
                              northings = veg_merged$northing) %>%
  dplyr::distinct()

# Generate a list of tiles if instead you decide to download manually from 
# the NEON Data portal. A text file is written with tile coordinates. 
tiles <- list_tiles_with_veg(veg_df = veg_merged
                            ,out_dir = "data/data_raw/")



# Remove multi-bole entries -----------------------------------------------

# Identify multi-bole entries: sometimes, there are multiple entries with 
# identical coordinates, height, and crown diameter (but their individualID's 
# are different, with "A", "B", etc. appended on the end of the last five 
# numbers.) In this step, the individualID values are assessed to find 
# multi-bole sets. If the coordinates, height, and crown diameters are 
# identical, then the entry with a letter appended on the end is deleted 
# from the analysis. This prevents duplicate polygons being used to extract 
# spectra. 

# List all individual ID strings 
ind_IDs_all <- as.character(unique(droplevels(veg_merged$individualID)))

# Split each individual ID using a period delimiter
last_id_section <- sapply(stringr::str_split(ind_IDs_all, "[.]"), tail, 1)

# Create another list without any letters
last_digits <- gsub("[^0-9]", "", last_id_section)

# Create a boolean vector where bole entries (which contain letters)
# are True, but stem entries (without letters) are False 
is_bole <- last_id_section != last_digits

# Create a lookup table with the id information
id_lut <- as.data.frame(last_digits) %>% 
  dplyr::mutate(individualIDs_all = ind_IDs_all
                ,last_id_section = last_id_section
                ,is_bole = is_bole
                ,height = veg_merged$height
                ,maxCrownDiameter = veg_merged$maxCrownDiameter)

# Count the frequency of each ID number. Identify the ID's with more than 
# one entry. 
multiple_ids <- as.data.frame(table(last_digits)) %>% 
  dplyr::filter(Freq >1)

# Create a list to populate with individualIDs to remove
remove_ids <- c()

# loop through the ID's that appear more than once in the data set
for(id in as.character(multiple_ids$last_digits)){
  
  # get the complete individual IDs 
  duplicates <- print(id_lut[id_lut$last_digits == id,])
  
  # see if the height and diameter values are identical 
  if(var(duplicates$height)==0 && var(duplicates$maxCrownDiameter) == 0){
    remove_ids <- c(remove_ids, 
                    duplicates$individualIDs_all[duplicates$is_bole==TRUE])
  }
  
}

# Remove the entries with the multi-bole individualIDs identified in the 
# previous step. 
veg_multibole_removed <- veg_merged %>% 
  dplyr::filter(!(individualID %in% remove_ids))

# Count how many trees are left after removing multi-bole entries
tree_count <- rbind(tree_counts
                    ,data.frame(count = c(nrow(veg_multibole_removed))
    ,description = c(" entries remain after removing multi-bole entries")))


# Create point and polygon features for each woody vegetation individual ------


# Get the Coordinate Reference System (CRS) for these stem coordinates based
# on the vst_plotperyear table, which contains geodetic datum and UTM zone 
datum <- as.character(veg_raw$vst_perplotperyear$geodeticDatum[1])
zone <- gsub("[^0-9\\.]", "", veg_raw$vst_perplotperyear$utmZone[1])
coord_ref <- sf::st_crs(paste("+proj=utm +zone=",zone, 
                       " +datum=",datum," +units=m",sep=""))


# Write shapefile with a POINT for every mapped stem. 
# NOTE this includes multi-bole entries and individuals without height or 
# crown diameter.
mapped_stems_sf <- sf::st_as_sf(x = veg_utm
                             ,coords = c("easting", "northing")
                             ,crs = coord_ref)
sf::st_write(obj = mapped_stems_sf
             ,dsn = "data/data_output/all_mapped_stem_points.shp")

# plot all mapped stem locations
# ggplot2::ggplot() +
#   ggplot2::geom_sf(data = all_stems_sf) +
#   ggplot2::ggtitle("Map of Stem Locations")

# Write shapefile with POINTs for mapped stems that also have 
# height & crown diameter
veg_merged_stems_sf <- sf::st_as_sf(x = veg_merged
                                ,coords = c("easting", "northing")
                                ,crs = coord_ref)
sf::st_write(obj = veg_merged_stems_sf
             ,dsn = "data/data_output/mapped_stem_points_w_height_diam.shp")

# Write shapefile with CIRCULAR POLYGONS for all mapped stems with 
# height & crown diameter
merged_buff_sf <- sf::st_buffer(x = veg_merged_stems_sf
                                # divide max diameter by 2 for the radius
                                ,dist = round((merged_stems_sf$maxCrownDiameter/2)))
sf::st_write(obj = merged_buff_sf
             ,dsn = "data/data_output/max_diam_polygons.shp")



# Write shapefile with CIRCULAR POLYGONS for all mapped stems with 
# height & crown diameter, after multibole entries were removed. 
multibole_removed_stems_sf <- sf::st_as_sf(x = veg_multibole_removed
                                    ,coords = c("easting", "northing")
                                    ,crs = coord_ref) 
multibole_removed_buff_sf <- sf::st_buffer(x = multibole_removed_stems_sf
                ,dist = round((multibole_removed_stems_sf$maxCrownDiameter/2)))
sf::st_write(obj = multibole_removed_buff_sf
             ,dsn = "data/data_output/max_diam_polygons_w_multibole_removed.shp")


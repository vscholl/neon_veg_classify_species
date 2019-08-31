# This script downloads NEON woody vegetation data from the API and loads 
# it directly into R. 


# Let's load the in-situ Woody Vegetation Structure data straight into R.
# Specify the NEON site and starting/ending date(s) for the data. 
# A message in the console will display the total file size to be downloaded.
# Proceed by typing "y" and pressing Enter. 
veg_raw <- neonUtilities::loadByProduct(dpID = "DP1.10098.001"   
                                              ,site = site_code              
                                              ,startdate = "2016-01"      
                                              #,enddate = "YYYY-MM"
                                              ,package = "basic"          
                                              ,check.size = T)

# ------------------------------------------------------------------------
# # Alternative option to loading data straight into R:
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


# Create point and polygon features for each woody vegetation individual ------


# Get the Coordinate Reference System (CRS) for these stem coordinates based
# on the vst_plotperyear table, which contains geodetic datum and UTM zone 
datum <- as.character(veg_raw$vst_perplotperyear$geodeticDatum[1])
zone <- gsub("[^0-9\\.]", "", veg_raw$vst_perplotperyear$utmZone[1])
coord_ref <- sf::st_crs(paste("+proj=utm +zone=",zone, 
                       " +datum=",datum," +units=m",sep=""))


# Write shapefile with a POINT for every mapped stem. 
# NOTE these include multi-bole entries and individuals without height or 
# crown diameter.
mapped_stems_sf <- sf::st_as_sf(x = veg_utm
                             ,coords = c("easting", "northing")
                             ,crs = coord_ref)
sf::st_write(obj = mapped_stems_sf
             ,dsn = "data/data_output/veg_points_all.shp"
             ,delete_dsn = TRUE)

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
             ,dsn = "data/data_output/veg_points_w_height_diam.shp"
             ,delete_dsn = TRUE)

# Write shapefile with CIRCULAR POLYGONS for all mapped stems with 
# height & crown diameter. Size: Maximum crown diameter
merged_buff_sf <- sf::st_buffer(x = veg_merged_stems_sf
                                # divide max diameter by 2 for the radius
                                ,dist = round((veg_merged_stems_sf$maxCrownDiameter/2)
                                              ,digits = 1))
sf::st_write(obj = merged_buff_sf
             ,dsn = "data/data_output/veg_polygons_max_diam.shp"
             ,delete_dsn = TRUE)


# Write shapefile with CIRCULAR POLYGONS for all mapped stems with 
# height & crown diameter. Size: 1/2 Maximum crown diameter
# merged_buff_sf_half_diam <- sf::st_buffer(x = veg_merged_stems_sf
#                                 # divide max diameter by 2 for the radius
#                                 ,dist = round((merged_stems_sf$maxCrownDiameter/4)
#                                               ,digits = 1))
# sf::st_write(obj = merged_buff_sf_half_diam
#              ,dsn = "data/data_output/veg_polygons_half_diam.shp"
#              ,delete_dsn = TRUE)
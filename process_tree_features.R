# In this script, processing steps are applied to the tree features 
# generated using in-situ NEON woody vegetation structure data: 
# 
# 1. Identical multi-bole entries are identified and excluded 
# from subsequent analysis to remove duplicated points or polygons present 
# in the raw data set. 
# 
# 2. An area threshold is then applied to remove any small trees area values 
# less than the area of four hyperspectral pixels. This threshold 
# was selected with the coarser resolution of the hyperspectral and LiDAR data 
# products in mind. By preserving the trees with larger crowns, it is believed 
# that purer spectra will be extracted for them for the training data sets, 
# as opposed to extracting mixed pixels which signal from smaller plants and 
# background materials or neighboring vegetation. 
# 
# 3. “Engulfed” polygons, those which are shorter and completely within the 
# boundaries of other polygons, are also present in the initial crown polygons.
# Since they likely cannot be observed from the airborne perspective, 
# “engulfed” polygons were removed from subsequent analysis. Remaining 
# polygons were checked for overlap with neighboring polygons. For each 
# overlapping pair of polygons, shorter polygons were clipped by taller 
# ones. If the remaining clipped area was smaller than the aforementioned area 
# threshold, it was deleted. 
#
# At this point, this workflow has generated a collection of polygons that 
# will theoretically intersect with independent pixels in the airborne remote 
# sensing data.


# Remove multi-bole entries -----------------------------------------------

# Identify multi-bole entries: sometimes, there are multiple entries with 
# identical coordinates, height, and crown diameter (but their individualID's 
# are different, with "A", "B", etc. appended on the end of the last five 
# numbers.) In this step, the individualID values are assessed to find 
# multi-bole sets. If the coordinates, height, and crown diameters are 
# identical, then the entry with a letter appended on the end is deleted 
# from the analysis. This prevents duplicate polygons being used to extract 
# spectra. 

remove_multibole <- function(df){
  
  # List all individual ID strings 
  ind_IDs_all <- as.character(unique(droplevels(df$individualID)))
  
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
                  ,height = df$height
                  ,maxCrownDiameter = df$maxCrownDiameter)
  
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
  multibole_removed <- df %>% 
    dplyr::filter(!(individualID %in% remove_ids))
  
  return(multibole_removed)
  
}

# Remove multibole entries from the merged woody veg data frame 
veg_multibole_removed <- remove_multibole(df = veg_merged)


# Count how many trees are left after removing multi-bole entries
tree_count <- rbind(tree_counts
                    ,data.frame(count = c(nrow(veg_multibole_removed))
      ,description = c(" entries remain after removing multi-bole entries")))


# Convert the data frame to an SF object 
veg_multibole_removed_sf <- sf::st_as_sf(veg_multibole_removed
                                         ,coords = c("easting", "northing")
                                         ,crs = coord_ref)

# Create circular polygons with diamter = maximum crown diameter 
polygons_multibole_removed <- sf::st_buffer(veg_multibole_removed_sf
                    ,dist = round((veg_multibole_removed_sf$maxCrownDiameter/2)
                              ,digits = 1))




# Apply area threshold to remove small polygons ---------------------------

# Define the area threshold in units of [m^2]
# Start with the area of RGB pixel, 10cm by 10cm
px_area_rgb <- 0.1 * 0.1 #[m^2]
# Gridded LiDAR products and HS pixels are 1m x 1m
px_area_hs <- 100 * px_area_rgb #[m^2]
# Multiply area of 1 HS pixel by the number of pixels; 4 in this case 
px_thresh <- 4
thresh <- px_area_hs * px_thresh #[m^2]

# Remove polygons with area < 4 hyperspectral pixels 
polygons_thresh <- polygons_multibole_removed %>% 
  dplyr::mutate(area_m2 = sf::st_area(polygons_multibole_removed)) %>% 
  dplyr::filter(as.numeric(area_m2) > thresh)



# Clip shorter polygons with taller polygons -----------------------------

polygons_clipped <- clip_overlap(polygons_thresh, thresh)

# Count how many trees are left after clipping areas of overlap and applying 
# area threshold to clipped polygons
tree_count <- rbind(tree_counts
                    ,data.frame(count = c(nrow(polygons_clipped))
          ,description = c(" entries remain after clipping overlapping regions")))


# Write shapefile with clipped tree crown polygons
sf::st_write(obj = polygons_clipped
             ,dsn = "data/data_output/veg_polys_clipped_overlap.shp"
             ,delete_dsn = TRUE)



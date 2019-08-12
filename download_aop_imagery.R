# This script downloads NEON AOP remote sensing data 
# and prepares them to become descriptive features for species classification.



# Download AOP data -------------------------------------------------------

# Download NEON AOP remote sensing tiles that cover the same area as the 
# in-situ woody vegetation data. We can specify specific coordinates using
# the tree locations from the woody vegetation structure data. 


# API call parameters 
site_code <- "NIWO"        # four-digit NEON site code 
data_year <- "2017"        # four-digit year YYYY 
dir_out <- "data/data_raw" # top-level directory to save downloaded files
buffer_val <- 5 #[m]       # integer buffer size around tree coordinates




# CHM ---------------------------------------------------------------------

dp_chm <- "DP3.30015.001"

neonUtilities::byTileAOP(
  dpID = dp_chm
  ,site = site_code
  ,year = data_year
  ,savepath = dir_out
  ,easting = veg_coordinates$eastings
  ,northing = veg_coordinates$northings
  ,buffer = buffer_val)


# aspect_slope ------------------------------------------------------------

dp_aspect_slope <- "DP3.30025.001"

neonUtilities::byTileAOP(
  dpID = dp_aspect_slope
  ,site = site_code
  ,year = data_year
  ,savepath = dir_out
  ,easting = veg_coordinates$eastings
  ,northing = veg_coordinates$northings
  ,buffer = buffer_val)



# RGB ---------------------------------------------------------------------

dp_rgb <- "DP3.30010.001"

neonUtilities::byTileAOP(
  dpID = dp_rgb
  ,site = site_code
  ,year = data_year
  ,savepath = dir_out
  ,easting = veg_coordinates$eastings
  ,northing = veg_coordinates$northings
  ,buffer = buffer_val)



# Vegetation indices ------------------------------------------------------

dp_veg_indices <- "DP3.30026.001"

neonUtilities::byTileAOP(
  dpID = dp_veg_indices
  ,site = site_code
  ,year = data_year
  ,savepath = dir_out
  ,easting = veg_coordinates$eastings
  ,northing = veg_coordinates$northings
  ,buffer = buffer_val)



# Hyperspectral reflectance -----------------------------------------------

dp_hs_refl <- "DP3.30006.001"

neonUtilities::byTileAOP(
  dpID = dp_hs_refl
  ,site = site_code
  ,year = data_year
  ,savepath = dir_out
  ,easting = veg_coordinates$eastings
  ,northing = veg_coordinates$northings
  ,buffer = buffer_val)









# Download all data for a given site, year, data product ------------------

# To download all AOP data for specified site, year, product 
neonUtilities::byFileAOP(
  dpID = dp_chm
  ,site = site_code
  ,year = data_year
  ,check.size = TRUE
  ,savepath = dir_out)




# Looping through data product downloads ----------------------------------



# List of Data Product IDs to download
# "DP3.30015.001" - "chm"               LiDAR-derived Canopy Height Model
# "DP3.30025.001" - "aspect_slope"      LiDAR-derived Aspect and Slope
# "DP3.30010.001" - "rgb"               High-resolution multispectral camera RGB 
# "DP3.30026.001" - "veg_indices"       Spectrometer-derived vegetation indices 
# "DP3.30006.001" - "hs_refl"           Spectometer-derived surface reflectance

data_products <- data.frame(id = c("DP3.30015.001"
                                   ,"DP3.30025.001"
                                   ,"DP3.30010.001"
                                   ,"DP3.30026.001"
                                   ,"DP3.30006.001")
                            ,name = c("chm"
                                      ,"aspect_slope"
                                      ,"rgb"
                                      ,"veg_indices"
                                      ,"hs_refl"))


# Can this be turned into a loop?
# How to make R wait for user input (y/n) and wait for the files to download? 

for (i in nrow(data_products)){
  
  dp_id <- as.character(data_products$id[i])
  dp_name <- as.character(data_products$name[i])
  
  print(paste("Downloading data product ID", dp_id))
  
  neonUtilities::byTileAOP(
    dp_id = dp_id
    ,site = site_code
    ,year = data_year
    ,savepath = dir_out
    ,easting = veg_coordinates$eastings
    ,northing = veg_coordinates$northings
    ,buffer = buffer_val)
  
} 




# Move downloaded AOP data ------------------------------------------------

# move the files intuituve folder name.
# For example, the default nested directories where RGB data is stored: 
#   "data/data_raw/DP3.30010.001/2017/FullSite/D13/2017_NIWO_1/L3/Camera/Mosaic/V01/2017_NIWO_1_453000_4433000_image.tif"
# to a simpler directory structure:
#   "data/data_raw/rgb/2017_NIWO_1_453000_4433000_image.tif"
# mainly for convenience if the user wants to quickly locate the AOP
# files for creating maps later.


# DP3.30010.001  high-resolution RGB multispectral imagery
neonUtilities::byTileAOP(
  dp_id = "DP3.30010.001"
  ,site = "NIWO"
  ,year = "2017"
  ,savepath = "data/data_raw"
  ,easting = veg_coordinates$eastings
  ,northing = veg_coordinates$northings
  ,buffer = 5)

# list all image filenames 
rgb_download_path <- "data/data_raw/DP3.30010.001"
list_rgb_files <- list.files(path = rgb_download_path
                             ,pattern = "*.tif$"
                             ,recursive = TRUE
                             ,full.names = TRUE)

# move rgb .tif images into a new folder called "rgb"
rgb_dir <- "data/data_raw/rgb"
check_create_dir(rgb_dir)
files_from <- list_rgb_files
files_to <- paste(rgb_dir
                  ,sapply(stringr::str_split(list_rgb_files
                                             ,.Platform$file.sep)
                          ,tail, 1)
                  ,sep = .Platform$file.sep)
# copy the downloaded files into the simplified directory
file.copy(from = files_from
          ,to = files_to
          ,overwrite = TRUE)
# If the copy was successful, delete original files in the nested directories 
if(all(file.exists(files_to))){
  unlink(rgb_download_path
         ,recursive = TRUE)
}



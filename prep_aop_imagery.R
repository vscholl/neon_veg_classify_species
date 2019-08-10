# This script downloads NEON AOP remote sensing data 
# and prepares them to become descriptive features for species classification.


# Download NEON AOP remote sensing tiles that cover the same area as the 
# in-situ woody vegetation data. We can specify specific coordinates using
# the tree locations from the woody vegetation structure data. 

# List of Data Product IDs to download
          
data_products <- data.frame(id = c("DP3.30015.001", "DP3.30025.001", "DP3.30010.001"),
                            name = c("chm", "aspect_slope", "rgb"))

for (dpID in data_products$id){
  
  print(paste("Downloading data product ID", dpID))
        
  neonUtilities::byTileAOP(
    dpID = dpID
    ,site = "NIWO"
    ,year = "2017"
    ,savepath = "data/data_raw"
    ,easting = veg_coordinates$eastings
    ,northing = veg_coordinates$northings
    ,buffer = 5)
  
} 

# DP3.30010.001  high-resolution RGB multispectral imagery
neonUtilities::byTileAOP(
  dpID = "DP3.30010.001"
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



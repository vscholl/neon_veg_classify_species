# This script stacks up the AOP remote sensing imagery and prepares it for
# extracting descriptive features for classification. 

library(raster)



rgb_red <- raster::raster(list_rgb_files[1], band = 1) 

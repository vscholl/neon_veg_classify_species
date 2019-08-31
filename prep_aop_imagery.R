# This script stacks up the AOP remote sensing imagery and prepares it for
# extracting descriptive features for classification. 


# specify the paths for each data directory
h5_dir <- file.path(dir_out, "hyperspectral")
chm_dir <- file.path(dir_out, "chm")
# slope geotiffs
slope_dir <- paste0('../data/', site_code, '/slope/')   
# aspect geotiffs
aspect_dir <- paste0('../data/', site_code, '/aspect/') 
# rgb image geotiffs
rgb_dir <- paste0('../data/', site_code, '/rgb/')
# vegetation index .tifs 
vegIndices_dir <- paste0('../data/', site_code, '/vegIndices/') 
# output directory for stacked AOP data
stacked_aop_data_dir <- paste0('../data/',site_code, '/stacked_aop_data/')


rgb_red <- raster::raster(list_rgb_files[1], band = 1) 

# This script stacks up the AOP remote sensing imagery and prepares it for
# extracting descriptive features for classification. 


# Specify the paths for each data directory
h5_dir <- file.path(dir_out, "hyperspectral")
chm_dir <- file.path(dir_out, "chm")
slope_dir <- file.path(dir_out, "slope")
aspect_dir <-file.path(dir_out, "aspect")
rgb_dir <- file.path(dir_out, "rgb")
vegIndices_dir <- file.path(dir_out, "veg_indices")

# output directory for stacked AOP data
stacked_aop_data_dir <- file.path(dir_out, "stacked_aop_data")
check_create_dir(stacked_aop_data_dir)


# list the files in each data directory; filter results based on file type
# hyperspectral data - list the .h5 files 
h5_list <- list.files(path = h5_dir, full.names = TRUE) 
chm_list <- list.files(path = chm_dir, full.names = TRUE)
slope_list <- list.files(path = slope_dir, full.names = TRUE)
aspect_list <- list.files(path = aspect_dir, full.names = TRUE)
rgb_list <- list.files(path = rgb_dir, full.names = TRUE)
veg_indices_list <- list.dirs(path = vegIndices_dir, full.names = TRUE)
veg_indices_names <- c("ARVI","EVI","NDLI","NDNI","NDVI","PRI","SAVI")




rgb_red <- raster::raster(list_rgb_files[1], band = 1) 


# read wavelengths from text file 
# created in supporting_functions/stack_hyperspectral
wavelengths = as.numeric(unlist(read.table(paste0(out_dir,"wavelengths.txt"),
                                           sep="\n",
                                           skip = 1,
                                           col.names = 'wavelength')))

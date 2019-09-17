check_create_dir <- function(new_dir){
  # check if directory exists. If it doesn't, create it. 
  if (!dir.exists(new_dir)){
    dir.create(new_dir)
  }
}


list_tiles_with_veg <- function(veg_df, out_dir){
  # generate a list of 1km x 1km tiles containing field data
  # write list to a text file in the output directory provided
  
  print("Generating list of tiles containing veg stems...")
  
  # get easting, northing coordinates of all plants
  e <- NA
  n <- NA
  for (i in 1:dim(veg_df)[1]){
    easting <- floor(as.numeric(veg_df$easting[i])/1000)*1000
    northing <- floor(as.numeric(veg_df$northing[i])/1000)*1000
    e[i] <- easting
    n[i] <- northing
  }
  
  # find unique rows repesenting tiles
  easting_northings <- data.frame(as.character(e),as.character(n))
  colnames(easting_northings) <- c('e','n')
  tiles <- unique(easting_northings[,c('e','n')])
  
  # order by ascending tile coordinates 
  tiles <- tiles %>%
    arrange(e)
  
  # write to text file 
  tile_names <- paste(tiles$e, tiles$n, sep="_")
  tiles_file <- file(paste(out_dir,"list_tiles.txt", sep=""))
  writeLines(tile_names, tiles_file)
  close(tiles_file)
  
  return(tiles)
}


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
    #duplicates <- print(id_lut[id_lut$last_digits == id,])
    duplicates <- id_lut[id_lut$last_digits == id,]
    
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


get_poly = function(spdf, index_type, number){
  # this fuction extracts a single polygon inside a SpatialPolygonsDataFrame 
  # object based on its individual ID OR index in the data frame for testing
  # index_type should be either "id" or "index"
  # if index_type == id, then search for matching entry with that individualID
  # if index_type == index, then it use number as an index into data frame
  
  if(index_type=='id'){ # use number as individual ID 
    i <- which(spdf$individualID == number)
  } else { # use number as index
    i <- number
  }
  
  coords = spdf@polygons[[i]]@Polygons[[1]]@coords
  extra_data = as.data.frame(spdf@data[spdf@data$individualID == spdf$individualID[i],], 
                             row.names = as.character(spdf$individualID[i]))
  
  # create SpatialPolygons
  P1 = sp::Polygon(coords)
  Ps1 = sp::SpatialPolygons(list(Polygons(list(P1), ID = spdf$individualID[i])), 
                        proj4string=spdf@proj4string)
  
  # create SpatialPolygonsDataFrame
  Ps1 = sp::SpatialPolygonsDataFrame(Ps1, 
                                 data = extra_data, match.ID = TRUE)
  return(Ps1)
  
}


clip_overlap <- function(df, thresh){
  # Each polygon will be compared with the rest to determine which ones are 
  # overlapping. For each overlapping pair, the taller polygon clips the 
  # shorter one. If the clipped polygon is smaller than the specified area 
  # threshold then it is deleted from subsequent analysis. 
  #
  # Args
  #   df
  #     (Simple Features object) containing polygons to compare / clip. 
  #
  #   thresh
  #     (integer) Area [m^2] threshold to remove small polygons. 
  #

  
  message("\nChecking for overlap & clipping shorter polygons...")
  
  # Convert SF to Spatial object since this function was written using 
  # SpatialPolygons and at some point it would be great to rewrite it 
  # using SF functions.....
  df <- sf::as_Spatial(df)
  
  # how should be polygons be reordered? 
  # for now, reorder then based on height.
  polys_ordered <- df[order(df$height, 
                            decreasing = TRUE),]
  
  # create a copy of the polygons to update with clipped/deleted entries
  polys_filtered <- polys_ordered
  
  # create an empty list of polygon pairs that have been compared 
  compared_pairs <- list();
  c <- 1 # index for appending to list
  
  for (individualID in as.character(polys_ordered@data$individualID)){
    
    #print(individualID)
    
    # if this polygon was removed from the polys_filtered
    # data frame in a previous iteration, skip it 
    if(sum(polys_filtered$individualID==individualID) == 0){
      next
    }
    
    # extract current vs. all other polygon from data frame
    current_poly <- get_poly(polys_filtered, index_type = 'id', number = individualID)
    other_polys <- polys_filtered[polys_filtered$individualID!=current_poly$individualID,]
    
    # check for overlap between current polygon and all polygons
    overlap <- raster::intersect(current_poly, other_polys)
    n_overlap <- length(overlap)
    
    if(n_overlap>0){ 
      for (o in 1:n_overlap){
        
        #print("o")
        #print(o)
        
        # if current polygon ID is not in filtered set
        if(sum(polys_filtered$individualID==individualID) == 0){
          break
        }
        
        # get height of current and test polygons that overlap
        current_poly.height <- current_poly@data$height
        
        test_poly <- get_poly(polys_filtered, 
                              index_type = 'id', 
                              number = overlap@data$individualID.2[[o]])
        test_poly.height <- test_poly@data$height
        
        # combine the ID's of the current and test polygons
        # to keep track of which pairs have been compared 
        id.pair <- paste(current_poly@data$individualID,
                         test_poly@data$individualID,
                         sep = " ")
        
        # if polygon pair was already compared, skip to the next pair
        if (id.pair %in% compared_pairs) {
          
          next
          
        } else { 
          
          # add to the list of polygon pairs that have been compared 
          compared_pairs[[c]] <- id.pair
          
          c <- c + 1 # increment index 
          
          # add opposite combination of polygons
          compared_pairs[[c]] <- paste(test_poly@data$individualID,
                                       current_poly@data$individualID,
                                       sep = " ")
          # increment index 
          c <- c + 1
          
        }
        
        # if test polygon is not in filtered set, skip to the next overlapping polygon
        # (because it has already been removed)
        if(sum(polys_filtered$individualID==test_poly$individualID) == 0){
          next
        }
        
        # if the area of one of the polygons is equivalent to zero, delete it and 
        # skip to the next overlapping polygon. 
        if(current_poly@polygons[[1]]@area < 0.01){
          
          # delete the current polygon from the polygons_filtered data frame. 
          polys_filtered <- polys_filtered[polys_filtered$individualID!=current_poly$individualID,]
          
          next
          
        } else if(test_poly@polygons[[1]]@area < 0.01){
          # delete the test polygon from the polygons_filtered data frame if its
          # area is essentially 0 (using the criterion < 0.5 here). This step 
          # was added for instances where after clipping, a small edge fragment remains.
          polys_filtered <- polys_filtered[polys_filtered$individualID!=test_poly$individualID,]
          
          next
          
        }
        
        # compare the heights of the polygons
        if(current_poly.height > test_poly.height){
          
          # if current polygon is taller, clip the test polygon.
          clipped <- raster::erase(test_poly,
                                   raster::crop(test_poly, current_poly))
          
          # unfortunately, gDifference does not preserve the attributes, it only
          # preserves the geometry. 
          clipped_geom <- rgeos::gDifference(test_poly, current_poly, 
                                             byid = TRUE, drop_lower_td = TRUE)
          
          # if the clipped region is NULL, skip to the next polygon comparison.
          if(is.null(clipped_geom)){
            #print("null clipped_geom")
            next
          } else{ 
          
          # set the ID field to match the test polygon individualID
          clipped_geom@polygons[[1]]@ID <- as.character(test_poly$individualID)
          
          # get the index within the filtered polygons data frame 
          # where the test polygon belongs. 
          j <- which(polys_filtered@data$individualID == test_poly$individualID)
        }
          
        } else{
          
          # otherwise, the test polygon is taller: clip the current polygon.
          clipped <- raster::erase(current_poly,
                                   raster::crop(current_poly, test_poly))
          
          clipped_geom <- rgeos::gDifference(current_poly, test_poly,
                                             byid = TRUE, drop_lower_td = TRUE)
          
          # set the ID field to match the current polygon individualID
          clipped_geom@polygons[[1]]@ID <- as.character(current_poly$individualID)
          
          # get the index within the filtered polygons data frame 
          # where the current polygon belongs. 
          j <- which(polys_filtered@data$individualID == current_poly$individualID)
          
        }
        
        # if there is no clipped area, skip to the next overlap polygon
        if(length(clipped) == 0){
          next
          
        } else{
          
          # check the area of the clipped test polygon. If it is greater than
          # or equal to the area threshold, replace it as the polygon 
          # geometry for the entry matching the test individualID in the 
          # polygons_filtered data frame. 
          if(clipped@polygons[[1]]@area  >= thresh){
            
            # replace the original polygon with the clipped polygon
            polys_filtered@polygons[[j]] <- clipped_geom@polygons[[1]]
            
            
          } else{
            # otherwise, delete the test polygon from the polygons_filtered data frame. 
            polys_filtered <- polys_filtered[polys_filtered$individualID!=clipped$individualID,]
            
          }
        }
      }
    }
  }
  
  # write final polygons to file after checking for overlap
  #writeOGR(polys_filtered, getwd(),
  #         paste(shp_filename),
  #         driver="ESRI Shapefile", overwrite_layer = TRUE)
  
  return(sf::st_as_sf(polys_filtered))
  
}


move_downloaded_files <- function(dir_out, dp_id, dp_name, file_pattern, delete_orig = FALSE, unzip = FALSE){
  # moves AOP files downloaded using the neonUtilities::byTileAOP
  # function into a folder with an intuitive name for each 
  # remote sensing data type. 
  # 
  # Args
  #   dir_out - character string path to the main directory where
  #             files were downloaded.
  #             example: "data/data_raw
  #
  #   dp_id - character string with the data product ID 
  #          example: "DP3.30015.001" for the Canopy Height Model
  #
  #   dp_name - character string with a short name describing the data 
  #            this is the folder name where the files will be moved. 
  #            this folder will be within the dir_out folder. 
  #            example: "chm" for Canopy Height Model 
  # 
  #   file_pattern - character string used to identify downloaded files 
  #                  that will be moved by this function. 
  #                  example: "*CHM.tif$" for Canopy Height Model tiles
  #                           that include CHM and end with a .tif extension
  # 
  #   delete_orig - optional logical parameter to delete the original 
  #                 downloaded files. Default value of FALSE 
  # 
  #   unzip - optional logical parameter to unxip the downloaded files
  #           after they have been moved. Deafult value of FALSE. 
  # 
  # Example function call: 
  # move_downloaded_files(dir_out = "data/data_raw/NIWO_2017"
  #                       ,dp_id = "DP3.30026.001"
  #                       ,dp_name = "veg_indices"
  #                       ,file_pattern = "*VegIndices.zip$"
  #                       ,delete_orig = TRUE
  #                       ,unzip = TRUE)
  
  
  # list all filenames 
  download_path <- file.path(dir_out, dp_id)
  list_files <- list.files(path = download_path
                           ,pattern = file_pattern
                           ,recursive = TRUE
                           ,full.names = TRUE)
  
  # move files into a new folder with an intuitive name
  move_dir <- file.path(dir_out, dp_name)
  check_create_dir(move_dir)
  files_from <- list_files
  files_to <- paste(move_dir
                    ,sapply(stringr::str_split(list_files
                                               ,.Platform$file.sep)
                            ,tail, 1)
                    ,sep = .Platform$file.sep)
  # copy the downloaded files into the simplified directory
  file.copy(from = files_from
            ,to = files_to
            ,overwrite = TRUE)
  
  # If the copy was successful, delete original files in the nested directories
  if(delete_orig){
    if(all(file.exists(files_to))){
      unlink(download_path
             ,recursive = TRUE)
    }
  }
  
  # Unzip compressed files 
  if(unzip){
    for(zipFile in files_to){
      utils::unzip(zipFile, exdir = move_dir)
    }
  }
  
  print(paste("Downloaded files moved from", download_path, "to",move_dir))
}


stack_hyperspectral <- function(h5, out_dir){
  # This function creates a rasterstack object for the specified HDF5 
  # filename. 
  #
  # Args: 
  # h5
  #   character string filename of HDF5 file 
  # out_dir
  #   directory for output files where the wavelengths will be written to
  #   a text file for further analysis
  #
  # Returns: 
  # s
  #   RasterStack (collection of Raster layers with the same spatial extent
  #   and resolution) containing nrows x ncols x nbands, based on the 
  #   resolution and number of bands in the HDF5 file. This RasterStack
  #   can then be clipped using Spatial vector layers. 
  #
  
  
  # list the contents of HDF5 file
  h5_struct <- rhdf5::h5ls(h5, all=T)
  
  # construct the string using "/Reflectance/Metadata/Coordinate_System",
  # without explicitly using a site code 
  crs_tag <- h5_struct$group[grepl("/Reflectance/Metadata/Coordinate_System", 
                                   h5_struct$group)][1] 
  
  # read coordinate reference system data
  crs_info <- rhdf5::h5read(h5, crs_tag)
  
  # convert "UTM" to lowercase "utm" for proper usage later
  crs_info$Proj4 <- CRS(chartr("UTM", "utm", crs_info$Proj4))
  
  # get attributes for the Reflectance dataset.
  # construct the string using "/Reflectance/Reflectance_Data"" 
  refl_tag <- paste0(h5_struct$group[grepl("/Reflectance", 
                                           h5_struct$group)][1],
                     "/Reflectance_Data")
  
  # read the reflectance metadata
  refl_info <- rhdf5::h5readAttributes(h5,refl_tag)
  
  # get the dimensions of the reflectance data
  n_rows <- refl_info$Dimensions[1]
  n_cols <- refl_info$Dimensions[2]
  n_bands <- refl_info$Dimensions[3]
  
  # print dimensions 
  print(paste0("# Rows: ", as.character(n_rows)))
  print(paste0("# Columns: ", as.character(n_cols)))
  print(paste0("# Bands: ", as.character(n_bands)))
  
  # read the wavelengths of the hyperspectral image bands
  wavelength_tag <- paste0(h5_struct$group[grepl("/Reflectance/Metadata/Spectral_Data", 
                                                 h5_struct$group)][1],
                           "/Wavelength")
  wavelengths <- rhdf5::h5read(h5,
                               wavelength_tag)
  
  # define spatial extent: extract resolution and origin coordinates
  map_info <- unlist(strsplit(crs_info$Map_Info, 
                              split = ", "))
  res_x <- as.numeric(map_info[6])
  res_y <- as.numeric(map_info[7])
  x_min <- as.numeric(map_info[4])
  y_max <- as.numeric(map_info[5])
  
  # calculate the maximum X and minimum Y values 
  x_max <- (x_min + (n_cols * res_x))
  y_min <- (y_max - (n_rows * res_y))
  tile_extent <- raster::extent(x_min, x_max, y_min, y_max)
  print("tile extent")
  print(tile_extent)
  
  # read reflectance data for all bands
  refl <- rhdf5::h5read(h5, refl_tag,
                        index = list(1:n_bands, 1:n_cols, 1:n_rows))
  
  # view and apply scale factor to convert integer values to reflectance [0,1]
  # and data ignore value
  scale_factor <- refl_info$Scale_Factor
  data_ignore <- refl_info$Data_Ignore_Value
  refl[refl == data_ignore] <- NA 
  refl_scaled <- refl / scale_factor
  
  # create georeferenced raster using band 1 
  r1 <- (refl_scaled[1,,]) # convert first band to matrix
  # transpose the image pixels for proper orientation to match
  # the other layers. create a raster for this band and assign
  # the CRS.
  print("Transposing reflectance data for proper orientation")
  r1 <- raster::t(raster::raster(r1, crs = crs_info$Proj4))
  extent(r1) <- tile_extent
  
  # start the raster stack with first band 
  s <- raster::stack(r1)
  
  # loop through bands and create a giant rasterstack with 426 (n_bands) bands
  for(b in 2:n_bands){
    print(b)
    
    # create raster with current band
    r <- (refl_scaled[b,,]) # convert to matrix
    r <- raster::t(raster::raster(r, crs = crs_info$Proj4))
    extent(r) <- tile_extent
    
    # add additional band to the stack with the addLayer function
    s <- raster::addLayer(s, r)
    
  }
  
  # adjust the names for each layer in raster stack to correspond to wavelength
  names(s) <- round(wavelengths)
  
  # write wavelengths to a text file 
  # write the exact wavelengths to file for future use 
  write.table(data.frame(wavelengths = wavelengths),
              file.path(out_dir,"wavelengths.txt"),
              sep="\n",
              row.names=FALSE)
  
  # return the stacked hyperspectral data to clip with vector files 
  return(s)
  
}

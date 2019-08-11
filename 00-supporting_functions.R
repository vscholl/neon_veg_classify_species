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
  P1 = Polygon(coords)
  Ps1 = SpatialPolygons(list(Polygons(list(P1), ID = spdf$individualID[i])), 
                        proj4string=spdf@proj4string)
  
  # create SpatialPolygonsDataFrame
  Ps1 = SpatialPolygonsDataFrame(Ps1, 
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
    
    print(individualID)
    
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
        
        print("o")
        print(o)
        
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
          
          # set the ID field to match the test polygon individualID
          clipped_geom@polygons[[1]]@ID <- as.character(test_poly$individualID)
          
          # get the index within the filtered polygons data frame 
          # where the test polygon belongs. 
          j <- which(polys_filtered@data$individualID == test_poly$individualID)
          
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


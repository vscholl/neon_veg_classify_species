# This script downloads NEON AOP remote sensing data 
# and prepares them to become descriptive features for species classification.


# Download NEON AOP remote sensing tiles that cover the same area as the 
# in-situ woody vegetation data. We can specify specific coordinates using
# the tree locations from the woody vegetation structure data. 

# List of Data Product IDs to download
          
data_products <- data.frame(id = c("DP3.30015.001", "DP3.30010.001"),
                            name = c("CHM", "RGB"))

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


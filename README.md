# Classify vegetation species using NEON data
My master's workflow to perform Random Forest classification using freely available data from the National Ecological Observatory Network (NEON). 

## Getting started 

Click the "neon_veg_classify_species.Rproj" file to open this R project in RStudio. 

### (1) create_tree_features 
This script downloads NEON woody vegetation data from the API and loads it directly into R. A point and circular polygon feature is created based on each tree location and maximum crown diameter measurement. These features are saved as shapefiles within the "data/data_output" folder. 

### (2) process_tree_features


### (3) download_aop_imagery
# Classify vegetation species using NEON data
My master's workflow to perform Random Forest classification using freely available data from the National Ecological Observatory Network (NEON). 

## Getting started 

Click the "neon_veg_classify_species.Rproj" file to open this R project in RStudio. 

### (1) create_tree_features 
This script downloads NEON woody vegetation data from the API and loads it directly into R. A point and circular polygon feature is created based on each tree location and maximum crown diameter measurement. These features are saved as shapefiles within the "data/data_output" folder. 

### (2) process_tree_features
In this script, processing steps are applied to the tree features generated using in-situ NEON woody vegetation structure data: 

1. Identical multi-bole entries are identified and excluded from subsequent analysis to remove duplicated points or polygons present in the raw data set. 

2. An area threshold is then applied to remove any small trees area values less than the area of four hyperspectral pixels. This threshold was selected with the coarser resolution of the hyperspectral and LiDAR data products in mind. By preserving the trees with larger crowns, it is believed that purer spectra will be extracted for them for the training data sets, as opposed to extracting mixed pixels which signal from smaller plants and background materials or neighboring vegetation. 

3. “Engulfed” polygons, those which are shorter and completely within the boundaries of other polygons, are also present in the initial crown polygons.Since they likely cannot be observed from the airborne perspective, “engulfed” polygons were removed from subsequent analysis. Remaining polygons were checked for overlap with neighboring polygons. For each overlapping pair of polygons, shorter polygons were clipped by taller ones. If the remaining clipped area was smaller than the aforementioned area threshold, it was deleted. 

At this point, this workflow has generated a collection of polygons that will theoretically intersect with independent pixels in the airborne remote sensing data.

### (3) download_aop_imagery
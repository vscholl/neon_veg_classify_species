# Classify vegetation species using NEON data
My master's workflow to perform Random Forest classification using freely available data from the National Ecological Observatory Network (NEON). 

## Getting started 

### Packages to install

- devtools
- neonUtilities
- geoNEON 
- dplyr

### Workflow

In RStudio, click the "neon_veg_classify_species.Rproj" file to open this R project.
Run through the following scripts:

### 1. Main
Install packages and define parameters for the other scripts. 

### 2. Create tree features
This script downloads NEON woody vegetation data from the API and loads it directly into R. A point and circular polygon feature is created based on each tree location and maximum crown diameter measurement. These features are saved as shapefiles within the "data/data_output" folder. 

### 3. Process tree features
In this script, processing steps are applied to the tree features generated using in-situ NEON woody vegetation structure data: 

1. Identical multi-bole entries are identified and excluded from subsequent analysis to remove duplicated points or polygons present in the raw data set. 

2. An area threshold is then applied to remove any small trees area values less than the area of four hyperspectral pixels. This threshold was selected with the coarser resolution of the hyperspectral and LiDAR data products in mind. By preserving the trees with larger crowns, it is believed that purer spectra will be extracted for them for the training data sets, as opposed to extracting mixed pixels which signal from smaller plants and background materials or neighboring vegetation. 

3. “Engulfed” polygons, those which are shorter and completely within the boundaries of other polygons, are also present in the initial crown polygons.Since they likely cannot be observed from the airborne perspective, “engulfed” polygons were removed from subsequent analysis. Remaining polygons were checked for overlap with neighboring polygons. For each overlapping pair of polygons, shorter polygons were clipped by taller ones. If the remaining clipped area was smaller than the aforementioned area threshold, it was deleted. 

At this point, this workflow has generated a collection of polygons that will theoretically intersect with independent pixels in the airborne remote sensing data.

### 4. Download AOP imagery

This script downloads selected NEON Airborne Observation Platform (AOP) remote sensing mosaic data product tiles for the site and year of interest (saved to the following directory in the project: "data/data_raw/SITE_YEAR"). Each one is downloaded to a deeply nested subdirectory structure in a top folder named with the data product ID (i.e. "DP3.30006.001"). Each set of files are moved into a folder with a short intuitive pathname (i.e. "data/data_raw/SITE_YEAR/hyperspectral")). 

| Data Product Name                                    | Data Product ID |
| :---                                                   | ---             |
| Ecosystem Structure (Canopy Height Model)             | DP3.30015.001   |
| Slope and Aspect - LiDAR                              | DP3.30025.001   |
| High-resolution orthorectified camera imagery  | DP3.30010.001   |
| Vegetation Indices - spectrometer - mosaic            | DP3.30026.001   |
| Spectrometer orthorectified surface directional reflectance | DP3.30006.001| 

### 5. Prep AOP imagery






#### VS-NOTE: 
To do:
- Check that the area threshold is properly applied in the clip_overlap step, since geometries are converted from SF to Spatial
- Remove invalid geometries after clip_polygon based on the sf::is_valid logical result 
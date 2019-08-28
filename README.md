# Classify vegetation species using NEON data
My master's workflow to perform Random Forest classification using freely available data from the National Ecological Observatory Network (NEON). 

## Getting started 

### (1) create_tree_features 
This script downloads NEON woody vegetation data from the API and loads it directly into R. A circular polygon features is created based on each tree location and maximum crown diameter measurement. 

### (2) 
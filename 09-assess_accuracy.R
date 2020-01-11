
# create an empty matrix to summarise the model Accuracies
rfAccuracies <- data.frame(matrix(ncol = 4, nrow = length(dirs_to_assess)))
colnames(rfAccuracies) <- c("description", "OA_OOB", "OA_IndVal","K")

i <- 1
for(shapefile_filename in dirs_to_assess){
  print(shapefile_filename)
  
  # get a description of the shapefile to use for naming outputs
  shapefile_description <- tools::file_path_sans_ext(basename(shapefile_filename))
  
  # build up the name of the directory containing RF outputs to assess
  out_dir <- file.path(dir_data_out,paste0("rf_",shapefile_description))
  
  rf_model_filename = file.path(out_dir,
                   paste0("rf_model_",shapefile_description,".RData"))
  
  load(rf_model_filename)
  
  print(rf_model)
  
  # calculate accuracy metrics 
  # y = predicted data; (horizontal axis)
  # x = observed data (true class labels) (vertical axis)
  accuracy <- rfUtilities::accuracy(x = rf_model$y,
                                    y = rf_model$predicted)
  
  # record each accuracy metric in the table for a final comparison.
  # round each value to the nearest decimal place 
  rfAccuracies$OA_OOB[i] <- round(accuracy$PCC, 1) # Overall Accuracy
  rfAccuracies$K[i] <- round(accuracy$kappa, 3) #Cohen's Kappa 
  rfAccuracies$description[i] <- shapefile_description
  
  i <- i + 1 
  
}

library(kableExtra)
rfAccuracies %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover","condensed"))

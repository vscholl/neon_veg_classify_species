
# create an empty matrix to summarise the model Accuracies
rfAccuracies <- data.frame(matrix(ncol = 4, nrow = length(dirs_to_assess)))
colnames(rfAccuracies) <- c("Training Set", "OA_OOB", "OA_IndVal","Kappa")

i <- 1
for(shapefile_filename in dirs_to_assess){
  print(shapefile_filename)
  
  # get a description of the shapefile to use for naming outputs
  shapefile_description <- tools::file_path_sans_ext(basename(shapefile_filename))
  print(shapefile_description)
  
  # build up the name of the directory containing RF outputs to assess
  out_dir <- file.path(dir_data_out,paste0("rf_",shapefile_description))
  
  rf_model_filename = file.path(out_dir,
                   paste0("rf_model_",shapefile_description,".RData"))
  
  # load the current RF model
  load(rf_model_filename)
  print(rf_model)
  
  # calculate accuracy metrics 
  # y = predicted data; (horizontal axis)
  # x = observed data (true class labels) (vertical axis)
  accuracy <- rfUtilities::accuracy(x = rf_model$y,
                                    y = rf_model$predicted)
  
  # record each accuracy metric in the table for a final comparison.
  # round each value to the nearest decimal place 
  rfAccuracies$OA_OOB[i] <- (accuracy$PCC / 100) # Overall Accuracy
  rfAccuracies$K[i] <- round(accuracy$kappa, 3) #Cohen's Kappa 
  rfAccuracies$description[i] <- shapefile_description
  
  # predict species for the independent validation set -------------
  predValidation <- predict(rf_model, validationSet, type = "class")
  confusionTable <- table(predValidation, validationSet$taxonID)
  print(confusionTable)
  val_OA <- sum(predValidation == validationSet$taxonID) / 
    length(validationSet$taxonID)
  
  print(paste0("overall accuracy predicting validation set: ",
               as.character(val_OA)))
  # write the Overall Accuracy for the independent validation set to the
  # Accuracies data frame
  rfAccuracies$OA_IndVal[i] <- round(val_OA, digits = 2) # Overall Accuracy
  
  
  
  i <- i + 1 
  
}

#library(kableExtra)
rfAccuracies %>%
  kableExtra::kable(digits = 2) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover","condensed"))

#library(xtable)
xtable::xtable(x = rfAccuracies, digits = 2)

# Calculating precision and recall
library(caret) 
model_stats <- caret::confusionMatrix(data = rf_model$predicted, reference = rf_model$y, mode = "prec_recall")



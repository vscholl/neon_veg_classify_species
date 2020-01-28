# This script gathers the accuracies from a series os of classification models
# and presents them in a table:
# Out-of-Bag accuracy, independent validation set accuracy, and Cohen's kappa.
#
# This script also creates a confusion matrix to show the classification 
# accuracy for each species, in addition to user's and producer's accuracy. 


# create an empty matrix to summarise the model Accuracies
rfAccuracies <- data.frame(matrix(ncol = 4, nrow = length(dirs_to_assess)))
colnames(rfAccuracies) <- c("description", "OA_OOB", "OA_IndVal","Kappa")


i <- 1
highest_accuracy <- 0
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
  rfAccuracies$Kappa[i] <- round(accuracy$kappa, 3) #Cohen's Kappa 
  rfAccuracies$description[i] <- shapefile_description
  
  
  # OOB CONFUSION MATRIX WITH USERS AND PRODUCER'S ACCURACIES
  # convert confusion table to a data frame 
  confusion_oob <- as.data.frame.matrix(accuracy$confusion)
  # add a row to the confusion matrix with User's accuracy 
  confusion_oob <- rbind(confusion_oob,
                        UA = accuracy$users.accuracy)
  # add a column with Producer's accuracy
  confusion_oob$PA <- c(accuracy$producers.accuracy, "")
  # VS-NOTE: round the count values for each species to be integers ??
  print("CONFUSION MATRIX OOB:")
  print(confusion_oob)

  
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
  rfAccuracies$OA_IndVal[i] <- val_OA  # Overall Accuracy
  
  # keep track of which training data set yielded the greatest accuracy
  # and keep the confusion matrix for a figure
  if(accuracy$PCC > highest_accuracy){
    
    # set a new highest accuracy for the sets being compared
    highest_accuracy <- accuracy$PCC
    highest_accuracy_set <- shapefile_filename
  
    #library(caret)
    model_stats <- caret::confusionMatrix(data = rf_model$predicted, 
                                          reference = rf_model$y, 
                                          mode = "prec_recall")
    
    # confusion matrix for validation set?
    #confusion_iv <- as.data.frame.matrix(model_stats$table)
    
    # keep track of the OOB confusion matrix for the model w highest accuracy 
    confusion_matrix_OOB <- confusion_oob
    
    
    
  }

  i <- i + 1 
  
}

# update the column names and training set names for the manuscript table 
colnames(rfAccuracies) <- c("Training Set", "OOB Accuracy", "IV Accuracy","Kappa")
rfAccuracies$`Training Set` <- c("Points", 
                                 "Polygons - half diameter", 
                                 "Polygons - max diameter",
                                 "Points - half diam clipped", 
                                 "Polygons - half diam clipped", 
                                 "Polygons - max diam clipped")

#library(kableExtra)
rfAccuracies %>%
  kableExtra::kable(digits = 3) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover","condensed"))

# print a LaTeX-syntax table to the console presenting the accuracies 
#library(xtable)
print("Overall accuracies table, in LaTeX table format: , ")
print(xtable::xtable(x = rfAccuracies, digits = 3))

# pring a LaTeX-syntax table to the console with the confusion matrix
# and users/producers accuracies for each species, for the model with the highest 
# overall OOB accuracy 
print("Confusion matrix with precision and recall for most accurate set, in LaTeX format: ")
print(highest_accuracy_set)

print(xtable::xtable(x = confusion_matrix_OOB, digits = 1))




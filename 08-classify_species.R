# This script trains a Random Forest (RF) model to classify tree species
# using the in-situ tree measurements for species labels 
# and remote sensing data as descriptive features. 

# user-defined parameters -----------------------------------------------------

# description to name a folder for the classification outputs 
# extracted_features_filename --> the csv file containing extracted features
out_description <- paste0("rf_",shapefile_description) 
check_create_dir(file.path(dir_data_out,out_description))

# taxonIDs (species) to predict
taxon_list <- c("ABLAL","PICOL","PIEN","PIFL2")



# define the "bad bands" wavelength ranges in nanometers, where atmospheric 
# absorption creates unreliable reflectance values. 
bad_band_window_1 <- c(1340, 1445)
bad_band_window_2 <- c(1790, 1955)

# RF tuning parameter, number of trees to grow. default value 500
ntree <- 5000

# To reduce bias, this boolean variable indicates whether the same number 
# of samples are randomly selected per species class. Otherwise,
# all samples per class are used for training the classifier. 
randomMinSamples <- FALSE

# To remove the sample size bias, if TRUE this filters down each of the raw NEON 
# shapefile data sets to only contain the individualIDs present in the neon_veg set 
neonvegIDsForBothShapefiles <- FALSE

# boolean variable. if TRUE, keep separate set for validation
independentValidationSet <- TRUE 

# randomly select this amount of data for training, use the rest for validation
percentTrain <- 0.8 

pcaInsteadOfWavelengths <- FALSE
nPCs <- 2 # number of PCAs to keep 

# keep most important variables and run RF again with reduced feature set 
#keepMostImpVar <- FALSE

# create boxplots and write to file 
createBoxplots <- TRUE

# features to use in the RF models.
# this list is used to filter the columns of the data frame,
# to remove the ones containing other metadata per tree from the model. 
featureNames <- c("taxonID", 
                  wavelength_lut$xwavelength,
                  "chm", 
                  "slope", 
                  "aspect",
                  "ARVI",
                  "EVI",
                  "NDLI",
                  "NDNI",
                  "NDVI",
                  "PRI",
                  "SAVI",
                  #"rgb_meanR",
                  #"rgb_meanG",
                  #"rgb_meanB",
                  "rgb_mean_sd_R",
                  "rgb_mean_sd_G",
                  "rgb_mean_sd_B",
                  "pixelNumber",
                  "eastingIDs",
                  "northingIDs")




# open a text file to record the output results -------------------------------
rf_output_file <- file(file.path(dir_data_out,out_description,
                              "rf_model_summaries.txt"), "w")

# start the timer
start_time <- Sys.time()

# read wavelengths if not previously created


if(independentValidationSet){
  # randomly select <percentTrain> of data 
  # from the neon_veg half diameter polygons for training,
  # use the remaining samples for validation.
  # keep track of which pixelNumber, easting, northing
  # that these pixels correspond to. 
  
  # read the file with spectra to sample randomly for validation trees 
  validationSourceFilename <- extracted_spectra_filename
  
  # remove any spectra with a height of 0
  # and remove any factors
  df_val <- read.csv(validationSourceFilename) %>% 
    dplyr::filter(chm>0) %>% 
    base::droplevels() 
  
  # combine the pixelNumber, easting, and northing into a string
  # for each row with underscore separation
  dfIDs <- paste(df_val[,"pixelNumber"],
                 df_val[,"eastingIDs"],
                 df_val[,"northingIDs"], 
                 sep = "_") 
  
  # add this as a new column to the data frame 
  df_val <- df_val %>% mutate(dfIDs = dfIDs)
  
  # remove any duplicated spectra from consideration
  df_val <- df_val[!duplicated(df_val$dfIDs),]
  
  # randomly sample rows from this data set 
  set.seed(104)
  # the number of sampled rows is calculated based on 
  # percentTrain and the number of rows in the validation set. 
  # percentTrain may have a value like 0.80 (80% data used to train)
  train <- sample(nrow(df_val), 
                  percentTrain*nrow(df_val), 
                  replace = FALSE)
  # identify the other 0.20 (20%) of samples for independent validation
  validationSet <- df_val[-train,]
  
  # combine the pixelNumber, easting, and northing into a string
  # for each row with underscore separation
  valIDs <- validationSet$dfIDs
  
  # in the loop below, when each set of training features is 
  # prepared for the classifier, any spectra with
  # pixelNumer_eastingIDs_northingIDs that belong in the validation set
  # will be removed. 
  
} else{
  # otherwise, just remove the pixelNumber, eastingIDs and northingIDs
  # since they should not be input to the classifier. 
  features <- features %>% 
    dplyr::select(-c(pixelNumber, eastingIDs, northingIDs))
}


  print("Currently training the random forest with features extracted using:")
  print(extracted_features_filename)
  
  # read the values extracted from the data cube
  df_orig <- read.csv(extracted_features_filename)
  
  # filter down to the species of interest 
  df_orig <- df_orig %>% 
    dplyr::filter(taxonID %in% taxon_list)
  
  
  
  
  # Remove any spectra that have a height == 0
  print(paste0(as.character(sum(df_orig$chm==0)), 
               " pixels have a height of 0 in the CHM"))
  print("Removing these rows from the training set ... ")
  
  # also reset the factor levels (in case there are dropped taxonID levels)
  df <- df_orig %>% filter(chm>0) %>% droplevels()
  
  # remove the bad bands from the list of wavelengths 
  remove_bands <- wavelengths[(wavelengths > bad_band_window_1[1] & 
                                 wavelengths < bad_band_window_1[2]) | 
                                (wavelengths > bad_band_window_2[1] & 
                                   wavelengths < bad_band_window_2[2])]
  
  # create a LUT that matches actual wavelength values with the column names,
  # X followed by the rounded wavelength values. Remove the rows that are 
  # within thebad band ranges. 
  wavelength_lut <- data.frame(wavelength = wavelengths,
                               xwavelength = paste0("X", 
                                                    as.character(round(wavelengths))),
                               stringsAsFactors = FALSE) %>% 
    filter(!wavelength %in% remove_bands) 
  
  # filter the data to contain only the features of interest 
  features <- df %>% 
    dplyr::select(featureNames)
  
  # testing whether PCA yields better accuracy than individual wavelength reflectance data
  if(pcaInsteadOfWavelengths == TRUE){
    
    # remove the individual spectral reflectance bands from the training data
    features <- features %>% dplyr::select(-c(wavelength_lut$xwavelength))
    
    print(colnames(features))
    
    # PCA: calculate Principal Components 
    hs <- df %>% dplyr::select(c(wavelength_lut$xwavelength)) %>% as.matrix()
    hs_pca <- stats::prcomp(hs, center = TRUE, scale. = TRUE)
    summary(hs_pca)
    features <- cbind(features, hs_pca$x[,1:nPCs]) # add first n PCs to features data frame
    # visualize where each sample falls on a plot with PC2 vs PC1 
    ggbiplot::ggbiplot(hs_pca, 
                       choices = 1:2, # which PCs to plot
                       obs.scale = 1, var.scale = 1, # scale observations & variables 
                       var.axes=FALSE, # remove arrows 
                       groups = df$taxonID, # color the points by species
                       ellipse = TRUE, # draw ellipse around each group
                       circle = TRUE # draw circle around center of data set
    )   + 
      ggtitle("PCA biplot, PC1 and PC2") + 
      scale_color_brewer(palette="Spectral") + 
      theme_bw()
    # save to file 
    ggplot2::ggsave(paste0(out_dir, outDescription, 
                           "pcaPlot_",shapefileLayerNames$description[i],
                           ".png"))
    
  }
  
  print("Features used in current RF model: ")
  print(colnames(features))
  
  
  # count the number of samples per species 
  featureSummary <- features %>%
    dplyr::group_by(as.character(taxonID)) %>%
    dplyr::summarize(total = n()) 
  
  print("number of samples per species class")
  print(featureSummary)
  
  # randomly select <percentTrain> of data for training,
  # use the remaining samples for validation
  if(independentValidationSet){
    
      # filter the data to contain only the features of interest 
      validationSet <- validationSet %>% 
        dplyr::select(featureNames)
      
      if(pcaInsteadOfWavelengths){
        # perform PCA
        # remove the individual band reflectances
        wl_removed <- validationSet %>% as.data.frame() %>% 
          dplyr::select(-c(wavelength_lut$xwavelength))
        
        # isolate the individual band reflectances
        val_hs <- validationSet %>% 
          dplyr::select(c(wavelength_lut$xwavelength)) %>% as.matrix()
        
        val_hs_pca <- stats::prcomp(val_hs, center = TRUE, scale. = TRUE)
        summary(val_hs_pca)
        validationSet <- cbind(wl_removed, val_hs_pca$x[,1:nPCs])
      }
      
    
    
    # concatenate pixel #, easting, and northing 
    # for an individual entry per pixel in the input imagery,
    # i.e. "264231_452000_4431000"
    features$dfIDs <- paste(features[,"pixelNumber"],
                            features[,"eastingIDs"],
                            features[,"northingIDs"], 
                            sep = "_") 
    
    print("The following pixelNumber_easting_northing values are ")
    print("found in the current input set and the validation set: ")
    print(intersect(features$dfIDs, valIDs))
    
    print("Originally this many rows in the features DF: ")
    print(nrow(features))
    
    # remove any spectra that are being kept separate for the 
    # independent validation set 
    features <- features[!(features$dfIDs %in% valIDs),]
    
    print("After removing pixels that are in the validation set, nrow = ")
    print(nrow(features))
    
    # remove the pixelNumber, easting, and northing columns since they
    # are not input features to the train the classifier 
    features <- features %>% 
      dplyr::select(-c(pixelNumber, eastingIDs, northingIDs, dfIDs))
    
  }
  
  
  
  
  if(randomMinSamples){
    # reduce number of samples per species to avoid classifier bias
    
    # count the minimum number of samples for a single class
    minSamples <- min(featureSummary$total)  
    print(paste0("Randomly selecting ",
                 as.character(minSamples),
                 " samples per species class to avoid classifier bias"))
    
    # isolate the samples per species
    taxon1 <- features[features$taxonID==taxon_list[1],]
    taxon2 <- features[features$taxonID==taxon_list[2],]
    taxon3 <- features[features$taxonID==taxon_list[3],]
    taxon4 <- features[features$taxonID==taxon_list[4],]
    
    # keep random minSamples of each species; merge
    taxon1 <- taxon1[sample(nrow(taxon1), minSamples), ]
    taxon2 <- taxon2[sample(nrow(taxon2), minSamples), ]
    taxon3 <- taxon3[sample(nrow(taxon3), minSamples), ]
    taxon4 <- taxon4[sample(nrow(taxon4), minSamples), ]
    
    features <- rbind(taxon1, taxon2, taxon3, taxon4)
    
  } else{
    print("Using all samples per class")
  }
  
  
  
  # TRAIN RF CLASSIFIER using training set  ---------------------------------
  rf_startTime <- Sys.time()
  set.seed(104)
  rf_model <- randomForest::randomForest(as.factor(features$taxonID) ~ .,
                                         data=features, 
                                         importance=TRUE, 
                                         # ntree = # of trees to grow
                                         ntree=ntree) 
  print("randomForest time elapsed for model training: ")
  print(Sys.time()-rf_startTime)
  
  print(rf_model)
  
  # save RF model to file 
  save(rf_model, file = paste0(out_dir, outDescription,"rf_model_",
                               shapefileLayerNames$description[i],".RData"))
  
  # write all relevant information to the textfile: 
  # shapefile name
  write(shapefileLayerNames$description[i], rf_output_file, append=TRUE)
  write("\n", rf_output_file, append=TRUE) #newline
  
  # number of samples per class
  featureSummary <- data.frame(featureSummary)
  colnames(featureSummary) <- c("taxonID","numberOfSamples")
  capture.output(featureSummary, file = rf_output_file, append=TRUE)
  
  # y = predicted data; (horizontal axis)
  # x = observed data (true class labels) (vertical axis)
  accuracy <- rfUtilities::accuracy(x = rf_model$y,
                                    y = rf_model$predicted)
  
  # record each accuracy metric in the table for a final comparison.
  # round each value to the nearest decimal place 
  rfAccuracies$OA_OOB[i] <- round(accuracy$PCC, 1) # Overall Accuracy
  rfAccuracies$K[i] <- round(accuracy$kappa, 3) #Cohen's Kappa 
  
  # record the users and producer's accuracies for each specie s
  rfUsersProducers <- data.frame(matrix(ncol = 3, nrow = nrow(shapefileLayerNames)))
  #colnames(rfAccuracies) <- c("shapefileDescription", "OA", "K")
  #rfAccuracies$shapefileDescription <- shapefileLayerNames$description
  
  write("\nOverall Accuracy:", rf_output_file, append=TRUE) #newline
  write(accuracy$PCC, rf_output_file, append=TRUE)
  
  write("\nUser's Accuracy:", rf_output_file, append=TRUE) #newline
  capture.output(accuracy$users.accuracy, file = rf_output_file, append=TRUE)
  
  write("\nProducer's Accuracy:", rf_output_file, append=TRUE) #newline
  capture.output(accuracy$producers.accuracy, file = rf_output_file, append=TRUE)
  
  write("\nConfusion Matrix:", rf_output_file, append=TRUE) #newline
  capture.output(accuracy$confusion, file = rf_output_file, append=TRUE)
  
  write("\nCohen's Kappa:", rf_output_file, append=TRUE) #newline
  capture.output(accuracy$kappa, file = rf_output_file, append=TRUE)
  
  
  # INDEPENDENT VALIDATION  -------------------------------------------------
  
  # predict species ID for validation set 
  if(independentValidationSet){
    predValidation <- predict(rf_model, validationSet, type = "class")
    confusionTable <- table(predValidation, validationSet$taxonID)
    print(confusionTable)
    val_OA <- sum(predValidation == validationSet$taxonID) / 
      length(validationSet$taxonID)
    print(paste0("overall accuracy predicting validation set: ",
                 as.character(val_OA)))
    # write the accuracy summary data frame to file 
    write.csv(confusionTable,
              paste0(out_dir, outDescription, 
                     "rfConfusionMatrix_independentValidationSet_",
                     shapefileLayerNames$description[i],"_Accuracy_",
                     as.character(round(val_OA, 3)),".csv"))
    # write the Overall Accuracy for the independent validation set to the
    # Accuracies data frame
    rfAccuracies$OA_IndVal[i] <- round(val_OA*100, 1) # Overall Accuracy
  }
  
  
  # write all relevant information to the textfile: 
  # features used to describe each sample (pixel)
  write("\ndescriptive features used to train this model: ", rf_output_file, append=TRUE) #newline
  write(colnames(features), rf_output_file, append=TRUE)
  
  # RF model summary, OOB error rate 
  capture.output(rf_model, file = rf_output_file, append=TRUE)
  
  
  # VARIABLE IMPORTANCE  ----------------------------------------------------
  
  # What variables were important? --> Consult the variable importance plot. 
  varImportance <- data.frame(randomForest::importance(rf_model))
  varImportance$feature <- rownames(varImportance)
  varImportance <- varImportance %>% 
    select(feature, MeanDecreaseAccuracy, MeanDecreaseGini, everything())
  varImportanceMDA <- varImportance %>% dplyr::arrange(desc(MeanDecreaseAccuracy))
  varImportanceMDG <- varImportance %>% dplyr::arrange(desc(MeanDecreaseGini))
  
  # create bar plot to illustrate variable importance MDA
  ggplot(data = varImportanceMDA, aes(x = reorder(feature, MeanDecreaseAccuracy), 
                                      y = MeanDecreaseAccuracy,
                                      fill = MeanDecreaseAccuracy)) + 
    geom_bar(stat = 'identity', color = "black", size = 0.1, show.legend = FALSE) + 
    labs(x = "Variable", y = "Importance (MDA)") +
    coord_flip() + 
    theme_bw() + 
    scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(9,"BuGn")),
                         values = rescale(varImportanceMDA$MeanDecreaseAccuracy, 
                                          to = c(0, 1))) + 
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14)) + 
    ggtitle("Variable Importance: Mean Decrease in Accuracy")
  
  # save variable importance bar plot to image file 
  ggsave(filename = paste0(out_dir, outDescription,"varImpPlot_MDA_",
                           shapefileLayerNames$description[i],
                           ".png"))
  
  
  # create bar plot to illustrate variable importance MDG
  ggplot(data = varImportanceMDG, aes(x = reorder(feature, MeanDecreaseGini), 
                                      y = MeanDecreaseGini,
                                      fill = MeanDecreaseGini)) + 
    geom_bar(stat = 'identity', color = "black", size = 0.1, show.legend = FALSE) + 
    labs(x = "Variable", y = "Importance (MDG)") +
    coord_flip() + 
    theme_bw() + 
    scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(9,"OrRd")),
                         values = rescale(varImportanceMDG$MeanDecreaseGini, 
                                          to = c(0, 1))) + 
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14)) + 
    ggtitle("Variable Importance: Mean Decrease Gini")
  
  # save variable importance bar plot to image file 
  ggsave(filename = paste0(out_dir, outDescription,"varImpPlot_MDG_",
                           shapefileLayerNames$description[i],
                           ".png"))
  
  # make varImpPlot using the default dot plot in randomForest
  #randomForest::varImpPlot(rf_model,
  #                         main = shapefileLayerNames$description[i])
  
  
  # Ordered from highest MDGini to lowest
  write("\nVariable Importance, ranked by Mean Decrease Gini: \n", 
        rf_output_file, append=TRUE)
  capture.output(varImportanceMDG,
                 file = rf_output_file,
                 append=TRUE)
  
  write("\n", rf_output_file, append=TRUE)
  
  # RECORD TOP N MOST IMPORTANT VARIABLES BASED ON MEAN DECREASE GINI 
  rfVarImp[i,(2+nVar):((1+nVar)+nVar)] <- varImportanceMDG$feature[1:nVar]
  
  # variable importance, ordered from highest MDA to lowest
  write("\nVariable Importance, ranked by MDA: \n", 
        rf_output_file, append=TRUE)
  capture.output(varImportanceMDA,
                 file = rf_output_file,
                 append=TRUE)
  
  print(paste0("Top ", as.character(nVar)," most important variables ranked by MDA"))
  print(varImportanceMDA$feature[1:nVar]) 
  # RECORD TOP N MOST IMPORTANT VARIABLES BASED ON MEAN DECREASE ACCURACY 
  rfVarImp[i,2:(nVar+1)] <- varImportanceMDA$feature[1:nVar]
  
  
  # # TO DO: keep the n most important variables and run the classification again?
  # print("KEEPING TOP 5 VARIABLES AND RUNNING RF AGAIN ")
  # if (keepMostImpVar == TRUE) {
  #   mostImpVar <- rownames(varImp[1:5,])
  #   features2 <- features %>% dplyr::select(taxonID, c(mostImpVar))
  #   # run RF model again, this time with reduced features
  #   set.seed(104)
  #   rf_model2 <- randomForest::randomForest(as.factor(features2$taxonID) ~ .,
  #                                          data=features2,
  #                                          importance=TRUE,
  #                                          ntree=ntree) # ntree is number of trees to grow
  #   print(rf_model2)
  # }
  # initial test shows that this does not significantly improve the accuracy
  
  
  # INTERSPECIES VARIABLE COMPARISON BOXPLOTS -------------------------------
  
  if (createBoxplots == TRUE){
    
    # select the top n most important variables based on MDA for the current model 
    vars <- as.character(rfVarImp[i,2:(nVar+1) ])
    # subset the features data frame to obtain just the columns needed 
    test_features <- features %>% dplyr::select(c(taxonID, vars))
    # "gather" the wide data to make it longer.
    # each row represents a value. There is a separate column to indicate
    # species and also which feature (lidar, hs, rgb, etc...) for grouping in the plot
    features_gathered <- tidyr::gather(test_features, "feature", "value",-taxonID)
    # make a new column where each feature name is a factor, so the plots
    # can be arranged in order of most important to least important 
    features_gathered$feature_ordered = factor(features_gathered$feature, levels=vars)
    
    # create the multi-plot
    g <- ggplot(data = features_gathered, aes(x = taxonID, y = value)) + 
      geom_boxplot(aes(fill = taxonID),outlier.size = 0.2) + 
      facet_wrap(. ~ feature_ordered,scales='free',ncol=3) + 
      scale_fill_brewer(palette = "Spectral") + 
      theme_bw() + 
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            plot.title = element_text(size = 10)) +
      ggtitle(paste0("Interspecies boxplot comparison of MDA most important features: \n",
                     shapefileLayerNames$description[i])) 
    ggsave(g, filename = paste0(out_dir,outDescription,"boxplot_impVarMDA_",shapefileLayerNames$description[i],".png"),
           width = 6, height = 5, dpi = 300, units = "in", device='png')
    
    
    # MDG
    # select the top n most important variables based on MDA for the current model 
    vars <- as.character(rfVarImp[i,(2+nVar):(1+2*nVar)])
    # subset the features data frame to obtain just the columns needed 
    test_features <- features %>% dplyr::select(c(taxonID, vars))
    # "gather" the wide data to make it longer.
    # each row represents a value. There is a separate column to indicate
    # species and also which feature (lidar, hs, rgb, etc...) for grouping in the plot
    features_gathered <- tidyr::gather(test_features, "feature", "value",-taxonID)
    # make a new column where each feature name is a factor, so the plots
    # can be arranged in order of most important to least important 
    features_gathered$feature_ordered = factor(features_gathered$feature, levels=vars)
    
    # create the multi-plot
    g2 <- ggplot(data = features_gathered, aes(x = taxonID, y = value)) + 
      geom_boxplot(aes(fill = taxonID),outlier.size = 0.2) + 
      facet_wrap(. ~ feature_ordered,scales='free',ncol=3) + 
      scale_fill_brewer(palette = "Spectral") + 
      theme_bw() + 
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            plot.title = element_text(size = 10)) +
      ggtitle(paste0("Interspecies boxplot comparison of MDG most important features: \n",
                     shapefileLayerNames$description[i]))
    
    ggsave(g2, filename = paste0(out_dir,outDescription,"boxplot_impVarMDGini_",shapefileLayerNames$description[i],".png"),
           width = 6, height = 5, dpi = 300, units = "in", device='png')
    
  }
  
  
  write("\n\n------------------------------\n\n", rf_output_file, append=TRUE)
  
}

# close the text file
close(rf_output_file)

end_time <- Sys.time()
print("Elapsed time: ")
print(end_time-start_time)




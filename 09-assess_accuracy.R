
for(shapefile_filename in dirs_to_assess){
  print(shapefile_filename)
  
  # get a description of the shapefile to use for naming outputs
  shapefile_description <- tools::file_path_sans_ext(basename(shapefile_filename))
  
  # build up the name of the directory containing RF outputs to assess
  out_dir <- file.path(dir_data_out,paste0("rf_",shapefile_description))
  
  rf_model_filename = file.path(out_dir,
                   paste0("rf_model_",shapefile_description,".RData"))
  
  load(rf_model_filename)
  
  print(rf_model_filename)
  
}


source("code/utils/preprocessing/calculate_object_distance.R")

base_path <- "data/data_20221107"
new_base_dir <- "data/transformed_distance"

dir.create(new_base_dir)
country_path_list <- list.dirs(base_path, recursive = F)

lapply(country_path_list, function(country_path){
  message(paste0(c("country: ", country_path)))
  file_list <- list.files(path = country_path, full.names = T)
  
  sapply(file_list, function(file_path) {
    message(paste0(c("file: ", file_path)))
    
    transformed_data <- calculate_object_distance(file_path)
    new_file_path <- paste(c(new_base_dir, 
                             strsplit(file_path, "/")[[1]][3], 
                             strsplit(file_path, "/")[[1]][4]), collapse = "/")
    dir.create(paste(c(new_base_dir, 
                       strsplit(file_path, "/")[[1]][3]), collapse = "/"))
    write.csv(x = transformed_data, file = new_file_path)
  })
})

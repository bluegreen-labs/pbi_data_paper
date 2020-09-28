# This script takes care of the processing of the images
# for both PBI growing seasons, it generates the anonymized
# images and calculates image by image colour metrics

library(tidyverse)
library(pbir)

#--- Growing season 1

# only calculate the image data if the gcc_data file does not
# exist, skip otherwise
if(!file.exists("./data-raw/rabi_2016_2017/pbi_0001_gcc_data.csv")){
  status <- try(pbi_process_images(
    database = "./data-raw/rabi_2016_2017/pbi_rabi_2016_2017_database.dta",
    img_path = "/scratch/IFPRI/rabi_2016_2017/",
    season_id = "2016_11",
    output_path = "/scratch/R/",
    output_prefix = 1
  ))
  
  # Check processing routine
  if(inherits(status, "try-error")){
    stop("Image processing failed, check all input parameters")
  } else {
    
    # copy greenness data to the non-public ./data-raw/ folder
    file <- list.files(file.path("/scratch/R/",1),
                       "*.csv",
                       full.names = TRUE)
    
    destination <- file.path("./data-raw/rabi_2016_2017/",
                             basename(file))
    
    file.copy(from = file,
              to = destination, 
              overwrite = TRUE)
  }
  
  #--- Compress and copy all image data over to the project's data directory
  system(paste('tar -zcf ./data/pbi_0001_images.tar.gz -C ',
               file.path("/scratch/R/",1,"images .")), wait = TRUE)
  
}

#--- Growing season 2

# only calculate the image data if the gcc_data file does not
# exist, skip otherwise
if(!file.exists("./data-raw/rabi_2017_2018/pbi_0002_gcc_data.csv")){
  status <- try(
    pbi_process_images(
      database = "./data-raw/rabi_2017_2018/pbi_rabi_2017_2018_database.dta",
      img_path = "/scratch/IFPRI/rabi_2017_2018/",
      output_path = "/scratch/R/",
      season_id = "2017_11",
      output_prefix = 2)
  )
  
  # Check processing routine
  if(inherits(status, "try-error")){
    stop("Image processing failed, check all input parameters")
  } else {
    
    # copy greenness data to the non-public ./data-raw/ folder
    file <- list.files(file.path("/scratch/R/",2),
                       "*.csv",
                       full.names = TRUE)
    
    destination <- file.path("./data-raw/rabi_2017_2018/",
                             basename(file))
    
    file.copy(from = file,
              to = destination, 
              overwrite = TRUE)
  }
  
  #--- Compress and copy all image data over to the project's data directory
  system(paste('tar -zcf ./data/pbi_0002_images.tar.gz -C ',
               file.path("/scratch/R/",2,"images .")), wait = TRUE)
}




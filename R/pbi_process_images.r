#' Batch process all IFPRI PBI data
#'
#' Uses an RDS data file as a driver
#'
#' @param database RDS or DTA database file as provided by IFPRI
#' @param img_path local IFPRI database of images to process
#' @param output_path where to store processing results
#' @param output_prefix prefix to use on processed data
#' @param ncores numbers of cores to use in parallel processing
#' @keywords gcc calculation, QA/GC
#' @export

pbi_process_images <- function(
  database,
  img_path,
  output_path,
  output_prefix = "pbi_rabi_2016_2017",
  season_id,
  ncores = 12
  ){
  
  # check if parameters are available
  if ( missing(database) | missing(img_path) | missing(output_path) ){
    stop('Missing input parameter(s)!')
  }
  
  # check if image data directory exists
  if (!dir.exists(img_path)){
    stop('data directory does not exist')
  }
  
  # Large if statement switching between either the first and
  # second rabi season. Both were acquired using different 
  # database structures and naming conventions and hence
  # need different processing. This will be standardized
  # in the future but for now this is the only way to 
  # homogenize both data streams
  
  # read STATA exported database
  df <- suppressWarnings(readstata13::read.dta13(database, missing.type = TRUE))
  
  # convert time field
  df$time <- do.call("rbind",
                     strsplit(as.character(df$datetime)," "))[,2]
  
  if(grepl("2018", database)){
    
    # screen outliers (bad GPS fixes etc)
    df$longitude[which(df$latitude < 26)] <- NA
    df$latitude[which(df$latitude < 26)] <- NA
    
    # generate report ID
    df$reportid <- 1:nrow(df)
    
    # list all images recursively
    image_files <- list.files(img_path,
                              "*.JPG",
                              recursive = TRUE,
                              full.names = TRUE)
    
    # create image location string
    df$filename_orig <- df$imagepath
    df$uniqueuserid <- as.numeric(df$farmerid)
    df$uniquecropsiteid <- df$plotid
    df$timecreated <- df$datetime
    
  } else {
    
    # remove cropcutting data
    df <- df[-which(df$image == "FromCropCutting"),]
    
    # list all images recursively
    image_files <- list.files(img_path,
                              "*.jpg",
                              recursive = TRUE,
                              full.names = TRUE)
    
    # set site id to 9999 (NA)
    df$siteid <- 9999
    
    # create image location string
    df$filename_orig <- paste(df$uniqueuserid,
                              "_",
                              df$reportid,
                              ".jpg",
                              sep = "")
    
  }
  
  # create data frame to merge with meta-data
  img_list <- data.frame(image_files,
                         filename_orig = basename(image_files),
                         stringsAsFactors = FALSE)
  
  # merge file locations with meta-data
  df <- merge(df,
              img_list,
              by = "filename_orig")
  
  # split out date and time in a nice string
  date <- gsub("-","_",df$date)
  time <- format(df$timecreated,"%H%M%S")
  
  # first images do not have a time stamp?
  # set default
  time[is.na(time)] <- 120000
  
  # format filename
  df$filename_final <- paste(
    sprintf("%04d",as.numeric(output_prefix)),
    "_",
    sprintf("%06d",as.numeric(df$uniqueuserid)),
    "_",
    sprintf("%04d",as.numeric(df$uniquecropsiteid)),
    "_",
    sprintf("%04d",as.numeric(df$siteid)),
    "_",
    sprintf("%06d",as.numeric(df$reportid)),
    "_",
    season_id,
    "_",
    date,
    "_",
    time,
    ".png",
    sep = "")
  
  # remove values to far West (test values from developer)
  # by setting longitude to NA (see next step)
  df$latitude[which(df$longitude < 50)] <- NA
  df$longitude[which(df$longitude < 50)] <- NA
  
  # regularize field locations
  # using the median coordinates (most common location for a site)
  df <- df %>%
    dplyr::group_by(uniqueuserid, uniquecropsiteid) %>%
    dplyr::mutate(lat = median(latitude, na.rm = TRUE),
                    lon = median(longitude, na.rm = TRUE))
  
  # remove data which don't meet the minimum spec
  # basically those lacking date time / coordinates
  # all fields of this dataset should be complete
  df <- df[!is.na(df$lon),]
  
  # create anonymized images data folders
  lapply(unique(df$uniqueuserid),
         function(folder){
           dir.create(file.path(output_path,
                                output_prefix,
                                "images",
                                folder),
                      recursive = TRUE,
                      showWarnings = FALSE)
         })
  
  # parallel processing of the images
  cl <- parallel::makeCluster(ncores, type = "FORK")
  values <- parallel::parApply(cl, df, 1, function(x){

    filename <- file.path(
      output_path,
      output_prefix,
      "images",
      as.numeric(x['uniqueuserid']),
      x['filename_final'])

    # read and (re)size the image
    img <- try(read_size(x['image_files']))

    if(inherits(img, "try-error")){
      return(data.frame(
        filename = filename,
        r_dn = NA,
        g_dn = NA,
        b_dn = NA,
        rcc_90 = NA,
        gcc_90 = NA,
        grvi_10 = NA))
    }

    # estimate an ROI (based upon the location of the horizon)
    roi <- estimate_roi(img = img)

    # calculate the gcc
    greenness_values <- try(pbi_colour_metrics(
      img = img,
      roi = roi))

    if(inherits(greenness_values, "try-error")){
      return(data.frame(
        filename = filename,
        r_dn = NA,
        g_dn = NA,
        b_dn = NA,
        rcc_90 = NA,
        gcc_90 = NA,
        grvi_10 = NA))
    }
    
    # maks image data / anonymize pictures
    if(!file.exists(filename)){
      img <- raster::mask(img, roi$roi, updatevalue = 0)
      png(filename,
          ncol(img),
          nrow(img),
          bg = "black")
      raster::plotRGB(img)
      dev.off()
    }

    # garbage collection cleanup
    raster::removeTmpFiles()

    # return values
    return(data.frame(
      filename = filename,
      r_dn = greenness_values$r_dn,
      g_dn = greenness_values$g_dn,
      b_dn = greenness_values$b_dn,
      rcc_90 = greenness_values$rcc,
      gcc_90 = greenness_values$gcc,
      grvi_10 = greenness_values$grvi))
  })

  # stop cluster nodes
  suppressWarnings(parallel::stopCluster(cl))

  # row bind values
  values <- do.call("rbind", values)

  # give some feedback
  print(head(values))
  
  if(grepl("2018", database)){
    # update data frame
    df <- data.frame(
      project_id = output_prefix,
      farmer = df$uniqueuserid,
      field = df$uniquecropsiteid,
      site = df$siteid,
      season_id = season_id,
      report_id = df$picid,
      lat = df$lat,
      lon = df$lon,
      date = df$date,
      time = df$time,
      image = df$filename_final,
      values
    )
  } else {
    # update data frame
    df <- data.frame(
      project_id = output_prefix,
      farmer = df$uniqueuserid,
      field = df$uniquecropsiteid,
      site = df$siteid,
      season_id = season_id,
      report_id = df$reportid,
      lat = df$lat,
      lon = df$lon,
      date = df$date,
      time = df$time,
      image = df$filename_final,
      values
    )
  }
  
  # write data to file as a CSV
  utils::write.table(df,
    file.path(output_path,
              output_prefix,
              paste0("pbi_",sprintf("%04d", output_prefix),"_gcc_data.csv")),
    sep = ",",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE)
}

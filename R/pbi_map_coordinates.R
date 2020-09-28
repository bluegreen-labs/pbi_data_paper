#' Maps locations to town centroids
#' 
#' For a particular country maps locations of fields to the
#' centroid of towns. This anonymizes the data by obfuscating
#' the original location.
#'
#' @param df data frame containing lat lon locations
#' @param country country to use in mapping locations to town centroids
#'
#' @return dataframe with lat lon values mapped to centroids
#' @export
#' 
pbi_map_gadm <- function(
  df,
  country
){
  # check for data frame
  if(missing(df)){
    stop("no data frame provided")
  }
  
  # checking country
  if(missing(country)){
    stop("please provide a GADM country abbreviation, e.g. IND for India")
  }
  
  # check for coordinates
  if(!any(grepl("lon",names(df)))){
    stop("missing longitude")
  }
  
  # check for coordinates
  if(!any(grepl("lat",names(df)))){
    stop("missing latitude")
  }
  
  # download GADM data
  gadm <- raster::getData('GADM',
                          country = country,
                          level=3,
                          path = tempdir())
  
  # convert coordinates to sp objects
  lat_lon <- raster::projection(gadm)
  location <- sp::SpatialPoints(cbind(df$lon,df$lat),
                                sp::CRS(lat_lon))
  
  # list centroid and cell values
  centroids <- data.frame(sp::coordinates(gadm),
                          cell_values = gadm@data$NAME_3)
  
  # find overlap between points and polygons
  attributes <- sp::over(location, gadm)
  attributes <- attributes[grepl("^NAME_*",names(attributes))]
  names(attributes) <- c("country","state","county","town")
  
  # merge with data frame and drop points without an overlap
  df <- data.frame(df,
                   spatial_location = attributes$town,
                   spatial_unit = "gadm",
                   stringsAsFactors = FALSE)
  df <- df[!is.na(df$spatial_location),]
  
  # map centroid coordinates to lat / lon
  # anonymization process
  for(i in 1:nrow(df)){
    df$lat[i] <- 
      centroids[centroids['cell_values'] == df[i,'spatial_location'], 2][1]
    df$lon[i] <- 
      centroids[centroids['cell_values'] == df[i,'spatial_location'], 1][1]
  }
  
  return(df)
}

#' Maps locations to arbitrary shape file centroids
#'
#' @param df data frame containing lat lon locations 
#' @param poly SpatialPolygon dataframe from which to take coordinates
#' @param project_id unique project id (numerical value)
#' 
#' @return dataframe with lat lon values mapped to centroids
#' @export
#' 
pbi_map_shp <- function(
  df,
  poly,
  project_id
){
  # check for data frame
  if(missing(df)){
    stop("no data frame provided")
  }
  
  # check for data frame
  if(missing(poly)){
    stop("missing sp polygon data")
  }
  
  # check for coordinates
  if(!any(grepl("lon",names(df)))){
    stop("missing longitude")
  }
  
  # check for coordinates
  if(!any(grepl("lat",names(df)))){
    stop("missing latitude")
  }
  
  # convert coordinates to sp objects
  lat_lon <- raster::projection(poly)
  location <- sp::SpatialPoints(cbind(df$lon,df$lat),
                                sp::CRS(lat_lon))
  
  # list centroid and cell values
  centroids <- data.frame(sp::coordinates(poly),
                          cell_values = gadm@data$NAME_3)
  
  # find overlap between points and polygons
  attributes <- sp::over(location, gadm)
  attributes <- attributes[grepl("^NAME_*",names(attributes))]
  names(attributes) <- c("country","state","county","town")
  
  # merge with data frame and drop points without an overlap
  df <- na.omit(data.frame(df, attributes))
  
  # map centroid coordinates to lat / lon
  # anonymization process
  for(i in 1:nrow(df)){
    df$lat[i] <- 
      centroids[centroids['cell_values'] == df[i,'town'], 2][1]
    df$lon[i] <- 
      centroids[centroids['cell_values'] == df[i,'town'], 1][1]
  }
  
  return(df)
}

#' Maps coordinates to grid centroids
#' 
#' Anonymizes lat/lon coordinates by remapping them
#' to low resolution grid cell centroids
#'
#' @param df data frame containing lat lon locations 
#' @param resolution resolution of the map in minutes (2.5, 5, 10)
#'
#' @return dataframe with lat lon values mapped to centroids
#' @export
#' 
pbi_map_grid <- function(
  df,
  resolution = 2.5
){
  # check for data frame
  if(missing(df)){
    stop("no data frame provided")
  }
  
  # check for coordinates
  if(!any(grepl("lon",names(df)))){
    stop("missing longitude")
  }
  
  # check for coordinates
  if(!any(grepl("lat",names(df)))){
    stop("missing latitude")
  }
  
  # download WorldClim data
  wc <- suppressMessages(raster::getData('worldclim',
                          var = 'prec',
                          res = resolution,
                          path = tempdir())$prec1)
  
  # convert coordinates to sp objects
  lat_lon <- raster::projection(wc)
  
  # create sp object for field locations
  location <- sp::SpatialPoints(
    cbind(df$lon,df$lat),
    sp::CRS(lat_lon))
  
  cells <- 1:raster::ncell(wc$prec1)
  
  # fill with numeric values (row wise)
  wc[] <- cells
  
  # crop and convert to polygon
  wc <- raster::crop(wc,
               sp::bbox(location),
               snap = 'out')
  wc <- raster::rasterToPolygons(wc)
  
  # list centroid and cell values
  centroids <- data.frame(sp::coordinates(wc),
                          cell_values = wc@data$prec1)
  
  # find overlap between points and polygons
  attributes <- as.numeric(unlist(sp::over(location, wc)))
  
  # merge with data frame and drop points without an overlap
  df <- data.frame(df,
                   spatial_location = attributes,
                   spatial_unit = paste0("worldclim_",resolution),
                   stringsAsFactors = FALSE)
  df <- df[!is.na(df$spatial_location),]
  
  # map centroid coordinates to lat / lon
  # anonymization process
  for(i in 1:nrow(df)){
     df$lat[i] <- 
       centroids[centroids['cell_values'] == df[i,'spatial_location'], 2][1]
     df$lon[i] <- 
       centroids[centroids['cell_values'] == df[i,'spatial_location'], 1][1]
   }
  
  return(df)
}

#' Maps locations to town centroids
#' 
#' For a particular country maps locations of fields to the
#' centroid of towns. This anonymizes the data by obfuscating
#' the original location.
#'
#' @param df data frame containing lat lon locations
#' @param rds rds file which includes an sf object with the village
#' parameters
#'
#' @return dataframe with lat lon values mapped to centroids
#' @export
#' 
pbi_map_sedac <- function(
  df,
  rds
){
  
  # check for data frame
  if(missing(df)){
    stop("no data frame provided")
  }
  
  # checking country
  if(missing(rds)){
    stop("please provide the sedac")
  }
  
  # check for coordinates
  if(!any(grepl("lon",names(df)))){
    stop("missing longitude")
  }
  
  # check for coordinates
  if(!any(grepl("lat",names(df)))){
    stop("missing latitude")
  }
  
  # read polygons
  message("reading polygons")
  sedac <- readRDS(rds)
  
  # calculate centroids
  message("calculating centroids")
  sedac_centroids <- sedac %>%
    st_centroid() %>%
    st_transform(crs = "+init=epsg:4326") %>%
    dplyr::select(UID)
  
  # convert to lat long
  message("convert polygons to lat long")
  sedac_ll <- sedac %>%
    st_transform(crs = "+init=epsg:4326")
  
  # convert to SF object
  df <- df %>%
    st_as_sf(coords = c("lon", "lat"), crs = "+init=epsg:4326")
  
  # grab intersection with polygons of SEDAC data
  # only retain the unique identifier
  df <- as.data.frame(st_intersection(df, sedac_ll)) %>%
    dplyr::select(-c("geometry", "SID", "DID","TID","VILL_CODE","NAME"))
  
  # join with the centroid data using the UID
  # rename the UID to spatial location to link back
  # to the SEDAC database if desired
  df_cent <- left_join(df, sedac_centroids,
                       by = "UID") %>%
    rename(spatial_location = "UID")
  
  # add identifier for the dataset
  df_cent$spatial_unit = "sedac"
  
  # convert the geometry to a two column lat lon setup
  coords <- do.call(rbind, st_geometry(df_cent$geometry)) %>%
    as_tibble() %>% setNames(c("lon","lat"))
  
  # bind with the original file and drop the geometry
  # column
  df_cent <- cbind(df_cent, coords)
  df_cent <- df_cent %>% dplyr::select(-geometry)
  
  # return the data adding 4 columns
  # lat / lon / spatial_location / spatial_unit
  # but overwriting the orignal lat / lon for
  # a total of 2 added
  return(df_cent)
}

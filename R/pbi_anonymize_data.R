#' Anonymize a raw data frame
#'
#' Anonymize raw data frame with colour indices
#' and other field / farmer specific data and
#' overwrite the original image coordinates with
#' those of an aggregated unit, either a WorldClim
#' grid cell or GADM administrative town boundaries.
#'
#' @param df a dataframe or filename of a data file
#' @param method the method to use (gadm or grid)
#' @param country country to use in the gadm method
#' @param resolution resolution to use in the grid method (2.5, 5 or 10)
#' @param output_file where to store the results
#' @param rds rds file with sf polygons of villages
#' @return dataframe with lat lon values mapped to centroids
#' @export

pbi_anonymize_data <- function(
  df,
  method = "sedac",
  country = "IND",
  resolution = 2.5,
  rds = "./data-raw/sedac_indian_villages.rds",
  output_file
){
  if(!is.data.frame(df)){
    if(file.exists(df)){
      df <- read.table(df,
                       header = TRUE,
                       sep = ",",
                       stringsAsFactors = FALSE)
    } else {
      stop("data frame or file does not exist")
    }
  }
  
  # administrative boundaries
  if(method == "sedac"){
    df <- pbi_map_sedac(df = df,
                        rds = rds)
  }

  # worldview grid
  if(method == "grid"){
    df <- pbi_map_grid(df = df,
                       resolution = resolution)
  }
  
  if(missing(output_file)){
   # return anonymized data
   return(df)
  } else{
    utils::write.table(df,
                output_file,
                quote = FALSE,
                row.names = FALSE,
                col.names = TRUE,
                sep = ',')
  }
}
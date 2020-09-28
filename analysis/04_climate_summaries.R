# get climate summaries
library(raster)

# read in master data file
df <- read.table("data/pbi_master_image_stats.csv",
                 header = TRUE,
                 sep = ",",
                 stringsAsFactors = FALSE)

loc <- df %>%
  filter(spatial_unit == "worldclim_2.5") %>%
  group_by(field) %>%
  summarize(lat = mean(lat),
            lon = mean(lon))

points <- sp::SpatialPoints(
  cbind(loc$lon,
        loc$lat),
  sp::CRS("+init=epsg:4326"))

# get worldclim 2.5 locations
# extract the climate data for
# these locations
bio <- getData('worldclim', var='bio', res=2.5)
tmean  <- bio$bio1 / 10
prec  <- bio$bio12

# sample locations
tmean_points <- extract(tmean, points)
prec_points <- extract(prec, points)

# temperature
message("temperature (C): ")
message(
  paste(
    round(mean(tmean_points),2),
    round(sd(tmean_points),2),
    sep = " "
  )
)

# precip
message("precip (mm): ")

message("site mean + sd")
message(
  paste(
    round(mean(prec_points)),
    round(sd(prec_points)),
    sep = " "
  )
)

message("site min max")
message(
  paste(
    round(min(prec_points)),
    round(max(prec_points)),
    sep = " "
  )
)